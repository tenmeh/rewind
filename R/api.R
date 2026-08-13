#' Enable undo and redo for a Shiny session
#'
#' Call once, near the top of your `server` function. From that point on,
#' `rewind` snapshots the session's inputs (and any reactive values registered
#' with [rewind_track()]) as the user interacts with the application, and lets
#' them step back and forth through that history.
#'
#' # What gets captured
#'
#' By default every input in the session is captured, with four exclusions
#' that are never useful to restore:
#'
#' * action buttons and links, whose value is a click counter;
#' * [shiny::fileInput()], whose value points at a server-side temp file that
#'   Shiny deletes on the next upload, so restoring an old snapshot would
#'   point at a path that no longer exists;
#' * inputs whose names begin with `rewind_`, which belong to this package;
#' * inputs whose names begin with `.`, which are internal to Shiny.
#'
#' Pass `inputs` to capture an explicit allow-list instead, which is usually
#' what you want in a large application: undoing should move the controls the
#' user thinks of as filters, not every stray input on the page.
#'
#' # Grouping
#'
#' Changes that arrive within `coalesce_ms` of one another become a single
#' history entry, so dragging a slider produces one undo step rather than
#' forty. Use [rewind_step()] when you need to group changes explicitly.
#'
#' # Modules
#'
#' Calling this inside a `moduleServer()` works: inputs are captured under
#' their module-local names (the same names `input$` sees inside that
#' module) and re-qualified with `session$ns()` on restore. `session$userData`
#' is shared across modules, so calling `rewind_enable()` more than once
#' reuses the same session-wide history rather than creating an independent
#' one per module - call it once, wherever in the module tree suits the app.
#'
#' @param session The Shiny session. Defaults to the current one.
#' @param inputs Character vector of input IDs to capture, or `NULL` (the
#'   default) to capture all eligible inputs.
#' @param exclude Character vector of input IDs to skip. Applied after
#'   `inputs`.
#' @param depth Maximum number of history entries to retain. Older entries are
#'   dropped from the far end.
#' @param coalesce_ms Quiet period, in milliseconds, before a change is
#'   committed to history. Raise it to group more aggressively.
#' @param shortcuts Whether to bind `Ctrl`/`Cmd` + `Z` and
#'   `Ctrl`/`Cmd` + `Shift` + `Z` (and `Ctrl` + `Y`) in the browser. Shortcuts
#'   are ignored while the user is typing in a text field, so native text undo
#'   keeps working.
#' @param verbose Emit messages describing what is being captured and
#'   restored. Useful while developing.
#'
#' @return The controller, invisibly. Most applications can ignore it.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     rewind_buttons(),
#'     selectInput("species", "Species", c("setosa", "versicolor", "virginica")),
#'     sliderInput("n", "Rows", 1, 50, 10),
#'     tableOutput("tbl")
#'   )
#'
#'   server <- function(input, output, session) {
#'     rewind_enable()
#'
#'     output$tbl <- renderTable({
#'       head(iris[iris$Species == input$species, ], input$n)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @export
rewind_enable <- function(session = shiny::getDefaultReactiveDomain(),
                          inputs = NULL,
                          exclude = NULL,
                          depth = 50L,
                          coalesce_ms = 400L,
                          shortcuts = TRUE,
                          verbose = FALSE) {
  session <- require_session(session)

  if (!is.null(inputs) && !is.character(inputs)) {
    stop("`inputs` must be a character vector or NULL.", call. = FALSE)
  }
  if (!is.null(exclude) && !is.character(exclude)) {
    stop("`exclude` must be a character vector or NULL.", call. = FALSE)
  }
  if (!is.numeric(coalesce_ms) || length(coalesce_ms) != 1L || coalesce_ms < 0) {
    stop("`coalesce_ms` must be a single non-negative number.", call. = FALSE)
  }

  if (!is.null(session$userData$.rewind)) {
    warning("rewind is already enabled for this session; ignoring.",
            call. = FALSE)
    return(invisible(session$userData$.rewind))
  }

  ctrl <- RewindController$new(
    session     = session,
    inputs      = inputs,
    exclude     = exclude,
    depth       = depth,
    coalesce_ms = coalesce_ms,
    verbose     = verbose
  )
  session$userData$.rewind <- ctrl

  # Make the assets available without requiring the user to touch their UI.
  shiny::insertUI(
    selector  = "head",
    where     = "beforeEnd",
    ui        = htmltools::attachDependencies(
      htmltools::tags$script(
        type = "application/json",
        `data-rewind-config` = "",
        # HTML() because script bodies are raw text: htmltools would otherwise
        # escape the quotes and the browser would not decode them back.
        htmltools::HTML(sprintf(
          '{"shortcuts": %s}',
          if (isTRUE(shortcuts)) "true" else "false"
        ))
      ),
      rewind_dependency()
    ),
    immediate = TRUE,
    session   = session
  )

  # Capture: reads every tracked reactive, so it re-runs on any change.
  #
  # The snapshot is taken as its own statement, not inlined as
  # ctrl$note(ctrl$snapshot()). note()'s first line returns early while
  # paused, and R's lazy argument evaluation means an inlined snapshot()
  # would then never actually run - so the observer would never read the
  # tracked reactives it depends on, and would permanently stop re-running on
  # future input changes (pausing before the very first flush is enough to
  # trigger this). Reading the snapshot first forces those reads
  # unconditionally, keeping the dependency alive across pause/resume.
  obs_capture <- shiny::observe({
    state <- ctrl$snapshot()
    ctrl$note(state)
  }, domain = session)

  # Coalesce: waits for the dust to settle, then commits one entry.
  obs_coalesce <- shiny::observe({
    ctrl$tick_dep()
    wait <- ctrl$time_to_flush()
    if (!is.finite(wait)) return(NULL)
    if (wait > 0) {
      shiny::invalidateLater(ceiling(wait), session)
    } else {
      ctrl$flush()
    }
  }, domain = session)

  obs_undo <- shiny::observeEvent(session$input$rewind_undo, ctrl$undo(),
                                  ignoreInit = TRUE, domain = session)
  obs_redo <- shiny::observeEvent(session$input$rewind_redo, ctrl$redo(),
                                  ignoreInit = TRUE, domain = session)
  obs_jump <- shiny::observeEvent(session$input$rewind_jump, ctrl$jump(session$input$rewind_jump$index),
                                  ignoreInit = TRUE, domain = session)

  ctrl$set_observers(list(obs_capture, obs_coalesce, obs_undo, obs_redo, obs_jump))

  invisible(ctrl)
}


#' Include server-side reactive values in the history
#'
#' Inputs are captured automatically, but state you hold yourself in a
#' [shiny::reactiveValues()] object is invisible to `rewind` until you register
#' it here. Registered fields are snapshotted alongside inputs and assigned
#' back on undo.
#'
#' Only register values that are genuinely *state*. Registering a derived or
#' cached value is harmless but pointless, and registering something an
#' observer immediately recomputes will produce an undo step that appears to do
#' nothing.
#'
#' @param values A [shiny::reactiveValues()] object.
#' @param fields Character vector of field names to track, or `NULL` for all
#'   fields present at snapshot time.
#' @param id A name for this group, used to keep multiple registered objects
#'   apart and to label history entries. Defaults to the name of the `values`
#'   argument at the call site.
#' @param session The Shiny session. Defaults to the current one.
#'
#' @return `TRUE`, invisibly.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   server <- function(input, output, session) {
#'     rewind_enable()
#'     state <- reactiveValues(selected = character(), zoom = 1)
#'     rewind_track(state, fields = c("selected", "zoom"))
#'   }
#' }
#' @export
rewind_track <- function(values,
                         fields = NULL,
                         id = NULL,
                         session = shiny::getDefaultReactiveDomain()) {
  session <- require_session(session)

  if (!shiny::is.reactivevalues(values)) {
    stop("`values` must be a reactiveValues object.", call. = FALSE)
  }
  if (!is.null(fields) && !is.character(fields)) {
    stop("`fields` must be a character vector or NULL.", call. = FALSE)
  }

  if (is.null(id)) {
    id <- deparse(substitute(values))
    if (length(id) != 1L || !grepl("^[[:alnum:]._]+$", id)) id <- "values"
  }

  ctrl <- require_controller(session)
  ctrl$track(id = id, values = values, fields = fields)
  invisible(TRUE)
}


#' Group several changes into one undo step
#'
#' Wraps a block of code so that everything it changes becomes a single history
#' entry with a label of your choosing. Use it for the "reset all filters" or
#' "apply preset" buttons, where the default time-based coalescing would either
#' split the change into several steps or label it unhelpfully.
#'
#' The block itself runs immediately; the resulting history entry is written
#' once the changes have made their round trip through the browser.
#'
#' @param expr Code to run. Typically a series of `update*Input()` calls or
#'   assignments to tracked reactive values.
#' @param label Label for the resulting history entry.
#' @param hold_ms How long to keep the entry open, in milliseconds. Defaults to
#'   twice the session's `coalesce_ms`. Raise it if a slow round trip is
#'   splitting your step in two.
#' @param session The Shiny session. Defaults to the current one.
#'
#' @return The value of `expr`, invisibly.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   server <- function(input, output, session) {
#'     rewind_enable()
#'
#'     observeEvent(input$reset, {
#'       rewind_step(label = "Reset filters", {
#'         updateSelectInput(session, "region", selected = "All")
#'         updateSliderInput(session, "year", value = c(2000, 2026))
#'         updateCheckboxInput(session, "only_active", value = FALSE)
#'       })
#'     })
#'   }
#' }
#' @export
rewind_step <- function(expr,
                        label = NULL,
                        hold_ms = NULL,
                        session = shiny::getDefaultReactiveDomain()) {
  ctrl <- get_controller(session)
  if (!is.null(ctrl)) ctrl$open_step(label = label, hold_ms = hold_ms)
  invisible(expr)
}


#' Move through the history programmatically
#'
#' The keyboard shortcuts and [rewind_buttons()] cover the common cases; these
#' let you drive the history from your own controls.
#'
#' @param index Position to jump to, 1-based, where 1 is the oldest retained
#'   entry.
#' @param session The Shiny session. Defaults to the current one.
#'
#' @return `TRUE` if the pointer moved, `FALSE` otherwise, invisibly.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   server <- function(input, output, session) {
#'     rewind_enable()
#'     observeEvent(input$my_back_button, rewind_undo())
#'   }
#' }
#' @export
rewind_undo <- function(session = shiny::getDefaultReactiveDomain()) {
  ctrl <- require_controller(session)
  ctrl$undo()
}

#' @rdname rewind_undo
#' @export
rewind_redo <- function(session = shiny::getDefaultReactiveDomain()) {
  ctrl <- require_controller(session)
  ctrl$redo()
}

#' @rdname rewind_undo
#' @export
rewind_jump <- function(index, session = shiny::getDefaultReactiveDomain()) {
  ctrl <- require_controller(session)
  ctrl$jump(index)
}

#' @rdname rewind_undo
#' @export
rewind_clear <- function(session = shiny::getDefaultReactiveDomain()) {
  ctrl <- require_controller(session)
  ctrl$clear()
}


#' Inspect the history
#'
#' `rewind_history()` returns one row per retained entry. `rewind_can_undo()`
#' and `rewind_can_redo()` report whether the pointer can move. All three take
#' a reactive dependency on the history, so they can be used inside
#' `render*()` and `observe()` to drive your own UI.
#'
#' @param session The Shiny session. Defaults to the current one.
#'
#' @return
#' For `rewind_history()`, a data frame with columns `index`, `label`, `time`
#' and `current`. For the others, a single logical.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   server <- function(input, output, session) {
#'     rewind_enable()
#'     output$steps <- renderTable(rewind_history())
#'   }
#' }
#' @export
rewind_history <- function(session = shiny::getDefaultReactiveDomain()) {
  ctrl <- require_controller(session)
  ctrl$version_dep()
  ctrl$history$entries()
}

#' @rdname rewind_history
#' @export
rewind_can_undo <- function(session = shiny::getDefaultReactiveDomain()) {
  ctrl <- require_controller(session)
  ctrl$version_dep()
  ctrl$history$can_undo()
}

#' @rdname rewind_history
#' @export
rewind_can_redo <- function(session = shiny::getDefaultReactiveDomain()) {
  ctrl <- require_controller(session)
  ctrl$version_dep()
  ctrl$history$can_redo()
}


#' Suspend and resume history capture
#'
#' While paused, changes are applied normally but not recorded. Useful around
#' programmatic churn you do not want the user to be able to step back into,
#' such as restoring a saved session or applying a URL bookmark.
#'
#' @param session The Shiny session. Defaults to the current one.
#'
#' @return `TRUE`, invisibly.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   server <- function(input, output, session) {
#'     rewind_enable()
#'     rewind_pause()
#'     # ... apply a saved bookmark here ...
#'     rewind_resume()
#'   }
#' }
#' @export
rewind_pause <- function(session = shiny::getDefaultReactiveDomain()) {
  require_controller(session)$pause()
}

#' @rdname rewind_pause
#' @export
rewind_resume <- function(session = shiny::getDefaultReactiveDomain()) {
  require_controller(session)$resume()
}


#' Fully disable undo/redo for a session
#'
#' Unlike [rewind_pause()], which suspends capture temporarily and expects
#' a matching [rewind_resume()], this tears down what [rewind_enable()] set
#' up entirely: every observer it created is destroyed, the buttons and
#' history rail in the browser go back to their empty, disabled state, and
#' the session goes back to not having rewind enabled at all. Call
#' [rewind_enable()] again to start a fresh history.
#'
#' Useful when undo/redo should be available only conditionally - for
#' example, gated behind a user role that is not known until partway through
#' the session.
#'
#' @param session The Shiny session. Defaults to the current one.
#'
#' @return `TRUE` if a session was disabled, `FALSE` if rewind was not
#'   enabled to begin with, invisibly.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   server <- function(input, output, session) {
#'     rewind_enable()
#'     observeEvent(input$readonly_mode, {
#'       if (input$readonly_mode) rewind_disable()
#'     })
#'   }
#' }
#' @export
rewind_disable <- function(session = shiny::getDefaultReactiveDomain()) {
  session <- require_session(session)
  ctrl <- get_controller(session)
  if (is.null(ctrl)) return(invisible(FALSE))

  ctrl$destroy()
  session$userData$.rewind <- NULL
  invisible(TRUE)
}
