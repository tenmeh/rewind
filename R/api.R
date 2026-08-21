#' Enable undo and redo for a Shiny session
#'
#' Call this once, near the top of your `server` function. After that,
#' `rewind` records the session inputs as the user works. It also records
#' the reactive values that you register with [rewind_track()]. The user can
#' then move backwards and forwards through that history.
#'
#' # What gets captured
#'
#' By default `rewind` captures every input in the session. There are four
#' exclusions. It is never useful to restore these:
#'
#' * action buttons and links. Their value is a click counter.
#' * [shiny::fileInput()]. Its value points to a temporary file on the
#'   server. Shiny deletes that file at the next upload. An old snapshot
#'   would thus point to a file that does not exist.
#' * inputs with names that start with `rewind_`. These belong to this
#'   package.
#' * inputs with names that start with `.`. These are internal to Shiny.
#'
#' Use `inputs` to give a list of the inputs to capture. This is usually
#' better in a large application. Undo must move the controls that the user
#' thinks of as filters. It must not move every other input on the page.
#'
#' # Grouping
#'
#' Changes that occur within `coalesce_ms` of each other become one history
#' entry. One drag of a slider is thus one undo step, not forty. Use
#' [rewind_step()] to group changes yourself.
#'
#' # Modules
#'
#' You can call this function inside a `moduleServer()`. `rewind` captures
#' the inputs with their module-local names. These are the same names that
#' `input$` uses inside the module. `rewind` adds the namespace with
#' `session$ns()` when it restores them.
#'
#' All modules share `session$userData`. A second call to `rewind_enable()`
#' thus uses the same history as the first call. It does not make a second
#' history. Call the function once, at the position in the module tree that
#' is best for your application.
#'
#' @param session The Shiny session. The default is the current session.
#' @param inputs Character vector of the input IDs to capture. Use `NULL`
#'   (the default) to capture all permitted inputs.
#' @param exclude Character vector of input IDs to skip. `rewind` applies
#'   this after `inputs`.
#' @param depth The maximum number of history entries to keep. `rewind`
#'   removes the oldest entries first.
#' @param coalesce_ms The quiet period in milliseconds before `rewind`
#'   writes a change to the history. Increase it to group more changes.
#' @param shortcuts Set to `TRUE` to bind the keyboard shortcuts in the
#'   browser. There are three:
#'
#'   * `Ctrl` + `Z` (`Cmd` + `Z` on macOS) does an undo;
#'   * `Ctrl` + `Shift` + `Z` (`Cmd` + `Shift` + `Z` on macOS) does a redo;
#'   * `Ctrl` + `Y` also does a redo. This is the usual redo shortcut on
#'     Windows.
#'
#'   The shortcuts do nothing while the user types in a text field. The
#'   text undo of the browser thus continues to work.
#' @param verbose Set to `TRUE` to show messages about what `rewind`
#'   captures and restores. This is useful during development.
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

  # Add the assets here. The user thus does not have to change the UI.
  shiny::insertUI(
    selector  = "head",
    where     = "beforeEnd",
    ui        = htmltools::attachDependencies(
      htmltools::tags$script(
        type = "application/json",
        `data-rewind-config` = "",
        # Use HTML() because the body of a script tag is raw text. Without
        # it, htmltools escapes the quotes. The browser cannot decode them.
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

  # Capture. This observer reads every tracked reactive. It thus runs again
  # after each change.
  #
  # Take the snapshot in its own statement. Do not write it as
  # ctrl$note(ctrl$snapshot()).
  #
  # note() returns at its first line while capture is paused. R evaluates
  # arguments only when it needs them. An inline snapshot() thus never runs
  # while capture is paused. The observer then reads no reactive values, and
  # it loses all its dependencies. It never runs again, even after
  # rewind_resume(). One pause before the first flush is enough to cause
  # this.
  #
  # The separate statement forces the reads. The dependencies thus stay
  # alive across a pause and a resume.
  obs_capture <- shiny::observe({
    state <- ctrl$snapshot()
    ctrl$note(state)
  }, domain = session)

  # Coalesce. This observer waits for the changes to stop. It then writes one
  # entry.
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
#' `rewind` captures inputs automatically. It cannot see the state that you
#' keep in a [shiny::reactiveValues()] object. Register that object here.
#' `rewind` then records the registered fields with the inputs, and writes
#' them back at an undo.
#'
#' Register only the values that are true *state*. A derived value or a
#' cached value does no harm, but it has no use. Do not register a value
#' that an observer computes again immediately. The undo step then appears
#' to do nothing.
#'
#' @param values A [shiny::reactiveValues()] object.
#' @param fields Character vector of the field names to track. Use `NULL`
#'   for all the fields that exist when `rewind` takes the snapshot.
#' @param id A name for this group. `rewind` uses it to keep the registered
#'   objects apart, and to label the history entries. The default is the
#'   name of the `values` argument at the call.
#' @param session The Shiny session. The default is the current session.
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
#' This function holds a block of code. Every change in that block becomes
#' one history entry with the label that you give. Use it for buttons such
#' as "reset all filters" or "apply preset". For these buttons, the standard
#' time-based grouping can make several steps, or it can give a label that
#' does not help the user.
#'
#' The block runs immediately. `rewind` writes the history entry after the
#' changes return from the browser.
#'
#' @param expr The code to run. This is usually a set of `update*Input()`
#'   calls, or assignments to tracked reactive values.
#' @param label The label for the history entry.
#' @param hold_ms The time in milliseconds to keep the entry open. The
#'   default is two times the `coalesce_ms` of the session. Increase it if a
#'   slow browser makes two steps from one block.
#' @param session The Shiny session. The default is the current session.
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
#' The keyboard shortcuts and [rewind_buttons()] are sufficient for most
#' applications. Use these functions to move through the history from your
#' own controls.
#'
#' @param index The position to move to. The first position is 1. Position 1
#'   holds the oldest entry that `rewind` keeps.
#' @param session The Shiny session. The default is the current session.
#'
#' @return `TRUE` if the position changed. If not, `FALSE`. The functions
#'   return the value invisibly.
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
#' `rewind_history()` gives one row for each entry that `rewind` keeps.
#' `rewind_can_undo()` and `rewind_can_redo()` tell you if the position can
#' change. All three functions depend on the history. You can thus use them
#' in `render*()` and `observe()` to control your own UI.
#'
#' @param session The Shiny session. The default is the current session.
#'
#' @return
#' `rewind_history()` gives a data frame. It has the columns `index`,
#' `label`, `time` and `current`. The other two functions give one logical
#' value.
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
#' While capture is paused, the application applies changes as usual. But
#' `rewind` does not record them. Use this around changes that your code
#' makes, and that the user must not step back into. Examples are a saved
#' session that you restore, or a URL bookmark that you apply.
#'
#' @param session The Shiny session. The default is the current session.
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
#' [rewind_pause()] stops capture for a short time. It needs a
#' [rewind_resume()] call after it. This function is different. It removes
#' everything that [rewind_enable()] made:
#'
#' * it destroys each observer;
#' * it sets the buttons and the history rail in the browser to their empty,
#'   disabled condition;
#' * it returns the session to the condition before `rewind_enable()`.
#'
#' Call [rewind_enable()] again to start a new history.
#'
#' Use this function when the application permits undo and redo only in some
#' conditions. An example is a user role that the application does not know
#' at the start of the session.
#'
#' @param session The Shiny session. The default is the current session.
#'
#' @return `TRUE` if the function disabled a session. `FALSE` if `rewind`
#'   was not enabled. The function returns the value invisibly.
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
