#' Enable undo and redo for a Shiny session
#'
#' Call this once, near the top of your `server` function. After that,
#' `rewind` records the session inputs as the user works. It also records
#' the reactive values that you register with [rewind_track()]. The user can
#' then move backwards and forwards through that history.
#'
#' # What gets captured
#'
#' By default `rewind` captures every input that the page can put back: an
#' input whose element has a Shiny input binding. That covers the inputs of
#' Shiny, and those of other packages that give their widgets a binding.
#'
#' It leaves out inputs that have no such element. A plot's `hover`,
#' `click`, `dblclick` and `brush`, and the values that DT keeps, such as
#' `_rows_current`, `_rows_all` and `_state`, are sent to the server with
#' `Shiny.setInputValue()` alone. An undo cannot restore them, so recording
#' them made steps that seemed to do nothing. The browser reports which
#' inputs have a binding, including those that `renderUI()` adds later.
#' Until that report arrives, and in [shiny::testServer()], which has no
#' browser, every input is captured.
#'
#' To capture an input the page cannot restore, such as the selected rows
#' of a DT table, name it in `inputs`. A list in `inputs` is used as it is.
#' Use [rewind_status()] to see what is captured, and why an input is not.
#'
#' Five more kinds of input are never captured:
#'
#' * action buttons and links. Their value is a click counter.
#' * [shiny::fileInput()]. Its value points to a temporary file on the
#'   server. Shiny deletes that file at the next upload. An old snapshot
#'   would thus point to a file that does not exist.
#' * password fields, such as [shiny::passwordInput()]. A password must not
#'   be kept in the history, put back in a field by an undo, or shown by
#'   [rewind_diff()]. The server cannot tell a password from other text, so
#'   the browser reports which fields are passwords, including fields that
#'   `renderUI()` adds later. This exclusion applies even when `inputs`
#'   names the field. In [shiny::testServer()] there is no browser, so a
#'   password field there is captured like any other input.
#' * inputs with names that start with `rewind_`. These belong to this
#'   package.
#' * inputs with names that start with `.`. These are internal to Shiny.
#'
#' # Text inputs
#'
#' A word typed at a normal speed is one step. A pause longer than
#' `coalesce_ms` starts a new step, so slow typing can give several. For a
#' text box, `textInput(updateOn = "blur")` gives exactly one step for each
#' edit, when the user leaves the box.
#'
#' While the cursor is in a text box, `Ctrl` + `Z` is the browser's own text
#' undo, and `rewind` does nothing (refer to `shortcuts` below). The
#' browser's undo still changes the box, so `rewind` records the change as
#' a new step. To step back through the history, leave the box first, or
#' use the buttons.
#'
#' Use `inputs` to give a list of the inputs to capture, and `exclude` or
#' [rewind_exclude()] to leave some out. A list is usually better in a large
#' application. Undo must move the controls that the user
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
#' @param restore_timeout The time in seconds that `rewind` waits for the
#'   browser to finish a restore. The default of 2 suits a fast
#'   connection.
#'
#'   Increase it for an application on a slow connection, such as one
#'   behind a VPN or on a mobile network. If the limit is too short,
#'   capture starts again while the browser is still applying the restore,
#'   and a partial state becomes a history entry that the user never made.
#'
#'   Do not increase it more than you need. If a restore never returns,
#'   `rewind` ignores the changes of the user until this limit ends.
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
#' @param labels A named character vector of the names a person reads,
#'   such as `c(region = "Region", year = "Year range")`. The names are the
#'   input ids. A value that [rewind_track()] records is named `id$field`.
#'   The labels appear in the history rail, where a step reads
#'   `"Region, Year range"` and not `"region, year"`, and in the `label`
#'   column of [rewind_diff()]. Shiny does not give the server the label of
#'   a widget, so `rewind` cannot find these by itself. An input with no
#'   label keeps its id.
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
                          restore_timeout = 2,
                          shortcuts = TRUE,
                          verbose = FALSE,
                          labels = NULL) {
  session <- require_session(
    session,
    hint = "To name the inputs to capture, use `inputs = c(...)`."
  )

  # Every argument is checked here, so a wrong value fails at the call and
  # names the argument. The checks are in R/utils.R.
  check_character(inputs, "inputs")
  check_character(exclude, "exclude")
  depth <- check_count(depth, "depth", min = 2L)
  check_number(coalesce_ms, "coalesce_ms", min = 0, inclusive = TRUE)
  check_number(restore_timeout, "restore_timeout", min = 0, inclusive = FALSE)
  check_flag(shortcuts, "shortcuts")
  check_flag(verbose, "verbose")
  check_labels(labels)

  if (!is.null(session$userData$.rewind)) {
    warning("rewind is already enabled for this session; ignoring.",
            call. = FALSE)
    return(invisible(session$userData$.rewind))
  }

  ctrl <- RewindController$new(
    session         = session,
    inputs          = inputs,
    exclude         = exclude,
    depth           = depth,
    coalesce_ms     = coalesce_ms,
    restore_timeout = restore_timeout,
    verbose         = verbose,
    labels          = labels
  )
  session$userData$.rewind <- ctrl

  # rewind_exclude() can run before this, for example in a module that is
  # called first. It keeps the names in userData until now.
  if (length(session$userData$.rewind_exclude)) {
    ctrl$add_exclude(session$userData$.rewind_exclude)
    session$userData$.rewind_exclude <- NULL
  }

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

  # The buttons, the shortcuts and the rail. Read these inputs from the root
  # session, and not from `session`.
  #
  # rewind.js sends them under the global ids rewind_undo, rewind_redo and
  # rewind_jump. rewind_buttons() has no id, so the ids stay global wherever
  # the buttons are placed. Inside a module, session$input$rewind_undo reads
  # the namespaced id "mymod-rewind_undo", which nothing ever sets. Capture
  # still worked, so the rail filled up, but no undo ever reached the
  # controller. A session has only one history (refer to the Modules
  # section above), so one set of global ids is correct.
  root <- session$rootScope()

  obs_undo <- shiny::observeEvent(root$input$rewind_undo, ctrl$undo(),
                                  ignoreInit = TRUE, domain = session)
  obs_redo <- shiny::observeEvent(root$input$rewind_redo, ctrl$redo(),
                                  ignoreInit = TRUE, domain = session)
  obs_jump <- shiny::observeEvent(root$input$rewind_jump,
                                  ctrl$jump(root$input$rewind_jump$index),
                                  ignoreInit = TRUE, domain = session)

  # The password fields in the page, reported by rewind.js. Not
  # ignoreInit: the report can arrive before this observer starts, and it
  # must still be applied.
  obs_secret <- shiny::observeEvent(root$input$rewind_secret_ids,
                                    ctrl$set_secret(root$input$rewind_secret_ids),
                                    ignoreInit = FALSE, domain = session)

  # The inputs that the page can restore, reported by rewind.js. By default
  # only these are captured. Not ignoreInit, for the same reason as above.
  obs_bound <- shiny::observeEvent(root$input$rewind_bound_ids,
                                   ctrl$set_bound(root$input$rewind_bound_ids),
                                   ignoreInit = FALSE, domain = session)

  ctrl$set_observers(list(obs_capture, obs_coalesce, obs_undo, obs_redo,
                          obs_jump, obs_secret, obs_bound))

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
  check_character(fields, "fields")
  check_string(id, "id")

  # A field that `values` does not have is captured as nothing, so a typo
  # used to give no message. A field can be added later, so warn and do not
  # stop.
  if (!is.null(fields)) {
    have <- names(shiny::isolate(shiny::reactiveValuesToList(values)))
    missing <- setdiff(fields, have)
    if (length(missing)) {
      warning(sprintf(
        "`fields` names %s that `values` does not have: %s. Check the spelling, or ignore this if the field is added later.",
        if (length(missing) == 1L) "a field" else "fields",
        paste0('"', missing, '"', collapse = ", ")
      ), call. = FALSE)
    }
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
  # Check this before the controller, so a bad value fails the same way
  # whether rewind is enabled or not. An NA here used to be accepted, and
  # then failed later inside the capture observer.
  if (!is.null(hold_ms)) check_number(hold_ms, "hold_ms", min = 0, inclusive = TRUE)
  # 1:2 used to give the label "1".
  check_string(label, "label")
  if (!is.null(ctrl)) ctrl$open_step(label = label, hold_ms = hold_ms)
  invisible(expr)
}


#' Make changes that are not a step
#'
#' This function holds a block of code. The changes that the block makes
#' do not become a step. They become part of the current state instead, so
#' an undo never goes back across them. Use it for code that sets up a
#' view: a saved view that you restore, a bookmark that you apply, or
#' defaults that you load when the user logs in.
#'
#' It is the other side of [rewind_step()]. That function makes the changes
#' of a block into one step with a label. This one makes them into no step
#' at all.
#'
#' # Why not rewind_pause() and rewind_resume()
#'
#' A pause and a resume in one observer have no effect. `rewind` captures
#' in its own observer, which runs after the one that made the change, so
#' capture has resumed by then. This function keeps a window open after
#' the block instead, so that the changes that come back from the browser
#' later are covered too.
#'
#' # What happens around it
#'
#' * A change from the user that still waits to be written becomes its own
#'   step first, so the block does not swallow it.
#' * The steps in front of the position go, as they do for any change.
#' * A change that the user makes inside the window is part of the current
#'   state too. Keep `hold_ms` short.
#'
#' @param expr The code to run. This is usually a set of `update*Input()`
#'   calls, or assignments to tracked reactive values.
#' @param hold_ms The time in milliseconds to keep the window open after
#'   the block. The default is two times the `coalesce_ms` of the session.
#'   Increase it if a slow browser lets a change through as a step.
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
#'     # A saved view. Loading it is not something to undo.
#'     observeEvent(input$load_view, {
#'       rewind_unrecorded({
#'         updateSelectInput(session, "region", selected = "South")
#'         updateSliderInput(session, "year", value = c(2020, 2024))
#'       })
#'     })
#'   }
#' }
#' @export
rewind_unrecorded <- function(expr,
                              hold_ms = NULL,
                              session = shiny::getDefaultReactiveDomain()) {
  if (!is.null(hold_ms)) check_number(hold_ms, "hold_ms", min = 0, inclusive = TRUE)
  ctrl <- get_controller(session)
  # Open the window before the block runs. `expr` is evaluated lazily, at
  # the return below.
  if (!is.null(ctrl)) ctrl$open_unrecorded(hold_ms = hold_ms)
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
  # 1.9 used to move to position 1, as as.integer() truncates. A whole
  # number out of range still returns FALSE, as an undo at the start does.
  ctrl$jump(check_index(index, "index"))
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
#' `rewind` does not record them. Use this to stop capture for a period
#' that spans more than one reactive flush, such as while the user works
#' in a part of the application that undo must not cover.
#'
#' # Not for a block of code
#'
#' A pause and a resume in the same observer, or the same `observeEvent()`,
#' have no effect. `rewind` captures in its own observer, which runs after
#' the one that made the change. By then capture has resumed, so the change
#' is recorded as usual. A change made with `update*Input()` also comes
#' back from the browser later.
#'
#' For code that changes the application and must not make a step, such as
#' code that restores a saved view, use [rewind_unrecorded()].
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
#'     # Stop capture while an "expert mode" switch is on.
#'     observeEvent(input$expert_mode, {
#'       if (input$expert_mode) rewind_pause() else rewind_resume()
#'     })
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
