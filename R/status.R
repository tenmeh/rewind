# status.R ----------------------------------------------------------------
#
# rewind_status() answers "why does an undo skip this input?", and
# rewind_exclude() lets any part of an application, a module included, keep
# its inputs out of the history.

#' See what rewind captures, and why it leaves an input out
#'
#' An undo that skips an input is hard to explain from outside. The input
#' can be excluded, can be a password field, can be missing from `inputs`,
#' or can be one that the page cannot put back. This function lists every
#' input of the session with the reason, so one call answers the question.
#'
#' It reads every input, so inside `render*()` the result follows the
#' inputs and the history. That makes it useful in a debug panel, for
#' example `renderPrint(rewind_status())`.
#'
#' @param session The Shiny session. The default is the current session.
#'
#' @return A list:
#'
#'   * `captured`: the names of the inputs that rewind records.
#'   * `skipped`: a named character vector. Each name is an input, and each
#'     value is the reason it is not recorded: `"excluded"`,
#'     `"password field"`, `"not in inputs"`,
#'     `"cannot be restored by the page"`, `"action button"`,
#'     `"file upload"` or `"NULL value"`. Inputs internal to Shiny and to
#'     rewind are left out of the list.
#'   * `tracked`: the groups registered with [rewind_track()], each with the
#'     fields it names (`NULL` for all fields).
#'   * `restorable`: the inputs that the page reported it can restore, or
#'     `NULL` before the report arrives. Refer to [rewind_enable()].
#'   * `paused`: `TRUE` between [rewind_pause()] and [rewind_resume()].
#'   * `size`, `position`: the number of steps, and the current one.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     rewind_buttons(),
#'     selectInput("region", "Region", c("North", "South")),
#'     passwordInput("pw", "Password"),
#'     verbatimTextOutput("status")
#'   )
#'
#'   server <- function(input, output, session) {
#'     rewind_enable()
#'     output$status <- renderPrint(rewind_status()$skipped)
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @export
rewind_status <- function(session = shiny::getDefaultReactiveDomain()) {
  ctrl <- require_controller(session)
  ctrl$version_dep()

  reasons <- ctrl$input_reasons()
  reasons <- reasons[!reasons %in% c("internal to Shiny", "part of rewind")]
  reasons <- reasons[order(names(reasons))]
  settings <- ctrl$capture_settings()

  list(
    captured   = names(reasons)[is.na(reasons)],
    skipped    = reasons[!is.na(reasons)],
    tracked    = settings$tracked,
    restorable = settings$bound,
    paused     = ctrl$is_paused(),
    size       = ctrl$history$size(),
    position   = ctrl$history$index()
  )
}


#' Keep inputs out of the history
#'
#' `rewind_enable(exclude = )` leaves inputs out, but only the code that
#' calls `rewind_enable()` can set it. This function can be called from
#' anywhere in the server, a module included, at any time. It works like
#' [shiny::setBookmarkExclude()]: inside a module, give the names that the
#' module uses, and rewind adds the namespace.
#'
#' Two things differ from `setBookmarkExclude()`. A second call adds to the
#' list; it does not replace it, so a module cannot undo an exclusion made
#' elsewhere. And a value already in the history for these inputs is
#' removed, together with any step that changed only them.
#'
#' A call before [rewind_enable()] in the same session is kept, and applied
#' when `rewind_enable()` runs.
#'
#' @param names Character vector of input names, as the calling module uses
#'   them.
#' @param session The Shiny session. The default is the current session.
#'
#' @return `names`, invisibly.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   search_ui <- function(id) textInput(NS(id, "query"), "Search")
#'   search_server <- function(id) {
#'     moduleServer(id, function(input, output, session) {
#'       # A search box that the user does not expect undo to move.
#'       rewind_exclude("query")
#'     })
#'   }
#'
#'   ui <- fluidPage(
#'     rewind_buttons(),
#'     selectInput("region", "Region", c("North", "South")),
#'     search_ui("search")
#'   )
#'
#'   server <- function(input, output, session) {
#'     rewind_enable()
#'     search_server("search")
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @export
rewind_exclude <- function(names = character(0),
                           session = shiny::getDefaultReactiveDomain()) {
  session <- require_session(session)
  if (!is.character(names)) {
    stop("`names` must be a character vector.", call. = FALSE)
  }

  # The full ids in the page. Inside a module, add the namespace, as the
  # browser reports ids with it. At the top level add nothing: the session
  # of shiny::testServer() gives "mock-session-" from ns(), where a real
  # session gives the name as it is.
  full <- if (identical(session, session$rootScope())) {
    names
  } else {
    vapply(names, session$ns, character(1), USE.NAMES = FALSE)
  }

  ctrl <- get_controller(session)
  if (is.null(ctrl)) {
    session$userData$.rewind_exclude <- union(session$userData$.rewind_exclude, full)
  } else {
    ctrl$add_exclude(full)
  }
  invisible(names)
}
