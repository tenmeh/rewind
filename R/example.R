#' Run the demo application
#'
#' Starts a small Shiny application that shows `rewind` at work: undo and
#' redo with the buttons and the keyboard, the history rail, and a step that
#' groups several changes. Its source is in the folder `examples/demo` of
#' the installed package.
#'
#' @param ... Passed on to [shiny::runApp()], such as `port` or
#'   `launch.browser`.
#'
#' @return The value of [shiny::runApp()], which runs until the application
#'   stops.
#'
#' @examples
#' if (interactive()) {
#'   rewind_example()
#' }
#' @export
rewind_example <- function(...) {
  app_dir <- system.file("examples", "demo", package = "rewind")
  if (!nzchar(app_dir)) {
    stop(
      "The demo application is missing. Install `rewind` again.",
      call. = FALSE
    )
  }
  shiny::runApp(app_dir, ...)
}
