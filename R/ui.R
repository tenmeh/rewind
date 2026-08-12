#' The rewind HTML dependency
#'
#' [rewind_enable()] injects this automatically, so you rarely need it. It is
#' exported for the case where you want the assets present before the server
#' function runs, or where `insertUI()` is unavailable to you.
#'
#' @return An [htmltools::htmlDependency()].
#' @examples
#' rewind_dependency()
#' @export
rewind_dependency <- function() {
  htmltools::htmlDependency(
    name       = "rewind",
    version    = as.character(utils::packageVersion("rewind")),
    src        = c(file = system.file("www", package = "rewind")),
    script     = "rewind.js",
    stylesheet = "rewind.css"
  )
}


#' Undo and redo buttons
#'
#' A pair of buttons wired to the session's history. They enable and disable
#' themselves as the history allows, so there is nothing to observe on the
#' server.
#'
#' @param undo_label,redo_label Button labels. Pass `NULL` for an icon-only
#'   button.
#' @param class Extra CSS classes applied to the containing element. The
#'   buttons themselves carry `btn btn-default`, so Bootstrap themes apply.
#'
#' @return A [htmltools::tagList()].
#' @examples
#' rewind_buttons()
#' rewind_buttons(undo_label = NULL, redo_label = NULL)
#' @export
rewind_buttons <- function(undo_label = "Undo",
                           redo_label = "Redo",
                           class = NULL) {
  htmltools::attachDependencies(
    htmltools::tags$div(
      class = paste(c("rewind-buttons", class), collapse = " "),
      htmltools::tags$button(
        type = "button",
        class = "btn btn-default rewind-undo",
        disabled = NA,
        title = "Undo (Ctrl+Z)",
        htmltools::HTML("&#8630;"),
        if (!is.null(undo_label)) htmltools::tags$span(
          class = "rewind-label", undo_label
        )
      ),
      htmltools::tags$button(
        type = "button",
        class = "btn btn-default rewind-redo",
        disabled = NA,
        title = "Redo (Ctrl+Shift+Z)",
        htmltools::HTML("&#8631;"),
        if (!is.null(redo_label)) htmltools::tags$span(
          class = "rewind-label", redo_label
        )
      )
    ),
    rewind_dependency()
  )
}


#' A scrubable history rail
#'
#' Renders the history as a vertical list of steps, newest last, with the
#' current position highlighted. Clicking a step jumps straight to it. The rail
#' populates itself from the server; there is no `render` function to write.
#'
#' @param label Heading shown above the rail. Pass `NULL` to omit it.
#' @param max_height CSS height at which the rail starts scrolling.
#' @param class Extra CSS classes for the containing element.
#'
#' @return A [htmltools::tagList()].
#' @examples
#' rewind_ui()
#' rewind_ui(label = "Steps", max_height = "12rem")
#' @export
rewind_ui <- function(label = "History",
                      max_height = "20rem",
                      class = NULL) {
  htmltools::attachDependencies(
    htmltools::tags$div(
      class = paste(c("rewind-rail-wrap", class), collapse = " "),
      if (!is.null(label)) {
        htmltools::tags$div(class = "rewind-rail-title", label)
      },
      htmltools::tags$ol(
        class = "rewind-rail",
        style = sprintf("max-height: %s;", max_height)
      )
    ),
    rewind_dependency()
  )
}
