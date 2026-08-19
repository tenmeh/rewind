#' The rewind HTML dependency
#'
#' [rewind_enable()] adds this automatically. You thus rarely need this
#' function. Use it when you want the assets before the server function
#' runs. Use it also when you cannot use `insertUI()`.
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
#' These two buttons connect to the history of the session. They become
#' enabled and disabled as the history permits. You thus do not have to
#' write an observer on the server.
#'
#' @param undo_label,redo_label The button labels. Use `NULL` for a button
#'   with an icon only. The function then sets the accessible name
#'   (`aria-label`) to "Undo" or "Redo". A screen reader can thus announce
#'   the button correctly when it has no text.
#' @param class More CSS classes for the container element. The buttons
#'   have the classes `btn btn-default`. Bootstrap themes thus apply to
#'   them.
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
        `aria-label` = undo_label %||% "Undo",
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
        `aria-label` = redo_label %||% "Redo",
        htmltools::HTML("&#8631;"),
        if (!is.null(redo_label)) htmltools::tags$span(
          class = "rewind-label", redo_label
        )
      )
    ),
    rewind_dependency()
  )
}


#' A scrubbable history rail
#'
#' This function draws the history as a vertical list of steps. The newest
#' step is at the end. The rail shows the current position. A click on a
#' step moves to that step. The rail gets its data from the server. You thus
#' do not have to write a `render` function.
#'
#' @param label The heading above the rail. Use `NULL` for no heading.
#' @param max_height The CSS height at which the rail starts to scroll.
#' @param class More CSS classes for the container element.
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
