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
#' # Appearance
#'
#' The buttons carry `btn btn-default`, so a Bootstrap theme applies to
#' them without any work. Use `button_class` to give them a different
#' Bootstrap variant, size or shape:
#'
#' ```r
#' rewind_buttons(button_class = "btn-primary")
#' rewind_buttons(button_class = "btn-outline-secondary btn-sm")
#' ```
#'
#' A variant that you add wins over `btn-default`, so the buttons take the
#' colours of the theme of your application. `bslib::bs_theme()` decides
#' what those colours are. `rewind` thus has no colours of its own to keep
#' in agreement with your application.
#'
#' @param undo_label,redo_label The button labels. Use `NULL` for a button
#'   with an icon only. The function then sets the accessible name
#'   (`aria-label`) to "Undo" or "Redo". A screen reader can thus announce
#'   the button correctly when it has no text.
#' @param class More CSS classes for the container element.
#' @param button_class More CSS classes for the two buttons. Use it for a
#'   Bootstrap variant such as `"btn-primary"` or `"btn-outline-secondary"`,
#'   and for a size such as `"btn-sm"`.
#'
#' @return A [htmltools::tagList()].
#' @examples
#' rewind_buttons()
#' rewind_buttons(undo_label = NULL, redo_label = NULL)
#' rewind_buttons(button_class = "btn-outline-primary btn-sm")
#' @export
rewind_buttons <- function(undo_label = "Undo",
                           redo_label = "Redo",
                           class = NULL,
                           button_class = NULL) {
  btn_class <- function(direction) {
    paste(c("btn", "btn-default", button_class, direction), collapse = " ")
  }

  htmltools::attachDependencies(
    htmltools::tags$div(
      class = paste(c("rewind-buttons", class), collapse = " "),
      htmltools::tags$button(
        type = "button",
        class = btn_class("rewind-undo"),
        disabled = NA,
        title = "Undo (Ctrl+Z)",
        `aria-label` = undo_label %||% "Undo",
        rewind_icon("undo"),
        if (!is.null(undo_label)) htmltools::tags$span(
          class = "rewind-label", undo_label
        )
      ),
      htmltools::tags$button(
        type = "button",
        class = btn_class("rewind-redo"),
        disabled = NA,
        title = "Redo (Ctrl+Shift+Z or Ctrl+Y)",
        `aria-label` = redo_label %||% "Redo",
        rewind_icon("redo"),
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


#' The arrow for an undo or a redo button
#'
#' The buttons used the HTML entities `&#8630;` and `&#8631;` before. Those
#' come from the font of the browser. They thus have a different weight and
#' a different size on each system, and some fonts do not hold them at all.
#'
#' An SVG gives the same shape everywhere. It uses `currentColor`, so it
#' takes the colour of the button text. It thus follows any Bootstrap
#' variant that the caller gives to `button_class`, and it stays correct in
#' a dark theme.
#'
#' @param direction `"undo"` or `"redo"`.
#' @return An [htmltools::tag()].
#' @keywords internal
#' @noRd
rewind_icon <- function(direction = c("undo", "redo")) {
  direction <- match.arg(direction)

  # An arrow head, and a shaft that turns back on itself. The redo arrow is
  # the undo arrow with each x position taken from 16.
  paths <- if (direction == "undo") {
    c("M5.5 3 2.5 6l3 3", "M2.5 6h7a3.5 3.5 0 0 1 0 7H6")
  } else {
    c("M10.5 3 13.5 6l-3 3", "M13.5 6h-7a3.5 3.5 0 0 0 0 7H10")
  }

  htmltools::tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 16 16",
    width = "1em",
    height = "1em",
    fill = "none",
    stroke = "currentColor",
    `stroke-width` = "1.7",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    class = "rewind-icon",
    # The button already has an aria-label, so a screen reader must not
    # announce this image as well.
    `aria-hidden` = "true",
    focusable = "false",
    lapply(paths, function(d) htmltools::tags$path(d = d))
  )
}
