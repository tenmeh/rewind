`%||%` <- function(x, y) if (is.null(x)) y else x


#' Get the session argument. Show a clear error if there is no session.
#' @keywords internal
#' @noRd
require_session <- function(session) {
  if (is.null(session)) {
    stop(
      "No Shiny session found. Call this from inside a server function, ",
      "or pass `session` explicitly.",
      call. = FALSE
    )
  }
  session
}


#' Get the controller of a session. Give `NULL` if there is none.
#' @keywords internal
#' @noRd
get_controller <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) return(NULL)
  session$userData$.rewind
}


#' Get the controller of a session. Show an error if `rewind` is not enabled.
#' @keywords internal
#' @noRd
require_controller <- function(session = shiny::getDefaultReactiveDomain()) {
  session <- require_session(session)
  ctrl <- session$userData$.rewind
  if (is.null(ctrl)) {
    stop(
      "rewind is not enabled for this session. ",
      "Call `rewind_enable()` in your server function first.",
      call. = FALSE
    )
  }
  ctrl
}
