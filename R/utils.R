`%||%` <- function(x, y) if (is.null(x)) y else x


#' Get the session argument. Show a clear error if there is no session.
#' @keywords internal
#' @noRd
require_session <- function(session, hint = NULL) {
  if (is.null(session)) {
    stop(
      "No Shiny session found. Call this from inside a server function, ",
      "or pass `session` explicitly.",
      call. = FALSE
    )
  }
  # A session is an environment, or a list in some tests. A plain vector
  # is always a mistake, and usually a positional argument in the wrong
  # place: rewind_enable(c("region", "year")) gives the input names to
  # `session`, which is the first argument. Without this check the error
  # is "$ operator is invalid for atomic vectors", which hides the cause.
  if (is.atomic(session)) {
    stop(
      "`session` must be a Shiny session, not a ", class(session)[1],
      " vector.", if (!is.null(hint)) paste0(" ", hint),
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


#' Check that an argument is one finite number within a limit
#'
#' `NA`, `NaN` and `Inf` all fail. A plain `x < 0` test does not catch them:
#' `NA < 0` is `NA`, and `if (NA)` stops with "missing value where TRUE/FALSE
#' needed", a message that does not name the argument. `Inf` is worse. It
#' passes such a test, and then does harm with no message at all: an
#' infinite `coalesce_ms` means that no change is ever written to the
#' history.
#'
#' @param x The value.
#' @param arg The name of the argument, for the message.
#' @param min The lower limit.
#' @param inclusive `TRUE` accepts `min` itself. `FALSE` needs a value
#'   greater than `min`.
#' @return `x`, invisibly.
#' @keywords internal
#' @noRd
check_number <- function(x, arg, min = 0, inclusive = TRUE) {
  ok <- is.numeric(x) && length(x) == 1L && is.finite(x) &&
    (if (inclusive) x >= min else x > min)
  if (!ok) {
    limit <- if (inclusive) sprintf("of %s or more", min) else sprintf("greater than %s", min)
    stop(sprintf("`%s` must be a single finite number %s.", arg, limit), call. = FALSE)
  }
  invisible(x)
}


#' Check that an argument is TRUE or FALSE
#'
#' `isTRUE()` alone turned `"yes"` and `NA` into `FALSE` with no message, so
#' a typo switched a feature off.
#'
#' @param x The value.
#' @param arg The name of the argument, for the message.
#' @return `x`, invisibly.
#' @keywords internal
#' @noRd
check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", arg), call. = FALSE)
  }
  invisible(x)
}


#' Check that an argument is one whole number, at least `min`
#'
#' `as.integer()` truncates, so `depth = 2.5` became 2 with no message.
#'
#' @param x The value.
#' @param arg The name of the argument, for the message.
#' @param min The lower limit.
#' @return `x` as an integer.
#' @keywords internal
#' @noRd
check_count <- function(x, arg, min = 0L) {
  ok <- is.numeric(x) && length(x) == 1L && is.finite(x) && x == round(x) && x >= min
  if (!ok) {
    stop(sprintf("`%s` must be a single whole number of %d or more.", arg, min),
         call. = FALSE)
  }
  as.integer(x)
}


#' Check that an argument is NULL or one string
#'
#' @param x The value.
#' @param arg The name of the argument, for the message.
#' @return `x`, invisibly.
#' @keywords internal
#' @noRd
check_string <- function(x, arg, null_ok = TRUE) {
  if (is.null(x) && null_ok) return(invisible(x))
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a single string%s.", arg,
                 if (null_ok) " or NULL" else ""), call. = FALSE)
  }
  invisible(x)
}


#' Check that an argument is NULL or a character vector
#'
#' @param x The value.
#' @param arg The name of the argument, for the message.
#' @return `x`, invisibly.
#' @keywords internal
#' @noRd
check_character <- function(x, arg) {
  if (!is.null(x) && (!is.character(x) || anyNA(x))) {
    stop(sprintf("`%s` must be a character vector or NULL.", arg), call. = FALSE)
  }
  invisible(x)
}
