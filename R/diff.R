# diff.R -----------------------------------------------------------------
#
# rewind_diff() and the code behind it.
#
# The label on a history entry says WHICH values changed. It comes from
# diff_label() in history.R, which finds the changed names and then drops
# the values. This file keeps the values, so an application can say what a
# step actually did.
#
# The functions below that do the work take plain lists and return a plain
# data frame. They hold no Shiny code, so a test can call them directly.

#' Show what changed between two steps
#'
#' [rewind_history()] gives one row for each step, with a label such as
#' `"region, year"`. That label says which values changed. This function
#' gives the values themselves: what each one held before, and what it
#' holds now.
#'
#' Use it to tell the user what an undo will do, or to write your own
#' record of what a person changed.
#'
#' # This is not an audit trail
#'
#' The history lives in the memory of one session, and it goes when the
#' session goes. It also drops the oldest entries when it passes `depth`,
#' and it drops the steps in front of the position when a change arrives
#' after an undo. A record for an inspection must be written as each change
#' occurs, and kept somewhere else. The history is reactive, so:
#'
#' ```r
#' observeEvent(rewind_history(), {
#'   write_my_audit_row(rewind_diff())
#' })
#' ```
#'
#' # Names, and not labels
#'
#' The `name` column holds the input ID, such as `region`. It does not hold
#' the label of the widget, because Shiny does not give the server the
#' label of an input. A value that [rewind_track()] records appears as
#' `id$field`.
#'
#' # The values can be any type
#'
#' `old` and `new` are list columns, so they hold the values themselves,
#' whatever their type. The `change` column is a short description for a
#' person to read. It shows both values when they are short, such as
#' `"North -> South"`, and a count when they are not, such as
#' `"12 values"`.
#'
#' @param from,to The two positions to compare, as in the `index` column of
#'   [rewind_history()]. The default compares the current position with the
#'   one before it. Give both to compare any two positions.
#' @param session The Shiny session. The default is the current session.
#'
#' @return A data frame with one row for each value that differs, in the
#'   order of the names. It has six columns:
#'
#'   * `from`, `to`: the two positions compared.
#'   * `name`: the input ID, or `id$field` for a tracked value.
#'   * `change`: a short description that a person can read.
#'   * `old`, `new`: list columns that hold the values themselves.
#'
#'   The data frame has no rows when nothing differs, and when there is no
#'   earlier step to compare with. It depends on the history, so you can
#'   use it in `render*()` and `observe()`.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     rewind_buttons(),
#'     selectInput("region", "Region", c("North", "South", "East")),
#'     sliderInput("year", "Years", 2018, 2026, c(2018, 2026), sep = ""),
#'     tableOutput("changed")
#'   )
#'
#'   server <- function(input, output, session) {
#'     rewind_enable()
#'
#'     # What did the last step change?
#'     output$changed <- renderTable({
#'       rewind_diff()[, c("name", "change")]
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @export
rewind_diff <- function(from = NULL,
                        to = NULL,
                        session = shiny::getDefaultReactiveDomain()) {
  ctrl <- require_controller(session)

  # Depend on the history, so this function can run inside render*().
  ctrl$version_dep()

  size <- ctrl$history$size()
  if (size == 0L) return(empty_diff())

  # An index the caller gave is checked. An index that this function works
  # out for itself is not, because "there is no step before the first one"
  # is a normal condition and not a mistake by the caller.
  given <- !is.null(from) || !is.null(to)

  to <- if (is.null(to)) ctrl$history$index() else check_index(to, "to")
  from <- if (is.null(from)) to - 1L else check_index(from, "from")

  if (given) {
    if (from < 1L || from > size) {
      stop(sprintf("`from` must be between 1 and %d.", size), call. = FALSE)
    }
    if (to < 1L || to > size) {
      stop(sprintf("`to` must be between 1 and %d.", size), call. = FALSE)
    }
  } else if (from < 1L) {
    return(empty_diff())
  }

  diff_states(
    flatten_state(ctrl$history$state_at(from)),
    flatten_state(ctrl$history$state_at(to)),
    from = from,
    to = to
  )
}


#' Compare two flattened states
#'
#' This is the code behind [rewind_diff()]. It takes two named lists and
#' holds no Shiny code.
#'
#' @param old,new Named lists, from `flatten_state()`. Either can be `NULL`.
#' @param from,to The positions the two states came from. They go into the
#'   result unchanged.
#' @return A data frame. Refer to [rewind_diff()] for the columns.
#' @keywords internal
#' @noRd
diff_states <- function(old, new, from = NA_integer_, to = NA_integer_) {
  old <- old %||% list()
  new <- new %||% list()

  names_all <- union(names(old), names(new))
  if (length(names_all) == 0L) return(empty_diff())

  # values_equal() is the same comparison that the history uses to decide
  # that a state is new. The diff and the history thus never disagree about
  # what changed.
  changed <- names_all[!vapply(
    names_all,
    function(nm) values_equal(old[[nm]], new[[nm]]),
    logical(1)
  )]
  if (length(changed) == 0L) return(empty_diff())

  changed <- sort(changed)

  out <- data.frame(
    from   = rep(as.integer(from), length(changed)),
    to     = rep(as.integer(to), length(changed)),
    name   = changed,
    change = vapply(
      changed,
      function(nm) format_change(old[[nm]], new[[nm]]),
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  # The values go in as list columns, so a value of any type survives.
  # unname() keeps the list columns free of the names that lapply() adds.
  out$old <- unname(lapply(changed, function(nm) old[[nm]]))
  out$new <- unname(lapply(changed, function(nm) new[[nm]]))

  rownames(out) <- NULL
  out
}


#' The empty result
#'
#' Every path out of [rewind_diff()] gives the same columns and the same
#' types. An application can thus bind the results together, and can index
#' a column, without a test for zero rows first.
#'
#' @keywords internal
#' @noRd
empty_diff <- function() {
  out <- data.frame(
    from   = integer(0),
    to     = integer(0),
    name   = character(0),
    change = character(0),
    stringsAsFactors = FALSE
  )
  out$old <- list()
  out$new <- list()
  out
}


#' Describe one change for a person to read
#'
#' @param old,new The two values. Either can be `NULL`.
#' @keywords internal
#' @noRd
format_change <- function(old, new) {
  if (is.null(old) && is.null(new)) return("no change")
  if (is.null(old)) return(paste("set to", format_value(new)))
  if (is.null(new)) return("cleared")
  paste(format_value(old), "->", format_value(new))
}


#' Describe one value in a few words
#'
#' A value in the history can be any type. It can be one string from a
#' `selectInput()`, two dates from a `dateRangeInput()`, or a whole data
#' frame from a tracked reactive value. This function gives a short text
#' for each of them.
#'
#' @param x The value.
#' @param max_items The largest number of elements to list one by one.
#' @param max_width The largest number of characters in the result.
#' @keywords internal
#' @noRd
format_value <- function(x, max_items = 3L, max_width = 40L) {
  if (is.null(x)) return("(none)")
  if (is.data.frame(x)) {
    return(sprintf("a table of %d row%s", nrow(x), if (nrow(x) == 1L) "" else "s"))
  }
  if (!is.atomic(x)) return(sprintf("a list of %d", length(x)))
  if (length(x) == 0L) return("(empty)")

  # A date comes back from the browser as text. format() gives the same
  # result for both forms.
  txt <- if (inherits(x, c("Date", "POSIXt"))) format(x) else as.character(x)
  txt[is.na(txt)] <- "NA"

  if (length(txt) == 1L) return(truncate_text(txt, max_width))

  if (length(txt) <= max_items) {
    joined <- paste(txt, collapse = ", ")
    if (nchar(joined) <= max_width) return(joined)
  }

  sprintf("%d values", length(txt))
}


#' Cut a long text, and show that it was cut
#'
#' @param x One string.
#' @param max_width The largest number of characters to keep.
#' @keywords internal
#' @noRd
truncate_text <- function(x, max_width) {
  if (nchar(x) <= max_width) return(x)
  paste0(substr(x, 1L, max_width - 3L), "...")
}


#' Check an index that the caller gave
#'
#' @param x The value.
#' @param arg The name of the argument, for the message.
#' @keywords internal
#' @noRd
check_index <- function(x, arg) {
  x <- suppressWarnings(as.integer(x))
  if (length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a single whole number.", arg), call. = FALSE)
  }
  x
}
