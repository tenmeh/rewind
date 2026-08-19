#' The undo/redo history stack
#'
#' This [R6][R6::R6Class] class holds an ordered list of application states.
#' It also holds a position in that list. Undo moves the position backwards.
#' Redo moves it forwards. A new state removes each entry after the
#' position.
#'
#' This class is available only for tests and for advanced use. Application
#' code must use [rewind_enable()] and the related functions.
#'
#' @keywords internal
#' @noRd
History <- R6::R6Class(
  "RewindHistory",
  public = list(

    # @description Make a history stack.
    # @param depth The maximum number of entries to keep.
    initialize = function(depth = 50L) {
      depth <- as.integer(depth)
      if (is.na(depth) || depth < 2L) {
        stop("`depth` must be a single integer >= 2.", call. = FALSE)
      }
      private$.depth <- depth
      private$.entries <- list()
      private$.index <- 0L
      invisible(self)
    },

    # @description Remove all the history. Then start the stack with `state`.
    # @param state A snapshot. This is a named list.
    # @param label A label for the entry that a person can read.
    reset = function(state, label = "Initial state") {
      private$.entries <- list(private$entry(state, label))
      private$.index <- 1L
      invisible(self)
    },

    # @description
    # Add `state` to the end of the stack. This method removes each entry
    # after the current position. This is the usual operation of an undo
    # stack. A change after an undo removes the steps that the undo left.
    #
    # This method ignores a state that is the same as the current state.
    # This is what stops the echo. The echo comes back from the browser
    # after a restore.
    #
    # @param state A snapshot. This is a named list.
    # @param label A label for the entry that a person can read.
    # @return `TRUE` if the method added an entry. `FALSE` if it did
    #   nothing.
    push = function(state, label = NULL) {
      if (private$.index > 0L &&
            states_equal(state, private$.entries[[private$.index]]$state)) {
        return(invisible(FALSE))
      }

      if (private$.index < length(private$.entries)) {
        private$.entries <- private$.entries[seq_len(private$.index)]
      }

      private$.entries[[length(private$.entries) + 1L]] <-
        private$entry(state, label)
      private$.index <- length(private$.entries)

      # Remove the oldest entries when the stack is longer than `depth`.
      overflow <- length(private$.entries) - private$.depth
      if (overflow > 0L) {
        private$.entries <- private$.entries[-seq_len(overflow)]
        private$.index <- private$.index - overflow
      }

      invisible(TRUE)
    },

    # @description Move one step backwards. This method gives the new
    # current state, or `NULL`.
    undo = function() {
      if (!self$can_undo()) return(NULL)
      private$.index <- private$.index - 1L
      self$current()
    },

    # @description Move one step forwards. This method gives the new
    # current state, or `NULL`.
    redo = function() {
      if (!self$can_redo()) return(NULL)
      private$.index <- private$.index + 1L
      self$current()
    },

    # @description Move the position to a given entry.
    # @param index The position. The first position is 1.
    jump = function(index) {
      index <- as.integer(index)
      if (is.na(index) || index < 1L || index > length(private$.entries)) {
        return(NULL)
      }
      private$.index <- index
      self$current()
    },

    # @description The state at the current position.
    current = function() {
      if (private$.index == 0L) return(NULL)
      private$.entries[[private$.index]]$state
    },

    # @description Is there an entry before the current position?
    can_undo = function() private$.index > 1L,

    # @description Is there an entry after the current position?
    can_redo = function() private$.index < length(private$.entries),

    # @description The number of entries that the stack keeps.
    size = function() length(private$.entries),

    # @description The current position. It is 0 when the stack is empty.
    index = function() private$.index,

    # @description
    # A data frame that describes the stack. Use it to draw a history rail.
    # It gives one row for each entry, in sequence.
    entries = function() {
      n <- length(private$.entries)
      if (n == 0L) {
        return(data.frame(
          index   = integer(),
          label   = character(),
          time    = as.POSIXct(character()),
          current = logical(),
          stringsAsFactors = FALSE
        ))
      }
      data.frame(
        index   = seq_len(n),
        label   = vapply(private$.entries, function(e) e$label, character(1)),
        time    = as.POSIXct(
          vapply(private$.entries, function(e) as.numeric(e$time), numeric(1)),
          origin = "1970-01-01", tz = "UTC"
        ),
        current = seq_len(n) == private$.index,
        stringsAsFactors = FALSE
      )
    },

    # @description Remove each entry but the current one.
    clear = function() {
      keep <- self$current()
      if (is.null(keep)) {
        private$.entries <- list()
        private$.index <- 0L
      } else {
        self$reset(keep, "Initial state")
      }
      invisible(self)
    }
  ),

  private = list(
    .entries = NULL,
    .index   = 0L,
    .depth   = 50L,

    entry = function(state, label) {
      list(
        state = state,
        label = if (is.null(label)) describe_state(state) else as.character(label)[1],
        time  = Sys.time()
      )
    }
  )
)


#' Compare two snapshots for equality
#'
#' Snapshots come from two sources. `rewind` captures some directly in R.
#' The browser sends the others back as JSON. The two sources do not always
#' give the same object. This function is thus less strict than
#' [identical()]. It sorts the names. It compares numbers with a tolerance,
#' and not by their form.
#'
#' @param a,b Snapshots. These are named lists.
#' @return One logical value.
#' @keywords internal
#' @noRd
states_equal <- function(a, b) {
  if (is.null(a) || is.null(b)) return(identical(a, b))
  if (!is.list(a) || !is.list(b)) return(isTRUE(all.equal(a, b)))

  if (!setequal(names(a), names(b))) return(FALSE)
  nms <- sort(names(a))
  for (nm in nms) {
    if (!values_equal(a[[nm]], b[[nm]])) return(FALSE)
  }
  TRUE
}

values_equal <- function(x, y) {
  if (is.null(x) && is.null(y)) return(TRUE)
  if (is.null(x) || is.null(y)) return(FALSE)
  if (length(x) != length(y)) return(FALSE)

  # Examine the lists inside a list. These are usually the tracked values.
  if (is.list(x) && is.list(y)) {
    if (!setequal(names(x), names(y))) return(FALSE)
    if (is.null(names(x))) {
      return(all(vapply(seq_along(x), function(i) values_equal(x[[i]], y[[i]]),
                        logical(1))))
    }
    return(all(vapply(names(x), function(nm) values_equal(x[[nm]], y[[nm]]),
                      logical(1))))
  }

  if (is.numeric(x) && is.numeric(y)) {
    return(isTRUE(all.equal(as.numeric(x), as.numeric(y),
                            tolerance = 1e-8, check.attributes = FALSE)))
  }

  # Dates and times frequently come back from JSON as text. A comparison of
  # the text form is thus the most reliable method.
  if (inherits(x, c("Date", "POSIXt")) || inherits(y, c("Date", "POSIXt"))) {
    return(identical(as.character(x), as.character(y)))
  }

  isTRUE(all.equal(x, y, check.attributes = FALSE))
}


#' Make a default label for a history entry
#'
#' @param state A snapshot.
#' @keywords internal
#' @noRd
describe_state <- function(state) {
  n <- length(state)
  if (n == 0L) return("Empty state")
  sprintf("%d value%s changed", n, if (n == 1L) "" else "s")
}


#' Describe what changed between two snapshots
#'
#' `rewind` uses this to label the history entries automatically. The rail
#' thus shows "region, year" and not "state 7".
#'
#' @param old,new Snapshots.
#' @param max_names The number of names to show before the text is cut.
#' @keywords internal
#' @noRd
diff_label <- function(old, new, max_names = 3L) {
  if (is.null(old)) return("Initial state")
  nms <- union(names(old), names(new))
  changed <- nms[!vapply(nms, function(nm) values_equal(old[[nm]], new[[nm]]),
                         logical(1))]

  if (length(changed) == 0L) return("No change")
  if (length(changed) <= max_names) return(paste(changed, collapse = ", "))
  sprintf("%s and %d more",
          paste(changed[seq_len(max_names)], collapse = ", "),
          length(changed) - max_names)
}
