#' The undo/redo history stack
#'
#' An [R6][R6::R6Class] class holding an ordered list of application states and
#' a pointer into that list. Undo moves the pointer backwards, redo moves it
#' forwards, and pushing a new state truncates anything ahead of the pointer.
#'
#' This class is exported only for testing and advanced use; application code
#' should go through [rewind_enable()] and friends.
#'
#' @keywords internal
#' @noRd
History <- R6::R6Class(
  "RewindHistory",
  public = list(

    # @description Create a history stack.
    # @param depth Maximum number of entries to retain.
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

    # @description Discard all history and seed the stack with `state`.
    # @param state A snapshot (named list).
    # @param label Human readable label for the entry.
    reset = function(state, label = "Initial state") {
      private$.entries <- list(private$entry(state, label))
      private$.index <- 1L
      invisible(self)
    },

    # @description
    # Append `state`. Anything ahead of the current pointer is discarded, which
    # is the standard behaviour for an undo stack: editing after undoing
    # abandons the abandoned future.
    #
    # States identical to the current state are ignored, which is what
    # suppresses the echo produced when a restore round-trips back from the
    # browser.
    #
    # @param state A snapshot (named list).
    # @param label Human readable label for the entry.
    # @return `TRUE` if an entry was added, `FALSE` if it was a no-op.
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

      # Trim from the front once we exceed `depth`.
      overflow <- length(private$.entries) - private$.depth
      if (overflow > 0L) {
        private$.entries <- private$.entries[-seq_len(overflow)]
        private$.index <- private$.index - overflow
      }

      invisible(TRUE)
    },

    # @description Step backwards. Returns the state now current, or `NULL`.
    undo = function() {
      if (!self$can_undo()) return(NULL)
      private$.index <- private$.index - 1L
      self$current()
    },

    # @description Step forwards. Returns the state now current, or `NULL`.
    redo = function() {
      if (!self$can_redo()) return(NULL)
      private$.index <- private$.index + 1L
      self$current()
    },

    # @description Move the pointer to an absolute position.
    # @param index Position, 1-based.
    jump = function(index) {
      index <- as.integer(index)
      if (is.na(index) || index < 1L || index > length(private$.entries)) {
        return(NULL)
      }
      private$.index <- index
      self$current()
    },

    # @description The state at the current pointer position.
    current = function() {
      if (private$.index == 0L) return(NULL)
      private$.entries[[private$.index]]$state
    },

    # @description Is there anything behind the pointer?
    can_undo = function() private$.index > 1L,

    # @description Is there anything ahead of the pointer?
    can_redo = function() private$.index < length(private$.entries),

    # @description Number of retained entries.
    size = function() length(private$.entries),

    # @description Current pointer position (0 when empty).
    index = function() private$.index,

    # @description
    # A data frame describing the stack, suitable for rendering a history
    # rail. One row per entry, in order.
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

    # @description Drop every entry except the current one.
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
#' Snapshots arrive from two directions: captured directly in R, and echoed
#' back from the browser after a JSON round trip. Those two paths do not always
#' produce bit-identical objects, so this is deliberately a little more
#' forgiving than [identical()]: names are sorted, and atomic vectors are
#' compared with tolerance rather than by representation.
#'
#' @param a,b Snapshots (named lists).
#' @return A single logical.
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

  # Recurse into nested lists (a namespaced sub-snapshot, typically).
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

  # Dates and times survive the JSON round trip as strings often enough that
  # comparing the character rendering is the pragmatic choice.
  if (inherits(x, c("Date", "POSIXt")) || inherits(y, c("Date", "POSIXt"))) {
    return(identical(as.character(x), as.character(y)))
  }

  isTRUE(all.equal(x, y, check.attributes = FALSE))
}


#' Produce a default label for a history entry
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
#' Used to label history entries automatically, so the rail reads
#' "region, year" rather than "state 7".
#'
#' @param old,new Snapshots.
#' @param max_names How many names to list before truncating.
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
