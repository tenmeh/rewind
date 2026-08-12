#' The per-session rewind controller
#'
#' Owns the history stack, the capture/coalesce machinery, and the restore
#' path. One instance is created per Shiny session by [rewind_enable()] and
#' stashed in `session$userData$.rewind`.
#'
#' @keywords internal
#' @noRd
RewindController <- R6::R6Class(
  "RewindController",
  public = list(

    history = NULL,

    initialize = function(session,
                          inputs = NULL,
                          exclude = NULL,
                          depth = 50L,
                          coalesce_ms = 400L,
                          verbose = FALSE) {
      private$.session     <- session
      private$.inputs      <- inputs
      private$.exclude     <- exclude
      private$.coalesce_ms <- as.numeric(coalesce_ms)
      private$.verbose     <- isTRUE(verbose)
      private$.tracked     <- list()
      private$.tick        <- shiny::reactiveVal(0L)
      private$.version     <- shiny::reactiveVal(0L)
      self$history         <- History$new(depth = depth)
      invisible(self)
    },

    # A reactive dependency that invalidates whenever the history changes.
    # Lets [rewind_history()] and friends be used inside `render*()`.
    version_dep = function() private$.version(),

    # ---- capture ---------------------------------------------------------

    # Read the current state of everything we track. Must be called from a
    # reactive context; that is what makes capture automatic.
    snapshot = function() {
      list(
        inputs = private$snapshot_inputs(),
        values = private$snapshot_values()
      )
    },

    # Record a candidate state and (re)start the coalescing window.
    note = function(state) {
      if (private$.paused) return(invisible(FALSE))

      # Is this the echo of a restore we just performed?
      if (!is.null(private$.expecting)) {
        if (states_equal(state, private$.expecting)) {
          private$.expecting <- NULL
          private$log("echo absorbed")
          return(invisible(FALSE))
        }
        # The browser has not finished applying our restore yet. Ignore
        # intermediate states until it has, or until we give up waiting.
        if (difftime(Sys.time(), private$.expecting_since, units = "secs") <
            private$.expect_timeout) {
          private$log("ignoring intermediate state while restoring")
          return(invisible(FALSE))
        }
        private$log("restore echo timed out; resuming capture")
        private$.expecting <- NULL
      }

      private$.pending <- state
      deadline <- Sys.time() + private$.coalesce_ms / 1000
      if (!is.null(private$.hold_until) && private$.hold_until > deadline) {
        deadline <- private$.hold_until
      }
      private$.deadline <- deadline
      private$.tick(shiny::isolate(private$.tick()) + 1L)
      invisible(TRUE)
    },

    # Milliseconds until the pending state should be committed. `Inf` when
    # nothing is pending.
    time_to_flush = function() {
      if (is.null(private$.pending)) return(Inf)
      ms <- as.numeric(difftime(private$.deadline, Sys.time(), units = "secs")) * 1000
      max(ms, 0)
    },

    # Reactive dependency used by the flush observer.
    tick_dep = function() private$.tick(),

    # Commit the pending state to the history stack.
    flush = function() {
      state <- private$.pending
      private$.pending <- NULL
      if (is.null(state)) return(invisible(FALSE))

      label <- private$.pending_label
      if (is.null(label)) {
        label <- diff_label(flatten_state(self$history$current()),
                            flatten_state(state))
      }
      private$.pending_label <- NULL
      private$.hold_until <- NULL

      added <- self$history$push(state, label)
      if (isTRUE(added)) {
        private$log(sprintf("pushed '%s' (%d entries)", label, self$history$size()))
        self$sync_client()
      }
      invisible(added)
    },

    # ---- navigation ------------------------------------------------------

    undo = function() private$navigate(self$history$undo()),
    redo = function() private$navigate(self$history$redo()),
    jump = function(index) private$navigate(self$history$jump(index)),

    clear = function() {
      private$.pending <- NULL
      self$history$clear()
      self$sync_client()
      invisible(TRUE)
    },

    # ---- transactions ----------------------------------------------------

    # Label the next commit and widen the coalescing window so that every
    # change produced by a block of code lands in one history entry.
    open_step = function(label = NULL, hold_ms = NULL) {
      if (is.null(hold_ms)) hold_ms <- private$.coalesce_ms * 2
      private$.pending_label <- label
      private$.hold_until <- Sys.time() + hold_ms / 1000
      # If a state is already pending, extend its deadline too.
      if (!is.null(private$.pending) && private$.hold_until > private$.deadline) {
        private$.deadline <- private$.hold_until
        private$.tick(shiny::isolate(private$.tick()) + 1L)
      }
      invisible(TRUE)
    },

    # ---- registration ----------------------------------------------------

    track = function(id, values, fields = NULL) {
      private$.tracked[[id]] <- list(values = values, fields = fields)
      invisible(TRUE)
    },

    pause  = function() { private$.paused <- TRUE;  invisible(TRUE) },
    resume = function() { private$.paused <- FALSE; invisible(TRUE) },
    is_paused = function() private$.paused,

    # ---- client ----------------------------------------------------------

    # Push the current history to the browser so the rail and the button
    # enabled/disabled states stay in sync.
    sync_client = function() {
      private$.version(shiny::isolate(private$.version()) + 1L)
      entries <- self$history$entries()
      private$.session$sendCustomMessage("rewind:history", list(
        entries = unname(lapply(seq_len(nrow(entries)), function(i) {
          list(
            index   = entries$index[i],
            label   = entries$label[i],
            time    = format(entries$time[i], "%H:%M:%S"),
            current = entries$current[i]
          )
        })),
        canUndo = self$history$can_undo(),
        canRedo = self$history$can_redo(),
        index   = self$history$index()
      ))
      invisible(TRUE)
    }
  ),

  private = list(
    .session        = NULL,
    .inputs         = NULL,
    .exclude        = NULL,
    .coalesce_ms    = 400,
    .verbose        = FALSE,
    .tracked        = NULL,
    .pending        = NULL,
    .pending_label  = NULL,
    .deadline       = NULL,
    .hold_until     = NULL,
    .expecting      = NULL,
    .expecting_since = NULL,
    .expect_timeout = 2,
    .paused         = FALSE,
    .tick           = NULL,
    .version        = NULL,

    log = function(msg) {
      if (private$.verbose) message("[rewind] ", msg)
      invisible(NULL)
    },

    snapshot_inputs = function() {
      all <- shiny::reactiveValuesToList(private$.session$input)

      keep <- names(all)
      keep <- keep[!startsWith(keep, "rewind_")]
      keep <- keep[!startsWith(keep, ".")]

      # Action buttons carry a monotonically increasing click count. Restoring
      # one would either do nothing or spuriously re-fire downstream observers,
      # so they are never part of the state.
      keep <- keep[!vapply(
        all[keep],
        function(v) inherits(v, "shinyActionButtonValue"),
        logical(1)
      )]

      if (!is.null(private$.inputs)) keep <- intersect(keep, private$.inputs)
      if (!is.null(private$.exclude)) keep <- setdiff(keep, private$.exclude)

      out <- all[sort(keep)]
      out[!vapply(out, is.null, logical(1))]
    },

    snapshot_values = function() {
      if (length(private$.tracked) == 0L) return(list())
      lapply(private$.tracked, function(spec) {
        vals <- shiny::reactiveValuesToList(spec$values)
        if (!is.null(spec$fields)) vals <- vals[intersect(names(vals), spec$fields)]
        vals[order(names(vals))]
      })
    },

    navigate = function(state) {
      if (is.null(state)) return(invisible(FALSE))
      private$.pending <- NULL
      private$.pending_label <- NULL
      private$restore(state)
      self$sync_client()
      invisible(TRUE)
    },

    restore = function(state) {
      private$.expecting <- state
      private$.expecting_since <- Sys.time()

      # Server-side reactive values can be assigned directly.
      for (id in names(state$values)) {
        spec <- private$.tracked[[id]]
        if (is.null(spec)) next
        vals <- state$values[[id]]
        for (field in names(vals)) {
          spec$values[[field]] <- vals[[field]]
        }
      }

      # Inputs have to go through the browser, because the widget itself owns
      # the visible value. The client applies them via each input's registered
      # Shiny binding, which works for third-party inputs too.
      payload <- state$inputs
      if (length(payload)) {
        names(payload) <- vapply(names(payload), private$.session$ns, character(1))
      }
      private$.session$sendCustomMessage("rewind:restore", list(
        inputs = payload
      ))

      private$log(sprintf("restored %d input(s), %d value group(s)",
                          length(state$inputs), length(state$values)))
      invisible(TRUE)
    }
  )
)


#' Flatten a snapshot into a single named list
#'
#' Used for labelling: `list(inputs = list(a = 1), values = list(rv = list(b = 2)))`
#' becomes `list(a = 1, "rv$b" = 2)`.
#'
#' @param state A snapshot, or `NULL`.
#' @keywords internal
#' @noRd
flatten_state <- function(state) {
  if (is.null(state)) return(NULL)
  out <- state$inputs %||% list()
  for (id in names(state$values %||% list())) {
    vals <- state$values[[id]]
    for (field in names(vals)) {
      out[[paste0(id, "$", field)]] <- vals[[field]]
    }
  }
  out
}
