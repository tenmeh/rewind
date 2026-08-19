#' The per-session rewind controller
#'
#' This class owns the history stack, the capture and grouping code, and the
#' restore code. [rewind_enable()] makes one instance for each Shiny session.
#' It keeps that instance in `session$userData$.rewind`.
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

    # A reactive dependency. It becomes invalid at each change of the
    # history. [rewind_history()] and the related functions can thus run
    # inside `render*()`.
    version_dep = function() private$.version(),

    # @description
    # Keep the observers that [rewind_enable()] made. [rewind_disable()]
    # then has objects to destroy. Call this once, immediately after you
    # make the observers.
    # @param observers A list of observer objects. Each object must have a
    #   `destroy()` method. `shiny::observe()` and `shiny::observeEvent()`
    #   give such objects.
    set_observers = function(observers) {
      private$.observers <- observers
      invisible(self)
    },

    # @description
    # Stop capture and destroy each observer of this session. Then tell the
    # browser to clear the buttons and the rail. This method keeps the
    # history. The caller can thus read it after the method runs. The method
    # removes only the reactive connections and the data in the client.
    destroy = function() {
      for (obs in private$.observers) obs$destroy()
      private$.observers <- list()
      private$.session$sendCustomMessage("rewind:history", list(
        entries = list(), canUndo = FALSE, canRedo = FALSE, index = 0
      ))
      invisible(self)
    },

    # ---- capture ---------------------------------------------------------

    # Read the current state of each tracked value. Call this from a
    # reactive context. This is what makes the capture automatic.
    snapshot = function() {
      list(
        inputs = private$snapshot_inputs(),
        values = private$snapshot_values()
      )
    },

    # Keep a possible new state. Then start the grouping period again.
    note = function(state) {
      if (private$.paused) return(invisible(FALSE))

      # Is this the echo of the restore that we just did?
      if (!is.null(private$.expecting)) {
        if (states_equal(state, private$.expecting)) {
          private$.expecting <- NULL
          private$log("echo absorbed")
          return(invisible(FALSE))
        }
        # The browser has not applied all of the restore yet. Ignore the
        # states between, until it is complete or the time limit ends.
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

    # The time in milliseconds until `rewind` must write the state that
    # waits. The result is `Inf` when no state waits.
    time_to_flush = function() {
      if (is.null(private$.pending)) return(Inf)
      ms <- as.numeric(difftime(private$.deadline, Sys.time(), units = "secs")) * 1000
      max(ms, 0)
    },

    # The reactive dependency that the flush observer uses.
    tick_dep = function() private$.tick(),

    # Write the state that waits to the history stack.
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

    # Set the label for the next write, and increase the grouping period.
    # Each change from a block of code thus goes into one history entry.
    open_step = function(label = NULL, hold_ms = NULL) {
      if (is.null(hold_ms)) hold_ms <- private$.coalesce_ms * 2
      private$.pending_label <- label
      private$.hold_until <- Sys.time() + hold_ms / 1000
      # If a state waits already, increase its time limit also.
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

    pause = function() {
      private$.paused <- TRUE
      invisible(TRUE)
    },

    resume = function() {
      private$.paused <- FALSE
      invisible(TRUE)
    },

    is_paused = function() private$.paused,

    # ---- client ----------------------------------------------------------

    # Send the current history to the browser. The rail and the buttons thus
    # show the correct condition.
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
    .observers      = list(),

    log = function(msg) {
      if (private$.verbose) message("[rewind] ", msg)
      invisible(NULL)
    },

    snapshot_inputs = function() {
      all <- shiny::reactiveValuesToList(private$.session$input)

      keep <- names(all)
      keep <- keep[!startsWith(keep, "rewind_")]
      keep <- keep[!startsWith(keep, ".")]

      # The value of an action button is a click counter. The counter only
      # increases. A restore of that value does nothing, or it starts the
      # observers again by mistake. The state thus never holds these values.
      keep <- keep[!vapply(
        all[keep],
        function(v) inherits(v, "shinyActionButtonValue"),
        logical(1)
      )]

      # The value of a fileInput() is a data frame. It points to a temporary
      # file on the server (refer to ?shiny::fileInput). Shiny deletes that
      # file at the next upload. An old snapshot would thus set input$file to
      # a path that does not exist.
      keep <- keep[!vapply(all[keep], is_file_input_value, logical(1))]

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

      # You can set the reactive values on the server directly.
      for (id in names(state$values)) {
        spec <- private$.tracked[[id]]
        if (is.null(spec)) next
        vals <- state$values[[id]]
        for (field in names(vals)) {
          spec$values[[field]] <- vals[[field]]
        }
      }

      # Inputs must go through the browser. The widget owns the value that
      # the user sees. The client applies each value with the Shiny binding
      # of that input. This method also works for inputs from other
      # packages.
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


#' Is this the value of a `fileInput()`?
#'
#' The server value of a `fileInput()` is a data frame. It has a fixed set
#' of columns: `name`, `size`, `type` and `datapath` (refer to
#' [shiny::fileInput]). It has no special S3 class. This function thus
#' compares the column names.
#'
#' @param x An input value to examine.
#' @keywords internal
#' @noRd
is_file_input_value <- function(x) {
  is.data.frame(x) && identical(names(x), c("name", "size", "type", "datapath"))
}


#' Flatten a snapshot into a single named list
#'
#' `rewind` uses this for the labels. It changes
#' `list(inputs = list(a = 1), values = list(rv = list(b = 2)))` into
#' `list(a = 1, "rv$b" = 2)`.
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
