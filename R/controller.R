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
                          restore_timeout = 2,
                          verbose = FALSE) {
      private$.session     <- session
      private$.inputs      <- inputs
      private$.exclude     <- exclude
      private$.coalesce_ms <- as.numeric(coalesce_ms)
      private$.expect_timeout <- as.numeric(restore_timeout)
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
        #
        # The time limit matters on a slow connection. Too short, and
        # capture starts again while the browser is still applying the
        # restore, so a partial state becomes a history entry that the user
        # never made. Too long, and a restore that never echoes holds
        # capture off, so real changes from the user are lost until the
        # limit ends. rewind_enable(restore_timeout=) sets it.
        if (difftime(Sys.time(), private$.expecting_since, units = "secs") <
              private$.expect_timeout) {
          private$log("ignoring intermediate state while restoring")
          return(invisible(FALSE))
        }
        private$log("restore echo timed out; resuming capture")
        private$.expecting <- NULL
      }

      # Nothing that is captured has changed. Do not start the grouping
      # period again. The capture observer reads every input, so it also
      # runs when an input that is not captured changes. A plot hover fires
      # every 0.1 s, and moving the deadline each time held every step back
      # until the mouse stopped, and merged the changes in between.
      same_as <- if (!is.null(private$.pending)) private$.pending else self$history$current()
      if (!is.null(same_as) && states_equal(state, same_as)) {
        return(invisible(FALSE))
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

    # ---- secrets ---------------------------------------------------------

    # Never keep these inputs in the history. The browser sends the ids of
    # its password fields (refer to reportSecrets() in rewind.js), because
    # the server cannot tell a password from any other string.
    #
    # The ids from the browser are the full ids in the page. Inside a module
    # the snapshot uses the names local to the module, so keep only the ids
    # under this session's namespace, and take the prefix off.
    #
    # The list only grows. A field that was a password once stays out, even
    # after it leaves the page.
    set_secret = function(ids) {
      ids <- private$local_ids(ids)
      new <- setdiff(ids, private$.secret)
      if (length(new) == 0L) return(invisible(FALSE))
      private$.secret <- union(private$.secret, new)

      # The first snapshot can be taken before the browser reports, so the
      # value may be stored already.
      private$drop_inputs(new)
      invisible(TRUE)
    },

    secret = function() private$.secret,

    # ---- restorable inputs -----------------------------------------------

    # The ids of the inputs that this page can put back. The browser sends
    # them (refer to reportBound() in rewind.js): the inputs whose element
    # has a Shiny binding. A plot's hover and the values that DT keeps have
    # no such element, so an undo cannot restore them. By default rewind
    # records only the inputs in this list.
    #
    # The list only grows, like the list of secrets, so an input that
    # leaves the page does not change the history. Until the first report
    # arrives, and in shiny::testServer(), which has no browser, there is
    # no list, and rewind records every input as it did before 0.4.0.
    set_bound = function(ids) {
      ids <- private$local_ids(ids)
      first <- is.null(private$.bound)
      new <- setdiff(ids, private$.bound)
      if (!first && length(new) == 0L) return(invisible(FALSE))
      private$.bound <- union(private$.bound, ids)

      # Only the default capture uses the list. An app that names its
      # inputs gets those inputs, whether the page can restore them or not.
      if (is.null(private$.inputs)) {
        private$drop_inputs(setdiff(private$held_inputs(), private$.bound))
      }
      invisible(TRUE)
    },

    bound = function() private$.bound,

    # ---- exclusions --------------------------------------------------------

    # Add inputs to `exclude`, from rewind_exclude(). The ids are the full
    # ids in the page, so they go through the same mapping as the reports
    # from the browser. A value captured before the call is removed, so the
    # history stays the same with or without it.
    add_exclude = function(ids) {
      ids <- private$local_ids(ids)
      new <- setdiff(ids, private$.exclude)
      if (length(new) == 0L) return(invisible(FALSE))
      private$.exclude <- union(private$.exclude, new)
      private$drop_inputs(new)
      invisible(TRUE)
    },

    # The settings that decide what is captured, for rewind_status().
    capture_settings = function() {
      list(inputs = private$.inputs, exclude = private$.exclude,
           secret = private$.secret, bound = private$.bound,
           tracked = lapply(private$.tracked, function(spec) spec$fields))
    },

    # Why each input is, or is not, captured. Refer to classify_inputs().
    input_reasons = function() {
      private$classify_inputs(shiny::isolate(
        shiny::reactiveValuesToList(private$.session$input)
      ))
    },

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
            # Send the moment, as milliseconds since the epoch, and not a
            # formatted time. The browser formats it in the viewer's own
            # time zone. Text made here would be in the time zone of the
            # server, which is usually UTC for a deployed application.
            time    = as.numeric(entries$time[i]) * 1000,
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
    .secret         = character(0),
    .bound          = NULL,
    .tick           = NULL,
    .version        = NULL,
    .observers      = list(),

    log = function(msg) {
      if (private$.verbose) message("[rewind] ", msg)
      invisible(NULL)
    },

    snapshot_inputs = function() {
      all <- shiny::reactiveValuesToList(private$.session$input)
      reasons <- private$classify_inputs(all)
      keep <- names(reasons)[is.na(reasons)]
      all[sort(keep)]
    },

    # Give each input a reason why it is not captured, or NA when it is.
    # snapshot_inputs() keeps the NAs. rewind_status() shows the reasons, so
    # a user can find out why an undo skips an input. The first reason that
    # applies wins, and the order is from the most fixed to the most
    # changeable.
    classify_inputs = function(all) {
      nms <- names(all)
      reason <- rep(NA_character_, length(nms))
      names(reason) <- nms
      set <- function(hit, why) reason[is.na(reason) & hit] <<- why

      set(startsWith(nms, "."), "internal to Shiny")
      set(startsWith(nms, "rewind_"), "part of rewind")

      # The value of an action button is a click counter. The counter only
      # increases. A restore of that value does nothing, or it starts the
      # observers again by mistake.
      set(vapply(all, function(v) inherits(v, "shinyActionButtonValue"), logical(1)),
          "action button")

      # The value of a fileInput() is a data frame. It points to a temporary
      # file on the server (refer to ?shiny::fileInput). Shiny deletes that
      # file at the next upload. An old snapshot would thus set input$file
      # to a path that does not exist.
      set(vapply(all, is_file_input_value, logical(1)), "file upload")

      # A password field is never kept, even if `inputs` names it. Refer to
      # set_secret().
      set(nms %in% private$.secret, "password field")
      set(nms %in% private$.exclude, "excluded")

      if (!is.null(private$.inputs)) {
        set(!nms %in% private$.inputs, "not in `inputs`")
      } else if (!is.null(private$.bound)) {
        # Refer to set_bound().
        set(!nms %in% private$.bound, "cannot be restored by the page")
      }

      # A NULL value is not kept, so an undo to it does not clear the input.
      set(vapply(all, is.null, logical(1)), "NULL value")
      reason
    },

    # The ids from the browser are the full ids in the page. Inside a module
    # the snapshot uses the names local to the module, so keep only the ids
    # under this session's namespace, and take the prefix off.
    #
    # Only a module has a namespace to take off. Do not test ns("") for
    # that: at the top level a real session gives "", but the session of
    # shiny::testServer() gives "mock-session-", which would drop every id.
    local_ids = function(ids) {
      ids <- as.character(unlist(ids))
      root <- private$.session$rootScope()
      prefix <- if (identical(private$.session, root)) "" else private$.session$ns("")
      if (nzchar(prefix)) {
        ids <- ids[startsWith(ids, prefix)]
        ids <- substring(ids, nchar(prefix) + 1L)
      }
      unique(ids)
    },

    # Every input name that a stored state holds.
    held_inputs = function() {
      states <- c(
        lapply(seq_len(self$history$size()), self$history$state_at),
        list(private$.pending, private$.expecting)
      )
      unique(unlist(lapply(states, function(s) names(s$inputs))))
    },

    # Remove inputs from everything that holds a state, then tell the
    # browser. The history removes a step whose only change was one of
    # these inputs (refer to History$scrub_inputs()).
    drop_inputs = function(ids) {
      if (length(ids) == 0L) return(invisible(FALSE))
      self$history$scrub_inputs(ids)
      if (!is.null(private$.pending)) private$.pending$inputs[ids] <- NULL
      if (!is.null(private$.expecting)) private$.expecting$inputs[ids] <- NULL
      self$sync_client()
      invisible(TRUE)
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
#' examines the shape of the value.
#'
#' The column types are examined as well as the names. A data frame of the
#' user, with those four names but other types, is then kept in the
#' history. Without the type test, `rewind` would drop such a value and
#' give no message, and an undo would not restore it.
#'
#' @param x An input value to examine.
#' @keywords internal
#' @noRd
is_file_input_value <- function(x) {
  is.data.frame(x) &&
    identical(names(x), c("name", "size", "type", "datapath")) &&
    is.character(x$name) &&
    is.character(x$type) &&
    is.character(x$datapath) &&
    is.numeric(x$size)
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
