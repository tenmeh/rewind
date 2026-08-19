# helper-shiny-smoke.R --------------------------------------------------
#
# A check that you can use again: "did the application report an error?"
# testthat reads each helper-*.R file before the tests. This file runs in
# the TEST process. It examines the application through the AppDriver
# object. It does not run inside the application.
#
# WHY THIS FILE EXISTS
# Two obvious checks do not find a render error:
#
#   * "the browser console has no errors". Shiny catches a render error and
#     puts it in the output element. Shiny does NOT send it to the browser
#     console.
#   * "the output element is not empty". A render that fails leaves a
#     `shiny-output-error` <div>. That div holds text. The element is thus
#     not empty, and the check passes when it must fail.
#
# A render error always goes to the stderr of the application. Shiny writes
# a line there that holds the word "Error". shinytest2 keeps that line in
# `app$get_logs()`, with `location == "shiny"`.
#
# This file examines three places: the stderr, the browser console, and the
# DOM. It collects each problem into one character vector. One assertion
# then reports all of them together. A test failure thus shows every
# problem at the same time, and not only the first one.

#' Collect each error that the running application reports.
#'
#' @param app A live `shinytest2::AppDriver`.
#' @return A character vector. It holds one line for each problem. The
#'   vector is empty when the application reported no error.
collect_shiny_problems <- function(app) {
  problems <- character(0)

  logs <- as.data.frame(app$get_logs())
  has_logs <- nrow(logs) > 0 && all(c("location", "message") %in% names(logs))

  if (has_logs) {
    # The stderr of the application. A render error reaches this place even
    # when it never reaches the browser console.
    from_r <- logs$message[logs$location == "shiny" &
                             grepl("Error", logs$message, fixed = TRUE)]
    if (length(from_r) > 0) {
      problems <- c(problems, paste("R stderr:", from_r))
    }

    # The browser console. These are failures in the JS code.
    if ("level" %in% names(logs)) {
      is_error <- !is.na(logs$level) & logs$level == "error"
      from_browser <- logs$message[is_error & logs$location == "chromote"]
      if (length(from_browser) > 0) {
        problems <- c(problems, paste("Browser console:", from_browser))
      }
    }
  }

  # The DOM. No output must have the `shiny-output-error` class.
  # get_html() gives NULL when it finds no such element.
  in_dom <- tryCatch(app$get_html(".shiny-output-error"),
                     error = function(e) NULL)
  if (!is.null(in_dom)) {
    problems <- c(problems, paste("Output element in an error state:", in_dom))
  }

  problems
}

#' Make sure that the running application reported no error.
#'
#' Call this AFTER the test uses the controls. A render error occurs only
#' when its reactive runs.
#'
#' @param app A live `shinytest2::AppDriver`.
#' @return The application, invisibly.
expect_no_shiny_errors <- function(app) {
  problems <- collect_shiny_problems(app)
  testthat::expect_equal(
    length(problems), 0L,
    info = paste0("The application reported these problems:\n",
                  paste(problems, collapse = "\n"))
  )
  invisible(app)
}

#' Start an AppDriver for `app_dir`. Skip the test, and do not fail it, when
#' this computer has no headless Chrome.
#'
#' Chrome writes its own work files while it runs. On Linux these are lock
#' files with names such as `com.google.Chrome.*`. On each system Chrome
#' also writes its user-data directory. Chrome puts these files in
#' `TMPDIR`, `TMP` or `TEMP`. It gets those values from the R process that
#' starts it.
#'
#' R CMD check sets those values to a directory inside the tree that it
#' examines later. That examination is the check for "detritus in the temp
#' directory". Each file that Chrome leaves thus causes a NOTE. This has no
#' relation to rewind. It is the normal operation of Chrome.
#'
#' A directory next to `tempdir()` does not solve this. That directory was
#' inside the examined tree on each system that was tried.
#' `tools::R_user_dir()` and `Sys.getenv("RUNNER_TEMP")` have no relation to
#' the position of `tempdir()`. GitHub Actions gives `RUNNER_TEMP` for this
#' purpose. This function thus sends Chrome to one of those directories
#' while the browser runs. The position of the temporary directory on each
#' system is then not important.
#'
#' The function removes the directory at the end. A failure to remove it has
#' no effect on the check. Chrome can hold a file open for a short time
#' after it stops.
#'
#' @param app_dir The path to the Shiny application.
#' @param ... More arguments for `shinytest2::AppDriver$new()`.
#' @return A live `AppDriver`.
local_app_driver <- function(app_dir, ...) {
  testthat::skip_if_not_installed("shinytest2")

  runner_tmp <- Sys.getenv("RUNNER_TEMP", unset = "")
  chrome_root <- if (nzchar(runner_tmp)) runner_tmp else tools::R_user_dir("rewind", "cache")
  chrome_tmp <- file.path(chrome_root, paste0("chromote-", Sys.getpid()))
  dir.create(chrome_tmp, showWarnings = FALSE, recursive = TRUE)
  withr::local_envvar(
    list(TMPDIR = chrome_tmp, TMP = chrome_tmp, TEMP = chrome_tmp),
    .local_envir = parent.frame()
  )

  app <- tryCatch(
    shinytest2::AppDriver$new(app_dir, ...),
    error = function(e) {
      unlink(chrome_tmp, recursive = TRUE)
      testthat::skip(paste("Could not start a headless browser:", conditionMessage(e)))
    }
  )
  withr::defer(
    {
      app$stop()
      unlink(chrome_tmp, recursive = TRUE)
    },
    envir = parent.frame()
  )
  app
}
