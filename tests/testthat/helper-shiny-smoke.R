# helper-shiny-smoke.R --------------------------------------------------------
#
# An AppDriver check that you can use again: "did the application throw an
# error?" testthat reads each helper-*.R file before the tests. This file
# runs in the TEST process. It examines the application through the
# AppDriver object. It does not run inside the application.
#
# WHY THIS FILE EXISTS
# A simple smoke test looks for the absence of a signal. But that signal
# does not always occur:
#   * "the browser console has no errors". Shiny catches a RENDER error and
#     shows it in the output element. Shiny does NOT write it to the browser
#     console.
#   * "the output element is not empty". A render that fails leaves a
#     `shiny-output-error` <div>. That div is not empty. "The element has
#     some HTML" is thus a false pass.
#
# One signal always occurs at a render error: the stderr of the application.
# Shiny prints "Warning: Error in <fn>: ..." there. shinytest2 keeps that
# text in `app$get_logs()`, with `location == "shiny"`. This is the check
# that finds each error. It needs no output IDs and no markers for each
# widget.
#
# This helper examines the stderr, the error class in the DOM, and the
# browser console. An error at run time thus cannot pass without a failure.

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Make sure that the application logged no Shiny error and no JS error.
#' Make sure also that no output shows an error.
#'
#' Call this AFTER the test uses the controls. A render error occurs only
#' when its reactive runs.
#'
#' @param app A live `shinytest2::AppDriver`.
#' @return The application, invisibly.
expect_no_shiny_errors <- function(app) {
  logs <- as.data.frame(app$get_logs())
  have <- nrow(logs) > 0 && all(c("location", "message") %in% names(logs))

  # 1. The stderr of Shiny. Errors at render time and at run time go here,
  #    also when they never reach the browser console.
  shiny_err <- character(0)
  if (have) {
    shiny_err <- logs$message[logs$location == "shiny" &
                                grepl("Error( in |:)", logs$message)]
  }
  testthat::expect_identical(
    shiny_err, character(0),
    info = paste0("App logged Shiny errors:\n", paste(shiny_err, collapse = "\n"))
  )

  # 2. Errors in the browser console. These are failures in the JS code.
  if (have && "level" %in% names(logs)) {
    console_err <- logs$message[!is.na(logs$level) & logs$level == "error" &
                                  logs$location == "chromote"]
    testthat::expect_identical(
      console_err, character(0),
      info = paste0("Browser console errors:\n",
                    paste(console_err, collapse = "\n"))
    )
  }

  # 3. The DOM. No output must have the `shiny-output-error` class.
  dom_err <- tryCatch(app$get_html(".shiny-output-error"),
                      error = function(e) NULL)
  testthat::expect_null(
    dom_err,
    info = paste0("An output is in an error state:\n", dom_err %||% "")
  )

  invisible(app)
}

#' Start an AppDriver for `app_dir`, skipping (not failing) if a headless
#' Chrome is not available in this environment.
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
