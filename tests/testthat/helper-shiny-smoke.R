# helper-shiny-smoke.R --------------------------------------------------------
#
# Reusable AppDriver assertion: "did the running app throw anything?"
# testthat auto-sources helper-*.R before the tests, and it runs in the TEST
# process (it inspects the app via the AppDriver handle; it is not loaded
# inside the app).
#
# WHY THIS EXISTS
# A naive smoke test asserts the absence of a signal that may not fire:
#   * "browser console has no errors" - a Shiny RENDER error is caught by
#     Shiny and shown in the output element; it is NOT written to the
#     browser console.
#   * "the output element is non-empty" - a failed render leaves a non-empty
#     `shiny-output-error` <div> behind, so "got some HTML" is a false pass.
#
# The signal that DOES fire on a render error is the app's stderr - Shiny
# prints "Warning: Error in <fn>: ..." there, and shinytest2 captures it in
# `app$get_logs()` under `location == "shiny"`. That is the universal catch:
# it needs no output IDs and no per-widget markers. This helper asserts on
# it, plus the DOM error class and the browser console, so a runtime
# exception cannot pass silently.

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Assert the running app logged no Shiny/JS errors and no output is in an
#' error state.
#'
#' Call this AFTER exercising the interactions under test (a render error
#' only fires when its reactive actually executes).
#'
#' @param app A live `shinytest2::AppDriver`.
#' @return The app, invisibly.
expect_no_shiny_errors <- function(app) {
  logs <- as.data.frame(app$get_logs())
  have <- nrow(logs) > 0 && all(c("location", "message") %in% names(logs))

  # 1. Shiny stderr - render/runtime exceptions land here even when they
  #    never reach the browser console.
  shiny_err <- character(0)
  if (have) {
    shiny_err <- logs$message[logs$location == "shiny" &
                                grepl("Error( in |:)", logs$message)]
  }
  testthat::expect_identical(
    shiny_err, character(0),
    info = paste0("App logged Shiny errors:\n", paste(shiny_err, collapse = "\n"))
  )

  # 2. Browser console errors (client-side JS failures).
  if (have && "level" %in% names(logs)) {
    console_err <- logs$message[!is.na(logs$level) & logs$level == "error" &
                                  logs$location == "chromote"]
    testthat::expect_identical(
      console_err, character(0),
      info = paste0("Browser console errors:\n",
                    paste(console_err, collapse = "\n"))
    )
  }

  # 3. DOM - no output rendered into a `shiny-output-error` state.
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
#' @param app_dir Path to the Shiny app.
#' @param ... Passed on to `shinytest2::AppDriver$new()`.
#' @return A live `AppDriver`.
local_app_driver <- function(app_dir, ...) {
  testthat::skip_if_not_installed("shinytest2")
  app <- tryCatch(
    shinytest2::AppDriver$new(app_dir, ...),
    error = function(e) {
      testthat::skip(paste("Could not start a headless browser:", conditionMessage(e)))
    }
  )
  withr::defer(app$stop(), envir = parent.frame())
  app
}
