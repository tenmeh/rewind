# test-rail-time.R -------------------------------------------------------
#
# The time on each rail step must be the local time of the person who
# looks at it.
#
# The server used to format the time, in UTC, and send the text. A viewer
# in India saw 08:03 at 13:33. The server's own time zone would not be
# correct either: a deployed application usually runs in UTC, and its
# users are anywhere. Only the browser knows the viewer's time zone.
#
# This test moves the browser to Pacific/Chatham, which is UTC+12:45 or
# UTC+13:45. Its clock can never show the same hour and minute as UTC, so
# a time made on the server cannot pass by chance.
#
# Refer to local_app_driver() in helper-shiny-smoke.R: install the package
# before a local run.

fixture_time_coalesce_ms <- 150

rail_time_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::selectInput("pick", "Pick", c("a", "b", "c")),
      rewind_ui()
    ),
    server = function(input, output, session) {
      rewind_enable(coalesce_ms = fixture_time_coalesce_ms)
    }
  )
}

settle_time <- function(app) {
  Sys.sleep((fixture_time_coalesce_ms + 350) / 1000)
  app$wait_for_idle(timeout = 10000L)
  invisible(app)
}


test_that("the rail shows the time in the viewer's own time zone", {
  testthat::skip_on_cran()

  app <- local_app_driver(rail_time_app(), name = "rail-time")
  app$get_chromote_session()$Emulation$setTimezoneOverride(
    timezoneId = "Pacific/Chatham"
  )

  # A change after the override, so the rail draws the step in the new zone.
  settle_time(app)
  app$set_inputs(pick = "b")
  settle_time(app)

  shown <- app$get_js(
    "document.querySelector('.rewind-step.is-current .rewind-step-time').textContent"
  )

  # The format stays HH:MM:SS, as before.
  expect_match(shown, "^[0-2][0-9]:[0-5][0-9]:[0-5][0-9]$")

  # Allow for a minute that turns over while the test runs.
  now <- Sys.time() + c(-60, 0, 60)
  chatham <- format(now, "%H:%M", tz = "Pacific/Chatham")
  utc <- format(now, "%H:%M", tz = "UTC")

  expect_true(substr(shown, 1, 5) %in% chatham)
  expect_false(substr(shown, 1, 5) %in% utc)

  expect_no_shiny_errors(app)
})
