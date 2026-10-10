# test-restorable.R ------------------------------------------------------
#
# rewind must record only inputs that the page can put back.
#
# An undo restores an input through the binding of its element in the page
# (applyValue() in rewind.js). Some inputs have no such element: a plot's
# hover, click and brush, and the values that DT keeps, are sent with
# Shiny.setInputValue() alone. rewind used to record them anyway. Each move
# of the mouse over a plot was then a step that an undo could not restore,
# so the user pressed Undo and nothing happened.
#
# The browser now reports the ids of the inputs that have a binding, and by
# default rewind records only those. This needs a real page, so the test
# drives a headless browser.
#
# Refer to local_app_driver() in helper-shiny-smoke.R: install the package
# before a local run.

restorable_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      rewind_buttons(),
      rewind_ui(),
      shiny::selectInput("region", "Region", c("North", "South")),
      shiny::plotOutput("p", height = "300px",
                        hover = shiny::hoverOpts("hov", delay = 50))
    ),
    server = function(input, output, session) {
      rewind_enable()
      output$p <- shiny::renderPlot(plot(1:10))
    }
  )
}

settle_restorable <- function(app) {
  Sys.sleep(1)
  app$wait_for_idle(timeout = 10000L)
  invisible(app)
}

test_that("moving the mouse over a plot is not a step, so one undo undoes the real change", {
  testthat::skip_on_cran()

  app <- local_app_driver(restorable_app(), name = "restorable", width = 1200, height = 900)
  settle_restorable(app)

  app$set_inputs(region = "South")
  settle_restorable(app)

  # Three separate passes of the mouse over the plot.
  b <- app$get_chromote_session()
  r <- app$get_js(
    "(function () { var e = document.getElementById('p').getBoundingClientRect(); return [e.left, e.top]; })()"
  )
  for (k in 1:3) {
    for (i in 1:6) {
      b$Input$dispatchMouseEvent(type = "mouseMoved",
                                 x = r[[1]] + 40 + 40 * i + 15 * k, y = r[[2]] + 80 + 20 * k)
      Sys.sleep(0.08)
    }
    Sys.sleep(0.8)
  }
  settle_restorable(app)

  # Guard: the hover really reached the server, or the test proves nothing.
  expect_false(is.null(app$get_value(input = "hov")))

  labels <- unlist(app$get_js(
    "Array.from(document.querySelectorAll('.rewind-step-label')).map(e => e.textContent)"
  ))
  expect_false("hov" %in% labels)
  expect_equal(length(labels), 2L)

  app$click(selector = ".rewind-undo")
  settle_restorable(app)
  expect_equal(app$get_value(input = "region"), "North")

  expect_no_shiny_errors(app)
})
