# test-unrecorded.R ------------------------------------------------------
#
# rewind_unrecorded() runs a block of code whose changes are not a step.
# They become part of the current state instead, so an undo does not go
# back across them. The usual case is code that restores a saved view.
#
# rewind_pause() and rewind_resume() could not do this inside one
# observer: capture runs after the observer that made the change, and by
# then capture had resumed. The probe of 2026-10-11 recorded the change as
# a step. The first test below is that probe.
#
# Refer to the note at the top of test-controller.R about the virtual clock
# of testServer and the real clock that the grouping period uses.

settle_unrec <- function(session, ...) {
  session$setInputs(...)
  Sys.sleep(0.15)
  session$elapse(250)
}

unrecorded_server <- function(input, output, session) {
  rewind_enable(coalesce_ms = 30)
  st <- shiny::reactiveValues(zoom = 1)
  rewind_track(st, fields = "zoom")
  shiny::observeEvent(input$restore, {
    rewind_unrecorded(st$zoom <- 99)
  })
}

test_that("a change made inside rewind_unrecorded() in one observer is not a step", {
  shiny::testServer(unrecorded_server, {
    ctrl <- session$userData$.rewind
    settle_unrec(session, region = "North")
    expect_equal(ctrl$history$size(), 1L)

    settle_unrec(session, restore = 1)

    expect_equal(ctrl$history$size(), 1L)
    # The change is part of the current state.
    expect_equal(ctrl$history$current()$values$st$zoom, 99)
  })
})

test_that("an undo after a later change goes back to the unrecorded state, not before it", {
  shiny::testServer(unrecorded_server, {
    ctrl <- session$userData$.rewind
    settle_unrec(session, region = "North")
    settle_unrec(session, restore = 1)
    Sys.sleep(0.2)                       # let the window end
    session$elapse(250)
    settle_unrec(session, region = "South")
    expect_equal(ctrl$history$size(), 2L)

    ctrl$undo()
    expect_equal(ctrl$history$current()$inputs$region, "North")
    expect_equal(ctrl$history$current()$values$st$zoom, 99)
    expect_equal(st$zoom, 99)
  })
})

test_that("a change from the user that still waits becomes its own step first", {
  shiny::testServer(unrecorded_server, {
    ctrl <- session$userData$.rewind
    settle_unrec(session, region = "North")

    # The user changes region, and before that step is written, code
    # restores a view. The user's change must not merge into the view.
    session$setInputs(region = "South", restore = 1)
    Sys.sleep(0.15)
    session$elapse(250)

    expect_equal(ctrl$history$size(), 2L)
    ctrl$undo()
    expect_equal(ctrl$history$current()$inputs$region, "North")
  })
})

test_that("the steps in front of the position go, as for any new change", {
  shiny::testServer(unrecorded_server, {
    ctrl <- session$userData$.rewind
    settle_unrec(session, region = "North")
    settle_unrec(session, region = "South")
    ctrl$undo()
    expect_true(ctrl$history$can_redo())

    Sys.sleep(2.1)                       # let the echo window of the undo end
    session$elapse(2200)
    settle_unrec(session, restore = 1)
    expect_false(ctrl$history$can_redo())
  })
})

test_that("rewind_unrecorded runs the code when rewind is not enabled", {
  shiny::testServer(function(input, output, session) {}, {
    expect_equal(rewind_unrecorded(1 + 1), 2)
  })
})

test_that("rewind_unrecorded checks hold_ms", {
  shiny::testServer(function(input, output, session) rewind_enable(), {
    expect_error(rewind_unrecorded(NULL, hold_ms = -1),
                 "`hold_ms` must be a single finite number of 0 or more.", fixed = TRUE)
  })
})


# --- In a real browser ------------------------------------------------------
#
# The main use is update*Input(), and those changes come back from the
# browser after the block has run. Only a real page shows that the window
# covers them. Refer to local_app_driver() in helper-shiny-smoke.R: install
# the package before a local run.

test_that("a view loaded with update*Input() is not a step, and an undo keeps it", {
  testthat::skip_on_cran()

  app <- local_app_driver(shiny::shinyApp(
    ui = shiny::fluidPage(
      rewind_buttons(),
      rewind_ui(),
      shiny::selectInput("region", "Region", c("North", "South", "East")),
      shiny::selectInput("year", "Year", c("2020", "2024")),
      shiny::actionButton("load_view", "Load view")
    ),
    server = function(input, output, session) {
      rewind_enable(coalesce_ms = 200)
      shiny::observeEvent(input$load_view, {
        rewind_unrecorded(shiny::updateSelectInput(session, "region", selected = "East"))
      })
    }
  ), name = "unrecorded")

  settle <- function() {
    Sys.sleep(1)
    app$wait_for_idle(timeout = 10000L)
  }
  n_steps <- function() as.integer(app$get_js("document.querySelectorAll('.rewind-step').length"))

  settle()
  app$click(input = "load_view")
  settle()
  expect_equal(app$get_value(input = "region"), "East")
  expect_equal(n_steps(), 1L)       # still only the first entry

  app$set_inputs(year = "2024")
  settle()
  expect_equal(n_steps(), 2L)

  app$click(selector = ".rewind-undo")
  settle()
  expect_equal(app$get_value(input = "year"), "2020")
  # The loaded view stays: the undo does not go back across it.
  expect_equal(app$get_value(input = "region"), "East")

  expect_no_shiny_errors(app)
})
