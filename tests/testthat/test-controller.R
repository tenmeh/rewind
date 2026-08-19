# Integration tests for the controller. They use real Shiny sessions through
# shiny::testServer(). test-history.R and test-state.R examine the R6 class
# and the helper functions directly. These tests are different. They examine
# the reactive connections that rewind_enable() makes: capture, grouping,
# echo control, and restore.
#
# A note about time. The grouping period uses Sys.time() (refer to
# controller.R). This is correct, and necessary in a real application. But
# session$elapse() in shiny::testServer() moves a private virtual clock.
# That clock is separate from Sys.time(). elapse() alone thus never moves
# the real clock that rewind waits for.
#
# Each test uses a short Sys.sleep() to move the real clock. The sleep is
# longer than the coalesce_ms of the test. The test then calls elapse() to
# run the flush. This is an effect of testServer and Sys.time() together. It
# is not a defect. It changes only how the tests observe the package. It
# does not change the package in production.

srv <- function(input, output, session) {
  rewind_enable(coalesce_ms = 30)
  state <- shiny::reactiveValues(zoom = 1)
  rewind_track(state, fields = "zoom", id = "st")
}

settle <- function(session, ...) {
  session$setInputs(...)
  Sys.sleep(0.15)
  session$elapse(200)
}

test_that("the first captured state becomes 'Initial state'", {
  shiny::testServer(srv, {
    ctrl <- session$userData$.rewind
    settle(session, region = "North")

    expect_equal(ctrl$history$size(), 1L)
    expect_equal(ctrl$history$entries()$label, "Initial state")
    expect_equal(ctrl$history$current()$inputs$region, "North")
  })
})

test_that("a later change becomes its own entry", {
  shiny::testServer(srv, {
    ctrl <- session$userData$.rewind
    settle(session, region = "North")
    settle(session, region = "South")

    expect_equal(ctrl$history$size(), 2L)
    expect_equal(ctrl$history$entries()$label[2], "region")
    expect_equal(ctrl$history$current()$inputs$region, "South")
  })
})

test_that("rapid changes within the coalescing window collapse into one entry", {
  shiny::testServer(srv, {
    ctrl <- session$userData$.rewind
    settle(session, region = "Baseline")
    expect_equal(ctrl$history$size(), 1L)

    session$setInputs(year = 2020)
    session$setInputs(year = 2021)
    session$setInputs(year = 2022)
    Sys.sleep(0.15)
    session$elapse(200)

    expect_equal(ctrl$history$size(), 2L)
    expect_equal(ctrl$history$current()$inputs$year, 2022)
  })
})

test_that("action buttons never enter a snapshot", {
  shiny::testServer(srv, {
    ctrl <- session$userData$.rewind
    settle(session,
      region = "North",
      go = structure(1L, class = c("shinyActionButtonValue", "integer"))
    )

    captured <- ctrl$history$current()$inputs
    expect_true("region" %in% names(captured))
    expect_false("go" %in% names(captured))
  })
})

test_that("fileInput values never enter a snapshot", {
  shiny::testServer(srv, {
    ctrl <- session$userData$.rewind
    settle(session,
      region = "North",
      upload = data.frame(
        name = "a.csv", size = 12, type = "text/csv",
        datapath = "/tmp/0x1.csv", stringsAsFactors = FALSE
      )
    )

    captured <- ctrl$history$current()$inputs
    expect_true("region" %in% names(captured))
    expect_false("upload" %in% names(captured))
  })
})

test_that("undo restores the value and the browser's echo is not a new entry", {
  shiny::testServer(srv, {
    ctrl <- session$userData$.rewind
    settle(session, region = "North")
    settle(session, region = "South")
    expect_equal(ctrl$history$size(), 2L)

    ctrl$undo()
    expect_equal(ctrl$history$current()$inputs$region, "North")

    # Do what the browser does after it applies rewind:restore. rewind.js
    # sends the restored value back. This test thus calls note() with the
    # state that undo() moved to.
    restored <- ctrl$history$current()
    ctrl$note(restored)
    Sys.sleep(0.15)
    session$elapse(200)

    expect_equal(ctrl$history$size(), 2L)
    expect_true(ctrl$history$can_redo())
  })
})

test_that("rewind_track round-trips server-side reactiveValues through undo", {
  shiny::testServer(srv, {
    ctrl <- session$userData$.rewind
    settle(session, region = "Baseline")
    expect_equal(ctrl$history$current()$values$st$zoom, 1)

    # An assignment to reactiveValues does not start a reactive flush in
    # testServer. session$setInputs() does. This test thus calls
    # flushReact() to let the capture observer see the change.
    state$zoom <- 2
    session$flushReact()
    Sys.sleep(0.15)
    session$elapse(200)
    expect_equal(ctrl$history$size(), 2L)
    expect_equal(ctrl$history$current()$values$st$zoom, 2)

    ctrl$undo()
    expect_equal(ctrl$history$current()$values$st$zoom, 1)
    expect_equal(state$zoom, 1)

    # The assignment inside restore() is also a change to a tracked value.
    # Make sure that rewind absorbs its echo, and does not make a third
    # entry.
    session$flushReact()
    Sys.sleep(0.15)
    session$elapse(200)
    expect_equal(ctrl$history$size(), 2L)
  })
})

test_that("rewind_step groups several changes into one labelled entry", {
  shiny::testServer(srv, {
    ctrl <- session$userData$.rewind
    settle(session, region = "Baseline")
    expect_equal(ctrl$history$size(), 1L)

    rewind_step(label = "Reset filters", {
      session$setInputs(region = "East", year = 2025)
    })
    Sys.sleep(0.2)
    session$elapse(300)

    expect_equal(ctrl$history$size(), 2L)
    expect_equal(ctrl$history$entries()$label[2], "Reset filters")
  })
})

test_that("pause suspends capture entirely, including the very first entry", {
  shiny::testServer(srv, {
    ctrl <- session$userData$.rewind
    rewind_pause()
    settle(session, region = "North")
    expect_equal(ctrl$history$size(), 0L)

    rewind_resume()
    settle(session, region = "South")
    expect_equal(ctrl$history$size(), 1L)
    expect_equal(ctrl$history$current()$inputs$region, "South")
  })
})

test_that("rewind_disable() destroys the observers, not just clears the pointer", {
  shiny::testServer(srv, {
    ctrl <- session$userData$.rewind
    settle(session, region = "North")
    expect_equal(ctrl$history$size(), 1L)

    result <- rewind_disable()
    expect_true(result)
    expect_null(session$userData$.rewind)

    # rewind must not capture more input changes. destroy() could remove
    # only the controller and keep the observers. The capture observer holds
    # `ctrl` in its closure. It would thus continue to run, and it would
    # change a history that nothing uses.
    session$setInputs(region = "South")
    Sys.sleep(0.15)
    session$elapse(200)
    expect_equal(ctrl$history$size(), 1L)
    expect_equal(ctrl$history$current()$inputs$region, "North")

    # The public functions must also show that rewind is not enabled.
    expect_error(rewind_history(), "not enabled")
  })
})

test_that("rewind_disable() is a no-op when rewind was never enabled", {
  bare_srv <- function(input, output, session) NULL
  shiny::testServer(bare_srv, {
    expect_false(rewind_disable())
  })
})
