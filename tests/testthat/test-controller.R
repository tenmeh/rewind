# Controller-level integration tests, driven through real Shiny sessions via
# shiny::testServer(). Unlike test-history.R and test-state.R, which exercise
# the pure R6/helper logic directly, these prove the reactive wiring
# rewind_enable() creates: capture, coalescing, echo suppression, and restore.
#
# A note on timing: the coalescing window is computed against Sys.time() (see
# controller.R), which is correct and necessary in a real running app - but
# shiny::testServer()'s session$elapse() drives a private virtual clock
# entirely decoupled from Sys.time(), so elapse() alone never advances the
# real clock rewind is actually waiting on. Each test bridges that gap with a
# short Sys.sleep() longer than the test's own coalesce_ms, letting real time
# genuinely pass, then calls elapse() to run the scheduled flush. This is a
# testServer/Sys.time() interaction, not a bug: it does not affect production
# behaviour, only how tests observe it.

srv <- function(input, output, session) {
  rewind_enable(coalesce_ms = 30)
  state <- shiny::reactiveValues(zoom = 1)
  rewind_track(state, fields = "zoom", id = "st")
}

settle <- function(session, ...) {
  session$setInputs(...)
  Sys.sleep(0.06)
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
    Sys.sleep(0.06)
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

    # Simulate the browser echoing the restored value back exactly, the way
    # rewind.js does after applying rewind:restore: note() the state undo()
    # just moved the pointer to.
    restored <- ctrl$history$current()
    ctrl$note(restored)
    Sys.sleep(0.06)
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

    # A bare reactiveValues assignment (unlike session$setInputs()) doesn't
    # itself trigger a reactive flush under testServer, so flushReact() is
    # called explicitly to let the capture observer see it.
    state$zoom <- 2
    session$flushReact()
    Sys.sleep(0.06)
    session$elapse(200)
    expect_equal(ctrl$history$size(), 2L)
    expect_equal(ctrl$history$current()$values$st$zoom, 2)

    ctrl$undo()
    expect_equal(ctrl$history$current()$values$st$zoom, 1)
    expect_equal(state$zoom, 1)

    # The assignment inside undo()'s restore() is itself a tracked-value
    # change; confirm its echo is absorbed too, rather than spawning a third
    # entry.
    session$flushReact()
    Sys.sleep(0.06)
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
    Sys.sleep(0.1)
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

    # Further input changes must not be captured: if destroy() had merely
    # detached the controller without destroying the observers, the capture
    # observer (which still holds `ctrl` in its closure) would keep running
    # and silently keep mutating a now-orphaned history.
    session$setInputs(region = "South")
    Sys.sleep(0.06)
    session$elapse(200)
    expect_equal(ctrl$history$size(), 1L)
    expect_equal(ctrl$history$current()$inputs$region, "North")

    # The public API agrees the session is back to "not enabled".
    expect_error(rewind_history(), "not enabled")
  })
})

test_that("rewind_disable() is a no-op when rewind was never enabled", {
  bare_srv <- function(input, output, session) NULL
  shiny::testServer(bare_srv, {
    expect_false(rewind_disable())
  })
})
