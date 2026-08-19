# Tests for rewind_enable() inside a moduleServer(). The documents said
# "untested" before. These tests show that it operates, and they record the
# names that rewind uses.
#
# A read of session$input inside a module gives module-local names already.
# This is the operation of Shiny, and not of rewind. The snapshots thus hold
# names with no namespace. restore() adds the namespace with session$ns()
# before it sends the values to the client.
#
# The note about Sys.sleep() and the virtual clock of testServer is the same
# as in test-controller.R.

mod_srv <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    rewind_enable(coalesce_ms = 30)
    state <- shiny::reactiveValues(pinned = character(0))
    rewind_track(state, fields = "pinned", id = "pins")
  })
}

settle <- function(session, ...) {
  session$setInputs(...)
  Sys.sleep(0.15)
  session$elapse(200)
}

test_that("captured snapshots use module-local input names", {
  shiny::testServer(mod_srv, args = list(id = "mymod"), {
    ctrl <- session$userData$.rewind
    settle(session, region = "North")

    expect_equal(names(ctrl$history$current()$inputs), "region")
    expect_equal(ctrl$history$current()$inputs$region, "North")
  })
})

test_that("session$ns() qualifies module-local names the way restore() relies on", {
  shiny::testServer(mod_srv, args = list(id = "mymod"), {
    # restore() in controller.R makes this change to each captured input
    # name before it sends the values to the client. The snapshot must hold
    # module-local names to give the correct DOM id.
    expect_equal(session$ns("region"), "mymod-region")
  })
})

test_that("undo works inside a module and round-trips through a simulated echo", {
  shiny::testServer(mod_srv, args = list(id = "mymod"), {
    ctrl <- session$userData$.rewind
    settle(session, region = "North")
    settle(session, region = "South")
    expect_equal(ctrl$history$size(), 2L)

    ctrl$undo()
    expect_equal(ctrl$history$current()$inputs$region, "North")

    restored <- ctrl$history$current()
    ctrl$note(restored)
    Sys.sleep(0.15)
    session$elapse(200)
    expect_equal(ctrl$history$size(), 2L)
  })
})

test_that("rewind_track() reactiveValues work inside a module", {
  shiny::testServer(mod_srv, args = list(id = "mymod"), {
    ctrl <- session$userData$.rewind
    settle(session, region = "Baseline")

    state$pinned <- "a"
    session$flushReact()
    Sys.sleep(0.15)
    session$elapse(200)

    expect_equal(ctrl$history$size(), 2L)
    expect_equal(ctrl$history$current()$values$pins$pinned, "a")

    ctrl$undo()
    expect_equal(ctrl$history$current()$values$pins$pinned, character(0))
    expect_equal(state$pinned, character(0))
  })
})

test_that("session$userData is shared across modules, so a second rewind_enable() reuses the first controller", {
  two_mod_srv <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      ctrl1 <- rewind_enable()
      # expect_warning(expr, ...) gives the warning condition. It does not
      # give the value of expr. This test thus keeps the value separately.
      warned <- FALSE
      ctrl2 <- withCallingHandlers(
        rewind_enable(),
        warning = function(w) {
          warned <<- TRUE
          invokeRestart("muffleWarning")
        }
      )
    })
  }
  shiny::testServer(two_mod_srv, args = list(id = "modx"), {
    expect_true(warned)
    expect_identical(ctrl1, ctrl2)
  })
})
