# test-capture.R ---------------------------------------------------------
#
# What capture records, and when it records it.
#
# Refer to the note at the top of test-controller.R about the virtual clock
# of testServer and the real clock that the grouping period uses.

settle_capture <- function(session, ..., wait = 0.15) {
  session$setInputs(...)
  Sys.sleep(wait)
  session$elapse(round(wait * 1000) + 100)
}


# --- A busy input must not hold a step back -------------------------------
#
# The capture observer reads every input, so it runs again when any input
# changes, also one that is not captured. It used to move the grouping
# deadline on every run, even when the captured state had not changed. A
# plot hover that fires every 0.1 s then held back every step until the
# mouse stopped, and merged the values in between.

test_that("an input that changes all the time does not hold back a step", {
  shiny::testServer(function(input, output, session) {
    rewind_enable(inputs = "region", coalesce_ms = 200)
  }, {
    ctrl <- session$userData$.rewind
    settle_capture(session, region = "a", wait = 0.35)
    expect_equal(ctrl$history$size(), 1L)

    session$setInputs(region = "b")
    # Hover for twice the grouping period, without a pause.
    for (i in 1:8) {
      Sys.sleep(0.05)
      session$setInputs(plot_hover = i)
      session$elapse(50)
    }

    # The step for "b" must be written by now, while the hover goes on.
    expect_equal(ctrl$history$size(), 2L)
    expect_equal(ctrl$history$current()$inputs$region, "b")
  })
})

test_that("a change between two others is kept while a busy input fires", {
  shiny::testServer(function(input, output, session) {
    rewind_enable(inputs = "region", coalesce_ms = 200)
  }, {
    ctrl <- session$userData$.rewind
    settle_capture(session, region = "a", wait = 0.35)

    session$setInputs(region = "b")
    for (i in 1:6) { Sys.sleep(0.05); session$setInputs(plot_hover = i); session$elapse(50) }
    session$setInputs(region = "c")
    for (i in 7:12) { Sys.sleep(0.05); session$setInputs(plot_hover = i); session$elapse(50) }

    # a, b and c are three steps. Before the fix, b was merged away.
    regions <- vapply(seq_len(ctrl$history$size()),
                      function(i) ctrl$history$state_at(i)$inputs$region, character(1))
    expect_equal(regions, c("a", "b", "c"))
  })
})


# --- Only inputs the page can restore -------------------------------------
#
# The browser reports the ids of the inputs that have a binding, as the
# input rewind_bound_ids (refer to reportBound() in rewind.js). These tests
# send that input by hand, which is what the browser does.
# test-restorable.R covers the same in a real browser.

test_that("by default, an input the page cannot restore is not a step", {
  shiny::testServer(function(input, output, session) {
    rewind_enable(coalesce_ms = 30)
  }, {
    ctrl <- session$userData$.rewind
    settle_capture(session, region = "North")
    settle_capture(session, rewind_bound_ids = "region")

    steps <- ctrl$history$size()
    settle_capture(session, plot_hover = list(x = 1, y = 2))
    settle_capture(session, tbl_rows_current = 1:10, tbl_state = list(start = 0))
    expect_equal(ctrl$history$size(), steps)

    settle_capture(session, region = "South")
    expect_equal(ctrl$history$size(), steps + 1L)
    expect_equal(names(ctrl$history$current()$inputs), "region")
  })
})

test_that("values captured before the report arrives are removed", {
  shiny::testServer(function(input, output, session) {
    rewind_enable(coalesce_ms = 30)
  }, {
    ctrl <- session$userData$.rewind
    settle_capture(session, region = "North", tbl_rows_current = 1:10)
    settle_capture(session, tbl_rows_current = 11:20)   # a step of its own
    expect_equal(ctrl$history$size(), 2L)

    settle_capture(session, rewind_bound_ids = "region")

    # The table values go, and the step that only changed them goes too.
    expect_equal(ctrl$history$size(), 1L)
    expect_false("tbl_rows_current" %in% names(ctrl$history$current()$inputs))
  })
})

test_that("an app that names its inputs gets them, restorable or not", {
  shiny::testServer(function(input, output, session) {
    rewind_enable(inputs = c("region", "tbl_rows_selected"), coalesce_ms = 30)
  }, {
    ctrl <- session$userData$.rewind
    settle_capture(session, region = "North", tbl_rows_selected = 1L)
    settle_capture(session, rewind_bound_ids = "region")
    settle_capture(session, tbl_rows_selected = 2L)
    expect_equal(ctrl$history$current()$inputs$tbl_rows_selected, 2L)
  })
})

test_that("without a report, every input is captured as before", {
  # shiny::testServer() has no browser, and a page can be slow to report.
  shiny::testServer(function(input, output, session) {
    rewind_enable(coalesce_ms = 30)
  }, {
    ctrl <- session$userData$.rewind
    settle_capture(session, region = "North", plot_hover = 1)
    expect_setequal(names(ctrl$history$current()$inputs), c("plot_hover", "region"))
  })
})

test_that("inside a module, the reported ids are matched to the local names", {
  mod <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      rewind_enable(coalesce_ms = 30)
    })
  }
  shiny::testServer(mod, {
    ctrl <- session$userData$.rewind
    settle_capture(session, region = "North", hov = 1)
    session$rootScope()$setInputs(rewind_bound_ids = c(session$ns("region"), "other-x"))
    Sys.sleep(0.15)
    session$elapse(250)
    expect_equal(ctrl$bound(), "region")
    expect_equal(names(ctrl$history$current()$inputs), "region")
  })
})
