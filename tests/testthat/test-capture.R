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
