# test-reference-values.R ------------------------------------------------
#
# rewind keeps each tracked value as it is, and does not copy it. An R6
# object and an environment are thus kept by reference. The help of
# rewind_track() says what that means. These tests hold both halves of it:
# a change inside the object is not a step, and a change to a copy is.
#
# Refer to the note at the top of test-controller.R about the virtual clock
# of testServer and the real clock that the grouping period uses.

settle_ref <- function(session) {
  session$flushReact()
  Sys.sleep(0.15)
  session$elapse(250)
}

Counter <- R6::R6Class("Counter", public = list(n = 0))

reference_server <- function(input, output, session) {
  rewind_enable(coalesce_ms = 30)
  state <- shiny::reactiveValues(model = Counter$new())
  rewind_track(state, fields = "model")
  session$userData$state <- state
}

test_that("a change inside an R6 object is not a step", {
  shiny::testServer(reference_server, {
    ctrl <- session$userData$.rewind
    state <- session$userData$state
    settle_ref(session)
    before <- ctrl$history$size()

    model <- state$model
    model$n <- 5
    state$model <- model # the same object
    settle_ref(session)

    expect_equal(ctrl$history$size(), before)
  })
})

test_that("a change to a copy is a step, and an undo brings the old one back", {
  shiny::testServer(reference_server, {
    ctrl <- session$userData$.rewind
    state <- session$userData$state
    settle_ref(session)
    before <- ctrl$history$size()

    model <- state$model$clone(deep = TRUE)
    model$n <- 5
    state$model <- model
    settle_ref(session)

    expect_equal(ctrl$history$size(), before + 1L)
    expect_equal(state$model$n, 5)

    ctrl$undo()
    session$flushReact()
    expect_equal(state$model$n, 0)
  })
})
