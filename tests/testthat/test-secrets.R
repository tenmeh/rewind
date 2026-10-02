# test-secrets.R ---------------------------------------------------------
#
# Password fields, on the server side. The browser reports their ids as the
# input rewind_secret_ids (refer to reportSecrets() in rewind.js). These
# tests send that input by hand, which is what the browser does.
#
# test-text-inputs.R covers the same in a real browser, including the
# report itself.
#
# Refer to the note at the top of test-controller.R about the virtual clock
# of testServer and the real clock that the grouping period uses.

srv_secret <- function(input, output, session) {
  rewind_enable(coalesce_ms = 30)
}

settle_secret <- function(session, ...) {
  session$setInputs(...)
  Sys.sleep(0.15)
  session$elapse(200)
}

test_that("a reported password field leaves the history, and stays out", {
  shiny::testServer(srv_secret, {
    ctrl <- session$userData$.rewind

    # At page load the first snapshot can hold the field before the browser
    # reports it. This is that situation.
    settle_secret(session, region = "North", pw = "")
    expect_true("pw" %in% names(ctrl$history$current()$inputs))

    settle_secret(session, rewind_secret_ids = "pw")
    expect_false("pw" %in% names(ctrl$history$current()$inputs))
    expect_equal(ctrl$secret(), "pw")

    # Typing a password is not a step.
    steps <- ctrl$history$size()
    settle_secret(session, pw = "s3cret")
    expect_equal(ctrl$history$size(), steps)

    # A real change is still a step, and the diff does not show the field.
    settle_secret(session, region = "South")
    expect_equal(ctrl$history$size(), steps + 1L)
    expect_equal(rewind_diff()$name, "region")

    # An undo does not send the password back to the browser.
    expect_false("pw" %in% names(ctrl$history$state_at(1)$inputs))
  })
})

test_that("a password field is left out even when `inputs` names it", {
  shiny::testServer(function(input, output, session) {
    rewind_enable(inputs = c("region", "pw"), coalesce_ms = 30)
  }, {
    ctrl <- session$userData$.rewind
    settle_secret(session, region = "North", pw = "")
    settle_secret(session, rewind_secret_ids = "pw")
    settle_secret(session, pw = "s3cret", region = "South")
    expect_false("pw" %in% names(ctrl$history$current()$inputs))
  })
})

test_that("inside a module, the id from the page is matched to the local name", {
  mod <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      rewind_enable(coalesce_ms = 30)
    })
  }

  shiny::testServer(mod, {
    ctrl <- session$userData$.rewind
    root <- session$rootScope()

    settle_secret(session, pw = "", other = 1)

    # The browser reports the full id from the page, such as "mymod-pw",
    # and a field outside this module as well.
    root$setInputs(rewind_secret_ids = c(session$ns("pw"), "elsewhere-pw"))
    Sys.sleep(0.15)
    session$elapse(200)

    expect_equal(ctrl$secret(), "pw")
    expect_false("pw" %in% names(ctrl$history$current()$inputs))
  })
})
