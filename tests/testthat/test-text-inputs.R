# test-text-inputs.R -----------------------------------------------------
#
# Text inputs and password inputs, driven through a real browser.
#
# A text input needs its own test because the browser treats it
# differently from every other input: Ctrl+Z inside the box is the
# browser's own text undo, and rewind stays out of it on purpose. The
# tests below type real key events, so the browser handles them as it
# handles a person typing.
#
# A password input needs its own test because the server cannot tell it
# from a text input: both send a string. Only the browser knows which
# fields are passwords, so it reports them, and rewind leaves them out of
# the history. Before that, a password was kept in the history, put back
# in the box by an undo, and shown in plain text by rewind_diff().
#
# Refer to local_app_driver() in helper-shiny-smoke.R: install the package
# before a local run.

fixture_text_coalesce_ms <- 300

text_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      rewind_buttons(),
      rewind_ui(),
      shiny::textInput("txt", "Text"),
      shiny::passwordInput("pw", "Password"),
      shiny::actionButton("add_pw", "Add a password field"),
      shiny::uiOutput("later"),
      shiny::verbatimTextOutput("diff_names")
    ),
    server = function(input, output, session) {
      rewind_enable(coalesce_ms = fixture_text_coalesce_ms)

      # A password field that appears only after the page has loaded. The
      # browser must report it when Shiny binds it.
      output$later <- shiny::renderUI({
        shiny::req(input$add_pw > 0)
        shiny::passwordInput("pw_later", "Password, added later")
      })

      # Every name that the history has ever held, so the test can see that
      # no password got in, even for a moment.
      seen <- shiny::reactiveVal(character(0))
      shiny::observe({
        h <- rewind_history()
        all_names <- unique(unlist(lapply(seq_len(nrow(h)), function(i) {
          rewind_diff(from = max(1L, i - 1L), to = i)$name
        })))
        seen(union(shiny::isolate(seen()), all_names))
      })
      output$diff_names <- shiny::renderText(paste(sort(seen()), collapse = ","))
    }
  )
}

settle_text <- function(app) {
  Sys.sleep((fixture_text_coalesce_ms + 500) / 1000)
  app$wait_for_idle(timeout = 10000L)
  invisible(app)
}

# Real key events, so the browser types as it does for a person. Plain
# Input.insertText does not go on the browser's own undo stack.
type_keys <- function(app, id, text) {
  b <- app$get_chromote_session()
  app$run_js(sprintf("document.getElementById('%s').focus()", id))
  for (ch in strsplit(text, "")[[1]]) {
    b$Input$dispatchKeyEvent(type = "keyDown", key = ch, text = ch)
    b$Input$dispatchKeyEvent(type = "keyUp", key = ch)
    Sys.sleep(0.05)
  }
}

leave_box <- function(app) app$run_js("document.activeElement.blur()")

n_steps <- function(app) {
  as.integer(app$get_js("document.querySelectorAll('.rewind-step').length"))
}

box <- function(app, id) {
  app$get_js(sprintf("document.getElementById('%s').value", id))
}


test_that("a typed word is one step, and undo restores both the box and the server", {
  testthat::skip_on_cran()

  app <- local_app_driver(text_app(), name = "text-undo")
  settle_text(app)

  type_keys(app, "txt", "nausea")
  settle_text(app)
  leave_box(app)
  settle_text(app)

  # Six key presses, one step.
  expect_equal(n_steps(app), 2L)
  expect_equal(app$get_value(input = "txt"), "nausea")

  app$click(selector = ".rewind-undo")
  settle_text(app)
  expect_equal(box(app, "txt"), "")
  expect_equal(app$get_value(input = "txt"), "")

  app$click(selector = ".rewind-redo")
  settle_text(app)
  expect_equal(box(app, "txt"), "nausea")
  expect_equal(app$get_value(input = "txt"), "nausea")

  expect_no_shiny_errors(app)
})


test_that("a password is never kept in the history, nor restored by an undo", {
  testthat::skip_on_cran()

  app <- local_app_driver(text_app(), name = "text-password")
  settle_text(app)

  type_keys(app, "pw", "s3cret")
  settle_text(app)
  leave_box(app)
  settle_text(app)

  # Typing a password is not a step.
  expect_equal(n_steps(app), 1L)

  # Make a real step after it, then undo it. The undo must not touch the
  # password box.
  type_keys(app, "txt", "abc")
  settle_text(app)
  leave_box(app)
  settle_text(app)
  expect_equal(n_steps(app), 2L)

  app$click(selector = ".rewind-undo")
  settle_text(app)
  expect_equal(box(app, "txt"), "")
  expect_equal(box(app, "pw"), "s3cret")

  # A password field that renderUI adds later is left out as well.
  app$click(input = "add_pw")
  settle_text(app)
  type_keys(app, "pw_later", "t0ken")
  settle_text(app)
  leave_box(app)
  settle_text(app)

  seen <- strsplit(app$get_text("#diff_names"), ",", fixed = TRUE)[[1]]
  expect_true("txt" %in% seen)
  expect_false("pw" %in% seen)
  expect_false("pw_later" %in% seen)

  expect_no_shiny_errors(app)
})
