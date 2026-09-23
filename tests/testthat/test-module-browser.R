# test-module-browser.R --------------------------------------------------
#
# Undo, redo and the rail, driven through a real browser, with
# rewind_enable() called inside a moduleServer().
#
# test-modules.R already covers modules, but it calls ctrl$undo() on the
# controller directly. It never goes through the browser, so it cannot see
# how an undo reaches the server. That path had a fault: rewind.js sends
# the global input ids rewind_undo, rewind_redo and rewind_jump, but inside
# a module session$input$rewind_undo reads the namespaced id
# "mymod-rewind_undo". Capture worked, so the rail filled up, but every
# button, shortcut and rail click did nothing.
#
# Refer to the note at the top of test-app-smoke.R about why settle() must
# sleep for real before it waits for Shiny to be idle.

fixture_module_coalesce_ms <- 150

module_app <- function() {
  mod_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::selectInput(ns("pick"), "Pick", c("a", "b", "c"))
  }

  mod_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      rewind_enable(coalesce_ms = fixture_module_coalesce_ms)
    })
  }

  shiny::shinyApp(
    # The buttons and the rail sit outside the module, as they do in most
    # applications. rewind_buttons() has no id, so its inputs are global
    # wherever it is placed.
    ui = shiny::fluidPage(
      rewind_buttons(),
      rewind_ui(),
      mod_ui("mymod")
    ),
    server = function(input, output, session) {
      mod_server("mymod")
    }
  )
}

settle_module <- function(app) {
  Sys.sleep((fixture_module_coalesce_ms + 350) / 1000)
  app$wait_for_idle(timeout = 10000L)
  invisible(app)
}


test_that("undo, redo and the rail work when rewind_enable() runs inside a module", {
  testthat::skip_on_cran()

  app <- local_app_driver(module_app(), name = "module-undo")
  pick <- function() app$get_value(input = "mymod-pick")
  n_steps <- function() {
    as.integer(app$get_js("document.querySelectorAll('.rewind-step').length"))
  }

  settle_module(app)
  app$set_inputs(`mymod-pick` = "b")
  settle_module(app)
  app$set_inputs(`mymod-pick` = "c")
  settle_module(app)

  # Guard: capture must work, or the assertions below prove nothing. This
  # part passed even while the fault was present.
  expect_equal(n_steps(), 3L)
  expect_equal(pick(), "c")

  # The button. Before the fix this did nothing, and the value stayed "c".
  app$click(selector = ".rewind-undo")
  settle_module(app)
  expect_equal(pick(), "b")

  app$click(selector = ".rewind-redo")
  settle_module(app)
  expect_equal(pick(), "c")

  # The rail. It uses its own input, rewind_jump, so it needs its own check.
  app$click(selector = ".rewind-step[data-index='1']")
  settle_module(app)
  expect_equal(pick(), "a")

  expect_no_shiny_errors(app)
})
