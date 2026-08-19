# test-app-smoke.R -------------------------------------------------------
#
# The unit tests (test-history.R, test-state.R) and the testServer tests
# (test-controller.R, test-modules.R) examine the reactive *logic*. They do
# not examine the code in the browser. They do not show that rewind.js finds
# the correct input binding. They do not show that a real movement of a
# slider becomes one step with real time. The grouping tests in
# test-controller.R use the virtual clock of testServer, and not a real
# clock. They also do not show that the buttons operate in a real page.
#
# This test starts the real demo application (inst/examples/demo) in a
# headless browser. It then uses the application as a user does. It changes
# an input, and does an undo and a redo. It pins a value that
# rewind_track() records. It moves the year slider. It uses both keyboard
# shortcuts.
#
# helper-shiny-smoke.R tells you why "the browser console is clean" does not
# show success. It also tells you why this test skips, and does not fail,
# when there is no headless Chrome.
#
# A note about time. The tests in test-controller.R use testServer. This
# test is different. It uses a real R process and a real browser. The
# coalesce_ms of the demo (400, refer to its rewind_enable() call) is thus a
# real delay.
#
# app$wait_for_idle() alone is not sufficient. Between a change of an input
# and the end of the grouping period, Shiny is idle. It computes nothing,
# and only waits for a timer. wait_for_idle() can thus return before rewind
# writes the entry. settle() first uses a real Sys.sleep(). That sleep is
# longer than the coalesce_ms of the application.

app_dir <- system.file("examples/demo", package = "rewind")
demo_coalesce_ms <- 400

settle <- function(app) {
  Sys.sleep((demo_coalesce_ms + 500) / 1000)
  app$wait_for_idle(timeout = 10000L)
  invisible(app)
}

rail_labels <- function(app) {
  unlist(app$get_js(
    "Array.from(document.querySelectorAll('.rewind-step-label')).map(e => e.textContent)"
  ))
}

rail_size <- function(app) {
  app$get_js("document.querySelectorAll('.rewind-step').length")
}

is_undo_disabled <- function(app) {
  isTRUE(app$get_js("document.querySelector('.rewind-undo').disabled"))
}

current_step_label <- function(app) {
  app$get_js("document.querySelector('.rewind-step.is-current .rewind-step-label').textContent")
}

test_that("the demo app: buttons, rail, coalescing, and tracked values all work in a real browser", {
  testthat::skip_on_cran()

  app <- local_app_driver(
    app_dir,
    name         = "rewind-demo-smoke",
    seed         = 42L,
    load_timeout = 45000L,
    timeout      = 15000L
  )
  app$wait_for_idle(timeout = 10000L)

  # --- initial state ------------------------------------------------------
  expect_true(is_undo_disabled(app))
  expect_equal(rail_labels(app), "Initial state")

  # --- one input change makes one entry. Then undo and redo. -------------
  app$set_inputs(region = "South")
  settle(app)
  expect_equal(rail_size(app), 2L)
  expect_false(is_undo_disabled(app))

  app$click(selector = ".rewind-undo")
  settle(app)
  expect_equal(app$get_value(input = "region"), "All")
  # The echo from the browser must not add a third entry.
  expect_equal(rail_size(app), 2L)

  app$click(selector = ".rewind-redo")
  settle(app)
  expect_equal(app$get_value(input = "region"), "South")

  # --- rewind_track(): undo must restore the values on the server --------
  app$click("pin")
  settle(app)
  expect_match(app$get_value(output = "selection"), "^Pinned:")
  expect_equal(rail_size(app), 3L)

  app$click(selector = ".rewind-undo")
  settle(app)
  expect_equal(app$get_value(output = "selection"), "Nothing pinned.")

  app$click(selector = ".rewind-redo")
  settle(app)

  # --- rewind_step(): several inputs give one entry with a label ---------
  before <- rail_size(app)
  app$click("reset")
  settle(app)
  expect_equal(app$get_value(input = "region"), "All")
  expect_false(isTRUE(app$get_value(input = "active_only")))
  expect_equal(rail_size(app), before + 1L)
  labels <- rail_labels(app)
  expect_equal(labels[length(labels)], "Reset filters")

  # --- grouping: a fast, real movement of a slider, with real time -------
  before <- rail_size(app)
  app$run_js("
    (function () {
      var $el = window.jQuery(document.getElementById('year'));
      var vals = [[2019,2025],[2020,2025],[2021,2024],[2022,2023]];
      var i = 0;
      function step() {
        if (i < vals.length) {
          $el.data('ionRangeSlider').update({from: vals[i][0], to: vals[i][1]});
          $el.trigger('change');
          i++;
          setTimeout(step, 40);
        }
      }
      step();
    })();
  ")
  Sys.sleep(0.3)
  settle(app)
  # Several fast changes inside coalesce_ms must give one entry.
  expect_equal(rail_size(app), before + 1L)

  # --- keyboard: Ctrl+Z does an undo from the page body ------------------
  before <- rail_size(app)
  before_label <- current_step_label(app)
  app$run_js("
    document.body.dispatchEvent(new KeyboardEvent(
      'keydown', {key: 'z', ctrlKey: true, bubbles: true, cancelable: true}
    ));
  ")
  settle(app)
  expect_equal(rail_size(app), before)
  expect_false(identical(current_step_label(app), before_label))

  # --- keyboard: Ctrl+Z in the text box must NOT do an undo --------------
  before_label <- current_step_label(app)
  app$run_js("
    var box = document.getElementById('search');
    box.focus();
    box.dispatchEvent(new KeyboardEvent('keydown', {key: 'z', ctrlKey: true, bubbles: true, cancelable: true}));
  ")
  settle(app)
  expect_equal(current_step_label(app), before_label)

  # --- rail: a click on a step moves to that step ------------------------
  app$run_js("document.querySelector('.rewind-step[data-index=\"1\"]').click();")
  settle(app)
  expect_equal(app$get_value(input = "region"), "All")
  expect_true(is_undo_disabled(app))

  # --- the last check: nothing threw an error, on the server or client ---
  expect_no_shiny_errors(app)
})
