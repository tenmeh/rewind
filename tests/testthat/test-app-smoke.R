# test-app-smoke.R -------------------------------------------------------
#
# The unit tests (test-history.R, test-state.R) and the testServer
# integration tests (test-controller.R, test-modules.R) prove the reactive
# *logic*. Neither proves the client half actually works: that rewind.js
# finds the right input binding, that a real slider drag really does
# coalesce under real timing (test-controller.R's coalescing tests run
# against testServer's virtual clock, not a real one), or that clicking the
# bundled buttons in a real page does what the custom-message wiring
# promises.
#
# This launches the real demo app (inst/examples/demo) in a headless
# browser and drives it the way a user would: change an input, undo it,
# redo it, pin something tracked via rewind_track(), drag the year slider,
# and exercise both keyboard-shortcut branches. See helper-shiny-smoke.R for
# why "browser console is clean" is not itself proof of success, and for
# why this skips (rather than fails) when no headless Chrome is available.
#
# A note on timing: unlike test-controller.R's testServer-based tests, this
# is a real R process and a real browser, so the demo's real coalesce_ms
# (400, see the app's rewind_enable() call) is a genuine wall-clock delay.
# app$wait_for_idle() is not enough on its own: between an input changing
# and the coalesce window elapsing, Shiny is genuinely idle (nothing is
# computing - it is just waiting on a timer), so wait_for_idle() can return
# before the entry is actually committed. settle() adds a real Sys.sleep()
# comfortably longer than the app's coalesce_ms first.

app_dir <- system.file("examples/demo", package = "rewind")
demo_coalesce_ms <- 400

settle <- function(app) {
  Sys.sleep((demo_coalesce_ms + 250) / 1000)
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

  # --- a plain input change becomes one entry, undo/redo round-trip -------
  app$set_inputs(region = "South")
  settle(app)
  expect_equal(rail_size(app), 2L)
  expect_false(is_undo_disabled(app))

  app$click(selector = ".rewind-undo")
  settle(app)
  expect_equal(app$get_value(input = "region"), "All")
  # The browser's echo of the restored value must not add a third entry.
  expect_equal(rail_size(app), 2L)

  app$click(selector = ".rewind-redo")
  settle(app)
  expect_equal(app$get_value(input = "region"), "South")

  # --- rewind_track(): server-side reactiveValues round-trip through undo -
  app$click("pin")
  settle(app)
  expect_match(app$get_value(output = "selection"), "^Pinned:")
  expect_equal(rail_size(app), 3L)

  app$click(selector = ".rewind-undo")
  settle(app)
  expect_equal(app$get_value(output = "selection"), "Nothing pinned.")

  app$click(selector = ".rewind-redo")
  settle(app)

  # --- rewind_step(): several inputs, one labelled entry ------------------
  before <- rail_size(app)
  app$click("reset")
  settle(app)
  expect_equal(app$get_value(input = "region"), "All")
  expect_false(isTRUE(app$get_value(input = "active_only")))
  expect_equal(rail_size(app), before + 1L)
  labels <- rail_labels(app)
  expect_equal(labels[length(labels)], "Reset filters")

  # --- coalescing: a real, rapid slider drag under real timing ------------
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
  Sys.sleep(0.2)
  settle(app)
  # Several rapid changes inside coalesce_ms must still land as one entry.
  expect_equal(rail_size(app), before + 1L)

  # --- keyboard: Ctrl+Z undoes from the page body --------------------------
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

  # --- keyboard: Ctrl+Z inside the text box does NOT undo (native undo) ---
  before_label <- current_step_label(app)
  app$run_js("
    var box = document.getElementById('search');
    box.focus();
    box.dispatchEvent(new KeyboardEvent('keydown', {key: 'z', ctrlKey: true, bubbles: true, cancelable: true}));
  ")
  settle(app)
  expect_equal(current_step_label(app), before_label)

  # --- rail: clicking a step jumps straight to it --------------------------
  app$run_js("document.querySelector('.rewind-step[data-index=\"1\"]').click();")
  settle(app)
  expect_equal(app$get_value(input = "region"), "All")
  expect_true(is_undo_disabled(app))

  # --- the universal gate: nothing threw, on either side of the wire ------
  expect_no_shiny_errors(app)
})
