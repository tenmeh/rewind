# test-rail-scroll.R -----------------------------------------------------
#
# A regression test for one fault: the history rail used to scroll the
# container it sits in.
#
# The rail keeps the current step in view. It did that with
# scrollIntoView(), which scrolls EVERY scrollable ancestor of the element
# and not only the nearest one. The option block: "nearest" limits how far
# each ancestor moves. It does not stop them moving. So an application that
# put rewind_ui() in a sidebar had that sidebar jump on every change of the
# history, and looked as though it scrolled by itself.
#
# This test cannot use testServer. The fault is in the browser, in
# rewind.js, and it appears only when a real element has a real height and
# a real overflow. It thus needs a real page.
#
# The application below is the smallest shape that shows the fault: the
# rail inside a short box that can scroll, with enough content above the
# rail to push it below the fold of that box.

# A short grouping period. This test makes many entries, and the default of
# 400 ms would make it slow for no gain.
fixture_coalesce_ms <- 150

rail_scroll_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::selectInput("pick", "Pick", c("a", "b", "c", "d", "e", "f")),
      # Outside the box on purpose. The test clicks this to move back
      # through the history, and a control inside the box would change what
      # the box has to scroll.
      rewind_buttons(),
      shiny::tags$div(
        id = "scrollbox",
        style = "height: 150px; overflow-y: auto;",
        shiny::tags$div(style = "height: 500px;", "filler"),
        rewind_ui(label = "History", max_height = "4rem")
      )
    ),
    server = function(input, output, session) {
      rewind_enable(inputs = "pick", coalesce_ms = fixture_coalesce_ms)
    }
  )
}

# Between a change of an input and the end of the grouping period, Shiny is
# idle: it computes nothing and only waits for a timer. wait_for_idle() can
# thus return before rewind writes the entry. Sleep for real first. Refer to
# the note at the top of test-app-smoke.R.
settle_rail <- function(app) {
  Sys.sleep((fixture_coalesce_ms + 350) / 1000)
  app$wait_for_idle(timeout = 10000L)
  invisible(app)
}


test_that("the rail scrolls itself and never scrolls the container that holds it", {
  testthat::skip_on_cran()

  app <- local_app_driver(rail_scroll_app(), name = "rail-scroll")

  box_top <- function() {
    as.numeric(app$get_js("document.getElementById('scrollbox').scrollTop"))
  }
  rail_top <- function() {
    as.numeric(app$get_js("document.querySelector('.rewind-rail').scrollTop"))
  }
  rail_overflow <- function() {
    as.numeric(app$get_js(
      "(function (r) { return r.scrollHeight - r.clientHeight; })(document.querySelector('.rewind-rail'))"
    ))
  }
  n_steps <- function() {
    as.integer(app$get_js("document.querySelectorAll('.rewind-step').length"))
  }
  current_is_visible_in_rail <- function() {
    isTRUE(as.logical(app$get_js(
      "(function () {
         var rail = document.querySelector('.rewind-rail');
         var cur = rail.querySelector('.is-current');
         if (!cur) return false;
         var rr = rail.getBoundingClientRect();
         var cr = cur.getBoundingClientRect();
         return cr.top >= rr.top - 1 && cr.bottom <= rr.bottom + 1;
       })()"
    )))
  }

  settle_rail(app)
  expect_equal(box_top(), 0)

  # Make enough entries that the rail is longer than its own max-height.
  for (value in c("b", "c", "d", "e", "f", "a", "b", "c")) {
    app$set_inputs(pick = value)
    settle_rail(app)
  }

  # Guard against a test that passes because nothing happened. Without
  # these two, an empty rail would satisfy every assertion below: a rail
  # that never re-renders never scrolls anything. The rail must have grown,
  # and it must be long enough to need a scroll of its own.
  expect_gt(n_steps(), 5L)
  expect_gt(rail_overflow(), 0)

  # The fault. The box that holds the rail must not have moved.
  expect_equal(box_top(), 0)

  # The behaviour the code exists for. The rail scrolls itself, so the
  # current step stays in view. Assert both: a fix that simply deleted the
  # scrolling code would pass the check above and fail these.
  expect_gt(rail_top(), 0)
  expect_true(current_is_visible_in_rail())

  # Undo back towards the start. The rail must now scroll the other way,
  # and the box must still not move.
  for (i in 1:4) {
    app$click(selector = ".rewind-undo")
    settle_rail(app)
  }

  expect_equal(box_top(), 0)
  expect_true(current_is_visible_in_rail())

  expect_no_shiny_errors(app)
})
