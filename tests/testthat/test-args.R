# test-args.R ------------------------------------------------------------
#
# Arguments with a wrong value must fail at the call, with a message that
# names the argument. Each case here was accepted with no message before,
# and the API review of 2026-10-10 found it by calling the function.

enable_with <- function(...) {
  args <- list(...)
  function(input, output, session) do.call(rewind_enable, args)
}

settle_args <- function(session, ...) {
  session$setInputs(...)
  Sys.sleep(0.15)
  session$elapse(250)
}

# --- rewind_enable ---------------------------------------------------------

test_that("shortcuts and verbose must be TRUE or FALSE", {
  # "yes" used to switch the shortcuts off, with no message.
  for (v in list("yes", NA, 1, c(TRUE, FALSE))) {
    expect_error(
      shiny::testServer(enable_with(shortcuts = v), {}),
      "`shortcuts` must be TRUE or FALSE.",
      fixed = TRUE
    )
    expect_error(
      shiny::testServer(enable_with(verbose = v), {}),
      "`verbose` must be TRUE or FALSE.",
      fixed = TRUE
    )
  }
})

test_that("depth must be a whole number of 2 or more", {
  # 2.5 used to become 2, and "50" was accepted.
  for (v in list(2.5, "50", 1, NA, Inf)) {
    expect_error(
      shiny::testServer(enable_with(depth = v), {}),
      "`depth` must be a single whole number of 2 or more.",
      fixed = TRUE
    )
  }
  expect_no_error(shiny::testServer(enable_with(depth = 10), {}))
})

test_that("inputs and exclude must be text", {
  expect_error(
    shiny::testServer(enable_with(inputs = 1), {}),
    "`inputs` must be a character vector or NULL.",
    fixed = TRUE
  )
  expect_error(
    shiny::testServer(enable_with(exclude = NA_character_), {}),
    "`exclude` must be a character vector or NULL.",
    fixed = TRUE
  )
})

test_that("a name in `inputs` that the page does not have gives a warning", {
  # A typo used to capture nothing for that input, with no message. The
  # check runs when the page first reports its inputs, because an input
  # does not exist before the page is there.
  shiny::testServer(
    enable_with(inputs = c("region", "regoin"), coalesce_ms = 30),
    {
      settle_args(session, region = "North")
      expect_warning(
        settle_args(session, rewind_bound_ids = "region"),
        "`inputs` names an input that the page does not have: \"regoin\".",
        fixed = TRUE
      )
    }
  )
  shiny::testServer(enable_with(inputs = "region", coalesce_ms = 30), {
    settle_args(session, region = "North")
    expect_no_warning(settle_args(session, rewind_bound_ids = "region"))
  })
})

# --- rewind_track -----------------------------------------------------------

test_that("a field that the reactiveValues does not have gives a warning", {
  expect_warning(
    shiny::testServer(
      function(input, output, session) {
        rewind_enable()
        st <- shiny::reactiveValues(zoom = 1)
        rewind_track(st, fields = "zom")
      },
      {}
    ),
    "`fields` names a field that `values` does not have: \"zom\".",
    fixed = TRUE
  )
})

test_that("the id of a tracked group must be one string", {
  expect_error(
    shiny::testServer(
      function(input, output, session) {
        rewind_enable()
        rewind_track(shiny::reactiveValues(a = 1), id = c("x", "y"))
      },
      {}
    ),
    "`id` must be a single string or NULL.",
    fixed = TRUE
  )
})

# --- moving through the history --------------------------------------------

test_that("rewind_jump refuses a position that is not a whole number", {
  shiny::testServer(enable_with(coalesce_ms = 30), {
    settle_args(session, region = "a")
    settle_args(session, region = "b")
    # 1.9 used to move to position 1. rewind_diff() already refused it.
    expect_error(
      rewind_jump(1.9),
      "`index` must be a single whole number.",
      fixed = TRUE
    )
    # "two" used to give a base warning that did not name the argument.
    expect_error(
      rewind_jump("two"),
      "`index` must be a single whole number.",
      fixed = TRUE
    )
    # A whole number out of range still returns FALSE, as undo at the start
    # does.
    expect_false(rewind_jump(99))
  })
})

# --- labels on steps and controls -------------------------------------------

test_that("rewind_step needs one string as the label", {
  shiny::testServer(enable_with(), {
    # 1:2 used to give the label "1".
    expect_error(
      rewind_step(NULL, label = 1:2),
      "`label` must be a single string or NULL.",
      fixed = TRUE
    )
  })
})

test_that("rewind_buttons and rewind_ui check their text and classes", {
  expect_error(
    rewind_buttons(undo_label = 1),
    "`undo_label` must be a single string or NULL.",
    fixed = TRUE
  )
  expect_error(
    rewind_buttons(button_class = 1),
    "`button_class` must be a character vector or NULL.",
    fixed = TRUE
  )
  expect_error(
    rewind_ui(label = c("a", "b")),
    "`label` must be a single string or NULL.",
    fixed = TRUE
  )
})

test_that("rewind_ui accepts a number of pixels for max_height", {
  # 300 used to write "max-height: 300;", which browsers ignore.
  html <- as.character(rewind_ui(max_height = 300))
  expect_match(html, "max-height: 300px;", fixed = TRUE)
  expect_match(as.character(rewind_ui()), "max-height: 20rem;", fixed = TRUE)
  expect_error(rewind_ui(max_height = "tall"), "not a valid CSS unit")
})
