test_that("the public API refuses to run outside a session", {
  expect_error(rewind_enable(session = NULL), "No Shiny session")
  expect_error(rewind_undo(session = NULL), "No Shiny session")
  expect_error(rewind_redo(session = NULL), "No Shiny session")
  expect_error(rewind_history(session = NULL), "No Shiny session")
  expect_error(rewind_pause(session = NULL), "No Shiny session")
})

test_that("the public API refuses to run before rewind_enable()", {
  fake <- list(userData = new.env(parent = emptyenv()))

  expect_error(rewind_undo(session = fake), "not enabled")
  expect_error(rewind_history(session = fake), "not enabled")
  expect_error(rewind_track(shiny::reactiveValues(), session = fake), "not enabled")
})

test_that("rewind_disable requires a session but tolerates a missing controller", {
  expect_error(rewind_disable(session = NULL), "No Shiny session")

  fake <- list(userData = new.env(parent = emptyenv()))
  expect_false(rewind_disable(session = fake))
})

test_that("rewind_enable validates its arguments", {
  fake <- list(userData = new.env(parent = emptyenv()))

  expect_error(rewind_enable(session = fake, inputs = 1), "`inputs`")
  expect_error(rewind_enable(session = fake, exclude = 1), "`exclude`")
  expect_error(rewind_enable(session = fake, coalesce_ms = -1), "`coalesce_ms`")
  expect_error(rewind_enable(session = fake, coalesce_ms = c(1, 2)), "`coalesce_ms`")

  # A restore timeout of zero would stop the guard against the echo of a
  # restore, so it is not permitted.
  expect_error(rewind_enable(session = fake, restore_timeout = 0), "`restore_timeout`")
  expect_error(rewind_enable(session = fake, restore_timeout = -1), "`restore_timeout`")
  expect_error(rewind_enable(session = fake, restore_timeout = c(1, 2)), "`restore_timeout`")
  expect_error(rewind_enable(session = fake, restore_timeout = "2"), "`restore_timeout`")
})

test_that("rewind_track rejects things that are not reactiveValues", {
  fake <- list(userData = new.env(parent = emptyenv()))
  fake$userData$.rewind <- TRUE  # pretend rewind is enabled

  expect_error(rewind_track(list(a = 1), session = fake), "reactiveValues")
})

test_that("rewind_step is a no-op without a controller, and returns its value", {
  expect_equal(rewind_step(1 + 1, session = NULL), 2)
})

test_that("UI helpers build tags and carry the dependency", {
  btns <- rewind_buttons()
  expect_s3_class(btns, "shiny.tag")

  html <- as.character(btns)
  expect_match(html, "rewind-undo")
  expect_match(html, "rewind-redo")
  expect_match(html, "disabled")

  rail <- rewind_ui(label = "Steps", max_height = "10rem")
  rail_html <- as.character(rail)
  expect_match(rail_html, "rewind-rail")
  expect_match(rail_html, "Steps")
  expect_match(rail_html, "10rem")

  expect_match(as.character(rewind_ui(label = NULL)), "rewind-rail")
})

test_that("labels are omitted when NULL", {
  bare <- as.character(rewind_buttons(undo_label = NULL, redo_label = NULL))
  expect_false(grepl("rewind-label", bare))
})

test_that("button_class reaches the buttons, not the container", {
  html <- as.character(rewind_buttons(button_class = "btn-outline-primary btn-sm"))

  # Both buttons take the classes.
  expect_match(html, "btn btn-default btn-outline-primary btn-sm rewind-undo")
  expect_match(html, "btn btn-default btn-outline-primary btn-sm rewind-redo")

  # The container keeps only its own class. A class given to `class` goes
  # there instead, and the two arguments must not reach the same element.
  expect_match(html, '<div class="rewind-buttons">')

  container <- as.character(rewind_buttons(class = "my-toolbar"))
  expect_match(container, '<div class="rewind-buttons my-toolbar">')
  expect_false(grepl("my-toolbar rewind-undo", container))
})

test_that("the arrows are SVG, so they do not depend on the font", {
  html <- as.character(rewind_buttons())

  expect_match(html, "<svg")
  expect_match(html, 'class="rewind-icon"')

  # currentColor makes the arrow follow the colour of the button text, so a
  # Bootstrap variant or a dark theme needs no extra rule.
  expect_match(html, 'stroke="currentColor"')

  # The button already has an aria-label. A screen reader must not announce
  # the image as well.
  expect_match(html, 'aria-hidden="true"')

  # The old entities must not come back.
  expect_false(grepl("&#8630;", html, fixed = TRUE))
  expect_false(grepl("&#8631;", html, fixed = TRUE))
})

test_that("the undo and redo arrows point in opposite directions", {
  html <- as.character(rewind_buttons())

  # The redo arrow is the undo arrow mirrored: each x becomes 16 - x. A
  # copy-and-paste mistake would give two arrows that point the same way.
  expect_match(html, "M5.5 3 2.5 6l3 3", fixed = TRUE)   # undo head, points left
  expect_match(html, "M10.5 3 13.5 6l-3 3", fixed = TRUE) # redo head, points right
})

test_that("icon-only buttons still carry an accessible name", {
  bare <- as.character(rewind_buttons(undo_label = NULL, redo_label = NULL))
  expect_match(bare, 'aria-label="Undo"')
  expect_match(bare, 'aria-label="Redo"')

  custom <- as.character(rewind_buttons(undo_label = "Back", redo_label = "Forward"))
  expect_match(custom, 'aria-label="Back"')
  expect_match(custom, 'aria-label="Forward"')
})

test_that("the html dependency points at installed assets", {
  dep <- rewind_dependency()

  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$name, "rewind")
  expect_equal(dep$script, "rewind.js")
  expect_equal(dep$stylesheet, "rewind.css")
  expect_true(file.exists(file.path(dep$src$file, "rewind.js")))
  expect_true(file.exists(file.path(dep$src$file, "rewind.css")))
})


# --- Argument checks ------------------------------------------------------
#
# A plain `x < 0` test lets NA through to `if (NA)`, which stops with
# "missing value where TRUE/FALSE needed" and does not name the argument.
# It also lets Inf through, which then does harm with no message: an
# infinite coalesce_ms means no change is ever written to the history.

enable_with <- function(...) {
  args <- list(...)
  function(input, output, session) do.call(rewind_enable, args)
}

test_that("rewind_enable() refuses a bad coalesce_ms and names it", {
  for (v in list(NA_real_, NaN, Inf, -1, c(1, 2), "400")) {
    expect_error(
      shiny::testServer(enable_with(coalesce_ms = v), {}),
      "`coalesce_ms` must be a single finite number of 0 or more",
      info = paste("coalesce_ms =", deparse(v))
    )
  }
  # 0 is allowed: it turns grouping off.
  expect_no_error(shiny::testServer(enable_with(coalesce_ms = 0), {}))
})

test_that("rewind_enable() refuses a bad restore_timeout and names it", {
  for (v in list(NA_real_, NaN, Inf, 0, -1)) {
    expect_error(
      shiny::testServer(enable_with(restore_timeout = v), {}),
      "`restore_timeout` must be a single finite number greater than 0",
      info = paste("restore_timeout =", deparse(v))
    )
  }
  expect_no_error(shiny::testServer(enable_with(restore_timeout = 0.5), {}))
})

test_that("rewind_step() refuses a bad hold_ms at the call, not later", {
  step_with <- function(hold) {
    function(input, output, session) {
      rewind_enable()
      rewind_step(NULL, hold_ms = hold)
    }
  }
  for (v in list(NA_real_, Inf, -5)) {
    expect_error(
      shiny::testServer(step_with(v), {}),
      "`hold_ms` must be a single finite number of 0 or more",
      info = paste("hold_ms =", deparse(v))
    )
  }
  expect_no_error(shiny::testServer(step_with(NULL), {}))
  expect_no_error(shiny::testServer(step_with(0), {}))
})
