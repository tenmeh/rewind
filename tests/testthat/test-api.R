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
