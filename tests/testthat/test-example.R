# test-example.R ---------------------------------------------------------
#
# rewind_example() starts the demo with shiny::runApp(). A real start would
# block the test, so runApp() is replaced here, and the test reads what it
# was given.

test_that("rewind_example() runs the demo folder, with the arguments it gets", {
  got <- NULL
  testthat::local_mocked_bindings(
    runApp = function(app_dir, ...) {
      got <<- list(dir = app_dir, args = list(...))
      invisible(NULL)
    },
    .package = "shiny"
  )

  rewind_example(port = 1234, launch.browser = FALSE)

  expect_equal(basename(got$dir), "demo")
  expect_true(file.exists(file.path(got$dir, "app.R")))
  expect_equal(got$args, list(port = 1234, launch.browser = FALSE))
})
