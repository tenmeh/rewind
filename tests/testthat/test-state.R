test_that("states_equal is order insensitive", {
  expect_true(rewind:::states_equal(list(a = 1, b = 2), list(b = 2, a = 1)))
  expect_false(rewind:::states_equal(list(a = 1, b = 2), list(a = 1, b = 3)))
  expect_false(rewind:::states_equal(list(a = 1), list(a = 1, b = 2)))
})

test_that("states_equal survives the JSON round trip", {
  # An integer captured in R comes back from the browser as a double, and a
  # length-one vector may lose its names. Neither is a real change.
  expect_true(rewind:::values_equal(1L, 1))
  expect_true(rewind:::values_equal(c(a = 1), 1))
  expect_true(rewind:::values_equal(c(1, 10), c(1.0, 10.0)))

  expect_false(rewind:::values_equal(1, 2))
  expect_false(rewind:::values_equal(c(1, 2), c(1, 2, 3)))
})

test_that("dates compare by their rendering", {
  expect_true(rewind:::values_equal(as.Date("2026-01-01"), as.Date("2026-01-01")))
  expect_true(rewind:::values_equal(as.Date("2026-01-01"), "2026-01-01"))
  expect_false(rewind:::values_equal(as.Date("2026-01-01"), as.Date("2026-01-02")))
})

test_that("NULL handling is not accidentally lossy", {
  expect_true(rewind:::values_equal(NULL, NULL))
  expect_false(rewind:::values_equal(NULL, 1))
  expect_false(rewind:::values_equal(1, NULL))
  expect_true(rewind:::states_equal(NULL, NULL))
  expect_false(rewind:::states_equal(NULL, list(a = 1)))
})

test_that("nested value groups compare recursively", {
  a <- list(inputs = list(x = 1), values = list(rv = list(sel = c("a", "b"))))
  b <- list(inputs = list(x = 1), values = list(rv = list(sel = c("a", "b"))))
  c_ <- list(inputs = list(x = 1), values = list(rv = list(sel = c("a", "z"))))

  expect_true(rewind:::states_equal(a, b))
  expect_false(rewind:::states_equal(a, c_))
})

test_that("is_file_input_value matches fileInput()'s documented shape", {
  file_val <- data.frame(
    name = "a.csv", size = 12, type = "text/csv",
    datapath = "/tmp/0x1.csv", stringsAsFactors = FALSE
  )
  expect_true(rewind:::is_file_input_value(file_val))

  multi <- data.frame(
    name = c("a.csv", "b.csv"), size = c(12, 34),
    type = c("text/csv", "text/csv"),
    datapath = c("/tmp/0x1.csv", "/tmp/0x2.csv"), stringsAsFactors = FALSE
  )
  expect_true(rewind:::is_file_input_value(multi))

  expect_false(rewind:::is_file_input_value(data.frame(x = 1)))
  expect_false(rewind:::is_file_input_value(list(name = "a", size = 1)))
  expect_false(rewind:::is_file_input_value(1))
  expect_false(rewind:::is_file_input_value(NULL))
  expect_false(rewind:::is_file_input_value("a.csv"))
})

test_that("flatten_state namespaces tracked values", {
  s <- list(inputs = list(x = 1), values = list(rv = list(sel = "a", zoom = 2)))
  flat <- rewind:::flatten_state(s)

  expect_setequal(names(flat), c("x", "rv$sel", "rv$zoom"))
  expect_equal(flat[["rv$zoom"]], 2)
  expect_null(rewind:::flatten_state(NULL))
})

test_that("diff_label names what changed", {
  expect_equal(rewind:::diff_label(NULL, list(a = 1)), "Initial state")
  expect_equal(rewind:::diff_label(list(a = 1), list(a = 1)), "No change")
  expect_equal(rewind:::diff_label(list(a = 1, b = 2), list(a = 9, b = 2)), "a")
  expect_equal(
    rewind:::diff_label(list(a = 1, b = 2), list(a = 9, b = 8)),
    "a, b"
  )
})

test_that("diff_label truncates long change lists", {
  old <- list(a = 1, b = 1, c = 1, d = 1, e = 1)
  new <- list(a = 2, b = 2, c = 2, d = 2, e = 2)

  expect_equal(rewind:::diff_label(old, new), "a, b, c and 2 more")
  expect_equal(rewind:::diff_label(old, new, max_names = 5L), "a, b, c, d, e")
})

test_that("diff_label notices additions and removals", {
  expect_equal(rewind:::diff_label(list(a = 1), list(a = 1, b = 2)), "b")
  expect_equal(rewind:::diff_label(list(a = 1, b = 2), list(a = 1)), "b")
})

test_that("describe_state gives a correct default label", {
  expect_equal(rewind:::describe_state(list()), "Empty state")
  expect_equal(rewind:::describe_state(list(a = 1)), "1 value changed")
  expect_equal(rewind:::describe_state(list(a = 1, b = 2)), "2 values changed")
})
