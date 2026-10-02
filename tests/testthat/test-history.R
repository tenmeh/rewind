st <- function(...) list(inputs = list(...), values = list())

test_that("a fresh stack cannot move in either direction", {
  h <- rewind:::History$new()

  expect_false(h$can_undo())
  expect_false(h$can_redo())
  expect_equal(h$size(), 0L)
  expect_null(h$current())
  expect_null(h$undo())
  expect_null(h$redo())
})

test_that("pushing advances the pointer and enables undo", {
  h <- rewind:::History$new()

  expect_true(h$push(st(a = 1)))
  expect_false(h$can_undo())      # nothing behind the first entry
  expect_true(h$push(st(a = 2)))
  expect_true(h$can_undo())
  expect_false(h$can_redo())
  expect_equal(h$size(), 2L)
})

test_that("undo and redo walk the stack", {
  h <- rewind:::History$new()
  h$push(st(a = 1))
  h$push(st(a = 2))
  h$push(st(a = 3))

  expect_equal(h$current()$inputs$a, 3)
  expect_equal(h$undo()$inputs$a, 2)
  expect_equal(h$undo()$inputs$a, 1)
  expect_false(h$can_undo())
  expect_null(h$undo())

  expect_equal(h$redo()$inputs$a, 2)
  expect_equal(h$redo()$inputs$a, 3)
  expect_false(h$can_redo())
  expect_null(h$redo())
})

test_that("pushing after undo discards the abandoned future", {
  h <- rewind:::History$new()
  h$push(st(a = 1))
  h$push(st(a = 2))
  h$push(st(a = 3))
  h$undo()
  h$undo()

  expect_true(h$can_redo())
  h$push(st(a = 99))

  expect_false(h$can_redo())
  expect_equal(h$size(), 2L)
  expect_equal(h$current()$inputs$a, 99)
})

test_that("identical states are not pushed", {
  h <- rewind:::History$new()
  expect_true(h$push(st(a = 1)))
  expect_false(h$push(st(a = 1)))
  expect_equal(h$size(), 1L)
})

test_that("this is how a restore echo gets absorbed", {
  # Undo restores state A, the browser echoes A back, and the echo must not
  # become a new history entry.
  h <- rewind:::History$new()
  h$push(st(a = 1))
  h$push(st(a = 2))

  restored <- h$undo()
  expect_equal(restored$inputs$a, 1)

  expect_false(h$push(restored))
  expect_equal(h$size(), 2L)
  expect_true(h$can_redo())
})

test_that("depth is enforced by dropping the oldest entries", {
  h <- rewind:::History$new(depth = 3L)
  for (i in 1:6) h$push(st(a = i))

  expect_equal(h$size(), 3L)
  expect_equal(h$current()$inputs$a, 6)

  expect_equal(h$undo()$inputs$a, 5)
  expect_equal(h$undo()$inputs$a, 4)
  expect_false(h$can_undo())
})

test_that("the pointer survives trimming", {
  h <- rewind:::History$new(depth = 3L)
  for (i in 1:3) h$push(st(a = i))
  h$undo()
  expect_equal(h$index(), 2L)

  h$push(st(a = 10))    # truncates entry 3, appends, still within depth
  expect_equal(h$index(), 3L)
  expect_equal(h$size(), 3L)
})

test_that("jump moves to an absolute position and rejects bad input", {
  h <- rewind:::History$new()
  for (i in 1:4) h$push(st(a = i))

  expect_equal(h$jump(2)$inputs$a, 2)
  expect_true(h$can_undo())
  expect_true(h$can_redo())

  expect_null(h$jump(0))
  expect_null(h$jump(99))
  expect_null(h$jump(NA))
  expect_equal(h$current()$inputs$a, 2)   # unmoved
})

test_that("clear keeps the present and drops the rest", {
  h <- rewind:::History$new()
  for (i in 1:4) h$push(st(a = i))
  h$undo()

  h$clear()
  expect_equal(h$size(), 1L)
  expect_equal(h$current()$inputs$a, 3)
  expect_false(h$can_undo())
  expect_false(h$can_redo())
})

test_that("entries() describes the stack", {
  h <- rewind:::History$new()
  h$push(st(a = 1), label = "first")
  h$push(st(a = 2), label = "second")
  h$undo()

  e <- h$entries()
  expect_s3_class(e, "data.frame")
  expect_equal(nrow(e), 2L)
  expect_equal(e$label, c("first", "second"))
  expect_equal(e$current, c(TRUE, FALSE))

  expect_equal(nrow(rewind:::History$new()$entries()), 0L)
})

test_that("depth is validated", {
  expect_error(rewind:::History$new(depth = 1L), "depth")
  expect_error(rewind:::History$new(depth = NA), "depth")
})


# --- scrub_inputs ----------------------------------------------------------
#
# rewind removes a password field from the history when the browser reports
# it. The first snapshot can hold the field already, so every stored state
# must lose it, and a step whose only change was that field must go.

test_that("scrub_inputs removes the inputs from every entry", {
  h <- rewind:::History$new()
  h$push(st(a = 1, pw = ""))
  h$push(st(a = 2, pw = ""))

  h$scrub_inputs("pw")

  expect_equal(h$size(), 2L)
  expect_false("pw" %in% names(h$current()$inputs))
  h$undo()
  expect_false("pw" %in% names(h$current()$inputs))
  expect_equal(h$current()$inputs$a, 1)
})

test_that("scrub_inputs removes a step that only changed the scrubbed input", {
  h <- rewind:::History$new()
  h$push(st(a = 1, pw = ""))
  h$push(st(a = 1, pw = "s3cret"))   # only the password changed
  h$push(st(a = 2, pw = "s3cret"))
  expect_equal(h$index(), 3L)

  h$scrub_inputs("pw")

  # The middle step is now the same as the first, so it is a step that does
  # nothing. It goes, and the position moves down with it.
  expect_equal(h$size(), 2L)
  expect_equal(h$index(), 2L)
  expect_equal(h$current()$inputs$a, 2)
})

test_that("scrub_inputs moves the position back when the current step goes", {
  h <- rewind:::History$new()
  h$push(st(a = 1, pw = ""))
  h$push(st(a = 1, pw = "s3cret"))
  expect_equal(h$index(), 2L)

  h$scrub_inputs("pw")

  expect_equal(h$size(), 1L)
  expect_equal(h$index(), 1L)
  expect_false(h$can_undo())
})

test_that("scrub_inputs does nothing to an empty stack or with no ids", {
  h <- rewind:::History$new()
  expect_silent(h$scrub_inputs("pw"))
  expect_equal(h$size(), 0L)

  h$push(st(a = 1))
  h$scrub_inputs(character(0))
  expect_equal(h$size(), 1L)
})
