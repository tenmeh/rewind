# Tests for rewind_diff() and the code behind it.
#
# The functions that do the work take plain lists and give a plain data
# frame, so most of this file needs no Shiny session. The tests at the end
# use shiny::testServer() to check the whole path, including the reactive
# dependency and the names that rewind_track() produces.
#
# Refer to the note at the top of test-controller.R about the virtual clock
# of testServer and the real clock that the grouping period uses.

# --- format_value --------------------------------------------------------

test_that("format_value shows a short value as it is", {
  expect_equal(format_value("North"), "North")
  expect_equal(format_value(42), "42")
  expect_equal(format_value(TRUE), "TRUE")
})

test_that("format_value lists a few values, and counts many", {
  expect_equal(format_value(c(2018, 2026)), "2018, 2026")
  expect_equal(format_value(c("a", "b", "c")), "a, b, c")
  # Above max_items, so a count and not a list.
  expect_equal(format_value(1:12), "12 values")
  # Within max_items but too wide, so a count as well.
  expect_equal(format_value(c(strrep("x", 30), strrep("y", 30))), "2 values")
})

test_that("format_value cuts one long value rather than counting it", {
  long <- strrep("a", 60)
  out <- format_value(long)
  expect_equal(nchar(out), 40L)
  expect_true(endsWith(out, "..."))
  # A single value must never be reported as "1 values".
  expect_false(grepl("values", out, fixed = TRUE))
})

test_that("format_value names the empty and the missing cases apart", {
  expect_equal(format_value(NULL), "(none)")
  expect_equal(format_value(character(0)), "(empty)")
  expect_equal(format_value(NA), "NA")
})

test_that("format_value describes a value that is not one line of text", {
  expect_equal(format_value(data.frame(a = 1:3)), "a table of 3 rows")
  expect_equal(format_value(data.frame(a = 1)), "a table of 1 row")
  expect_equal(format_value(list(1, 2)), "a list of 2")
})

test_that("format_value renders a date as text", {
  expect_equal(format_value(as.Date("2026-09-16")), "2026-09-16")
})


# --- format_change -------------------------------------------------------

test_that("format_change shows both sides of a change", {
  expect_equal(format_change("North", "South"), "North -> South")
})

test_that("format_change names an added and a removed value", {
  expect_equal(format_change(NULL, "South"), "set to South")
  expect_equal(format_change("North", NULL), "cleared")
})


# --- diff_states ---------------------------------------------------------

make_states <- function() {
  list(
    old = list(region = "North", year = c(2018, 2026), active = TRUE),
    new = list(region = "South", year = c(2018, 2026), active = FALSE)
  )
}

test_that("diff_states reports only the values that differ", {
  s <- make_states()
  out <- diff_states(s$old, s$new, from = 1L, to = 2L)

  # `year` is the same in both, so it must not appear.
  expect_equal(out$name, c("active", "region"))
  expect_equal(nrow(out), 2L)
})

test_that("diff_states orders the rows by name", {
  out <- diff_states(list(b = 1, a = 1), list(b = 2, a = 2))
  expect_equal(out$name, c("a", "b"))
})

test_that("diff_states carries the positions it was given", {
  s <- make_states()
  out <- diff_states(s$old, s$new, from = 3L, to = 7L)
  expect_equal(unique(out$from), 3L)
  expect_equal(unique(out$to), 7L)
})

test_that("diff_states keeps the values themselves, and not only the text", {
  out <- diff_states(
    list(year = c(2018, 2026)),
    list(year = c(2020, 2024))
  )
  # The list columns must hold the real objects, of the real type.
  expect_equal(out$old[[1]], c(2018, 2026))
  expect_equal(out$new[[1]], c(2020, 2024))
  expect_type(out$old, "list")
  expect_equal(out$change, "2018, 2026 -> 2020, 2024")
})

test_that("diff_states finds a value that only one side has", {
  out <- diff_states(list(a = 1), list(a = 1, b = 2))
  expect_equal(out$name, "b")
  expect_equal(out$change, "set to 2")
  expect_null(out$old[[1]])
})

test_that("diff_states gives no rows when nothing differs", {
  s <- make_states()
  out <- diff_states(s$old, s$old)
  expect_equal(nrow(out), 0L)
})

test_that("diff_states accepts NULL on either side", {
  expect_equal(nrow(diff_states(NULL, NULL)), 0L)
  expect_equal(diff_states(NULL, list(a = 1))$change, "set to 1")
  expect_equal(diff_states(list(a = 1), NULL)$change, "cleared")
})

test_that("an empty result has the same shape as a full one", {
  # An application binds these together and indexes the columns without a
  # test for zero rows first. The shapes must therefore agree.
  full <- diff_states(list(a = 1), list(a = 2))
  empty <- empty_diff()

  expect_equal(names(empty), names(full))
  expect_equal(names(empty), c("from", "to", "name", "change", "old", "new"))
  expect_equal(
    vapply(empty, class, character(1)),
    vapply(full, class, character(1))
  )
  expect_equal(nrow(rbind(empty, full)), 1L)
})


# --- rewind_diff ---------------------------------------------------------

srv_diff <- function(input, output, session) {
  rewind_enable(coalesce_ms = 30)
  state <- shiny::reactiveValues(zoom = 1)
  rewind_track(state, fields = "zoom", id = "st")
}

settle <- function(session, ...) {
  session$setInputs(...)
  Sys.sleep(0.15)
  session$elapse(200)
}

test_that("rewind_diff compares the current step with the one before it", {
  shiny::testServer(srv_diff, {
    settle(session, region = "North")
    settle(session, region = "South")

    out <- rewind_diff()
    expect_equal(out$name, "region")
    expect_equal(out$change, "North -> South")
    expect_equal(out$old[[1]], "North")
    expect_equal(out$new[[1]], "South")
    expect_equal(out$from, 1L)
    expect_equal(out$to, 2L)
  })
})

test_that("rewind_diff gives no rows when there is no earlier step", {
  shiny::testServer(srv_diff, {
    settle(session, region = "North")

    # One entry only, so there is nothing to compare with. This is a normal
    # condition and must not be an error.
    expect_equal(nrow(rewind_diff()), 0L)
  })
})

test_that("rewind_diff compares any two positions", {
  shiny::testServer(srv_diff, {
    settle(session, region = "North")
    settle(session, region = "South")
    settle(session, region = "East")

    # Skip the middle step. The result describes the whole distance.
    out <- rewind_diff(from = 1, to = 3)
    expect_equal(out$name, "region")
    expect_equal(out$change, "North -> East")
  })
})

test_that("rewind_diff refuses a position that does not exist", {
  shiny::testServer(srv_diff, {
    settle(session, region = "North")
    settle(session, region = "South")

    expect_error(rewind_diff(from = 1, to = 99), "must be between 1 and 2")
    expect_error(rewind_diff(from = 0, to = 2), "must be between 1 and 2")
    expect_error(rewind_diff(from = "x", to = 2), "single whole number")
  })
})

test_that("rewind_diff names a tracked value as id$field", {
  shiny::testServer(srv_diff, {
    settle(session, region = "North")

    # A value that rewind_track() records is not an input. flatten_state()
    # gives it the id$field form, and the diff must use the same name.
    # testServer evaluates this block in the environment of the server
    # function, so `state` here is the same object rewind_track() was
    # given.
    # An assignment to reactiveValues does not start a reactive flush in
    # testServer, so the capture observer would not see it. Refer to the
    # same note in test-controller.R.
    state$zoom <- 4
    session$flushReact()
    Sys.sleep(0.15)
    session$elapse(200)

    out <- rewind_diff()
    expect_equal(out$name, "st$zoom")
    expect_equal(out$change, "1 -> 4")
    expect_equal(out$new[[1]], 4)
  })
})

test_that("rewind_diff follows the position after an undo", {
  shiny::testServer(srv_diff, {
    settle(session, region = "North")
    settle(session, region = "South")
    settle(session, region = "East")

    ctrl <- session$userData$.rewind
    ctrl$undo()

    # The position is now 2, so the default pair is 1 and 2.
    out <- rewind_diff()
    expect_equal(out$to, 2L)
    expect_equal(out$change, "North -> South")
  })
})

test_that("rewind_diff needs rewind to be enabled", {
  shiny::testServer(function(input, output, session) {}, {
    expect_error(rewind_diff(), "rewind_enable")
  })
})


# --- check_index ---------------------------------------------------------

test_that("check_index refuses a number that is not whole, rather than truncating it", {
  # as.integer(2.7) is 2, so this used to pass quietly as position 2.
  expect_error(check_index(2.7, "to"), "`to` must be a single whole number")
  expect_error(check_index(Inf, "to"), "single whole number")
  expect_error(check_index(NA, "to"), "single whole number")
  expect_error(check_index(c(1, 2), "to"), "single whole number")
  expect_identical(check_index(3, "to"), 3L)
  expect_identical(check_index(3L, "to"), 3L)
})
