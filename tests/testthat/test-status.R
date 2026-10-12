# test-status.R ----------------------------------------------------------
#
# rewind_status() and rewind_exclude().
#
# Refer to the note at the top of test-controller.R about the virtual clock
# of testServer and the real clock that the grouping period uses.

settle_status <- function(session, ...) {
  session$setInputs(...)
  Sys.sleep(0.15)
  session$elapse(250)
}

# --- rewind_status --------------------------------------------------------

test_that("rewind_status names each input and the reason it is not captured", {
  shiny::testServer(
    function(input, output, session) {
      rewind_enable(exclude = "search", coalesce_ms = 30)
    },
    {
      settle_status(
        session,
        region = "North",
        search = "abc",
        pw = "x",
        multi = NULL
      )
      settle_status(session, rewind_secret_ids = "pw")

      s <- rewind_status()
      expect_equal(s$captured, "region")
      expect_equal(s$skipped[["search"]], "excluded")
      expect_equal(s$skipped[["pw"]], "password field")
      # rewind's own inputs are not listed.
      expect_false(any(startsWith(names(s$skipped), "rewind_")))
      expect_equal(s$size, 1L)
      expect_equal(s$position, 1L)
      expect_false(s$paused)
      expect_null(s$restorable)
    }
  )
})

test_that("rewind_status shows the inputs the page cannot restore", {
  shiny::testServer(
    function(input, output, session) {
      rewind_enable(coalesce_ms = 30)
    },
    {
      settle_status(session, region = "North", hov = 1)
      settle_status(session, rewind_bound_ids = "region")

      s <- rewind_status()
      expect_equal(s$restorable, "region")
      expect_equal(s$skipped[["hov"]], "cannot be restored by the page")
    }
  )
})

test_that("rewind_status shows an input missing from `inputs`, and tracked values", {
  shiny::testServer(
    function(input, output, session) {
      rewind_enable(inputs = "region", coalesce_ms = 30)
      st <- shiny::reactiveValues(zoom = 1)
      rewind_track(st, fields = "zoom")
    },
    {
      settle_status(session, region = "North", year = 2020)
      s <- rewind_status()
      expect_equal(s$skipped[["year"]], "not in inputs")
      expect_equal(s$tracked, list(st = "zoom"))
    }
  )
})

# --- rewind_exclude ---------------------------------------------------------

test_that("rewind_exclude leaves an input out, and removes what was stored", {
  shiny::testServer(
    function(input, output, session) {
      rewind_enable(coalesce_ms = 30)
    },
    {
      ctrl <- session$userData$.rewind
      settle_status(session, region = "North", search = "a")
      settle_status(session, search = "ab") # a step that only changed search
      expect_equal(ctrl$history$size(), 2L)

      expect_invisible(rewind_exclude("search"))

      # The value is gone, and so is the step that only changed it.
      expect_equal(ctrl$history$size(), 1L)
      expect_false("search" %in% names(ctrl$history$current()$inputs))

      # Later changes are not steps.
      settle_status(session, search = "abc")
      expect_equal(ctrl$history$size(), 1L)
      expect_equal(rewind_status()$skipped[["search"]], "excluded")
    }
  )
})

test_that("a second call adds to the list", {
  shiny::testServer(
    function(input, output, session) {
      rewind_enable(exclude = "a", coalesce_ms = 30)
      rewind_exclude("b")
      rewind_exclude("c")
    },
    {
      settle_status(session, a = 1, b = 1, c = 1, d = 1)
      expect_equal(rewind_status()$captured, "d")
    }
  )
})

test_that("a call before rewind_enable() is applied when it runs", {
  shiny::testServer(
    function(input, output, session) {
      rewind_exclude("search")
      rewind_enable(coalesce_ms = 30)
    },
    {
      settle_status(session, region = "North", search = "a")
      expect_equal(rewind_status()$skipped[["search"]], "excluded")
      expect_null(session$userData$.rewind_exclude)
    }
  )
})

test_that("inside a module, the module's own name is used", {
  search <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      rewind_exclude("query")
    })
  }
  shiny::testServer(
    function(input, output, session) {
      rewind_enable(coalesce_ms = 30)
      search("find")
    },
    {
      settle_status(session, region = "North", `find-query` = "a")
      s <- rewind_status()
      expect_equal(s$skipped[["find-query"]], "excluded")
      expect_equal(s$captured, "region")
    }
  )
})

test_that("rewind_exclude refuses names that are not text", {
  shiny::testServer(function(input, output, session) rewind_enable(), {
    expect_error(
      rewind_exclude(1),
      "`names` must be a character vector.",
      fixed = TRUE
    )
  })
})
