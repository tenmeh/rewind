# test-labels.R ----------------------------------------------------------
#
# rewind_enable(labels = ) gives the names a person reads, in the rail and
# in rewind_diff(). Without it both show the input id, such as "region",
# because Shiny does not give the server the label of a widget.

settle_labels <- function(session, ...) {
  session$setInputs(...)
  Sys.sleep(0.15)
  session$elapse(250)
}

labelled_server <- function(input, output, session) {
  rewind_enable(
    coalesce_ms = 30,
    labels = c(region = "Region", year = "Year", "st$zoom" = "Zoom")
  )
  st <- shiny::reactiveValues(zoom = 1)
  rewind_track(st, fields = "zoom")
}

test_that("the rail shows the labels instead of the ids", {
  shiny::testServer(labelled_server, {
    ctrl <- session$userData$.rewind
    settle_labels(session, region = "North", year = 2020, other = 1)
    settle_labels(session, region = "South", year = 2024)
    expect_equal(ctrl$history$entries()$label[2], "Region, Year")

    # An input with no label keeps its id.
    settle_labels(session, other = 2)
    expect_equal(ctrl$history$entries()$label[3], "other")
  })
})

test_that("a tracked value takes its label from the id$field name", {
  shiny::testServer(labelled_server, {
    ctrl <- session$userData$.rewind
    settle_labels(session, region = "North")
    st$zoom <- 4
    session$flushReact()
    Sys.sleep(0.15)
    session$elapse(250)
    expect_equal(ctrl$history$entries()$label[2], "Zoom")
  })
})

test_that("rewind_diff adds a label column at the end, and keeps name as the id", {
  shiny::testServer(labelled_server, {
    settle_labels(session, region = "North", other = 1)
    settle_labels(session, region = "South", other = 2)
    out <- rewind_diff()
    expect_equal(
      names(out),
      c("from", "to", "name", "change", "old", "new", "label")
    )
    expect_equal(out$name, c("other", "region"))
    expect_equal(out$label, c("other", "Region"))
  })
})

test_that("without labels, the label column repeats the id", {
  shiny::testServer(
    function(input, output, session) rewind_enable(coalesce_ms = 30),
    {
      settle_labels(session, region = "North")
      settle_labels(session, region = "South")
      expect_equal(rewind_diff()$label, "region")
    }
  )
})

test_that("labels must be a named character vector", {
  bad <- list(
    c("Region"),
    c(region = 1),
    c(region = NA_character_),
    stats::setNames("x", ""),
    c(a = "A", a = "B")
  )
  for (v in bad) {
    expect_error(
      shiny::testServer(
        function(input, output, session) rewind_enable(labels = v),
        {}
      ),
      "`labels` must be a character vector with a unique name for each element",
      fixed = TRUE,
      info = deparse(v)
    )
  }
})
