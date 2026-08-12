# rewind demo -----------------------------------------------------------
#
# Run with:
#   shiny::runApp(system.file("examples/demo", package = "rewind"))
#
# Things to try:
#   * change a few filters, then press Ctrl+Z (Cmd+Z on macOS)
#   * drag the year slider around wildly, then undo once: the whole drag is
#     a single step, not forty
#   * click "Reset filters": three inputs change, but it is one undo step
#   * pin a customer, undo, redo: the pin list lives in reactiveValues, not in
#     an input, so it is tracked explicitly with rewind_track()
#   * click any step in the history rail to jump straight to it
#   * type in the search box and press Ctrl+Z: native text undo still works,
#     because rewind stays out of the way while you are typing

library(shiny)
library(rewind)

# A small synthetic dataset so the demo has no data dependencies.
set.seed(1)
regions <- c("North", "South", "East", "West")
sales <- data.frame(
  customer = paste("Customer", sprintf("%03d", 1:200)),
  region   = sample(regions, 200, replace = TRUE),
  year     = sample(2018:2026, 200, replace = TRUE),
  revenue  = round(stats::rlnorm(200, 10, 0.8)),
  active   = sample(c(TRUE, FALSE), 200, replace = TRUE, prob = c(0.7, 0.3)),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  tags$style(HTML("
    body { padding: 1.5rem; }
    .toolbar { display: flex; align-items: center; gap: 1rem; margin-bottom: 1rem; }
    .hint { opacity: 0.6; font-size: 0.85rem; }
  ")),

  titlePanel("rewind demo"),

  div(
    class = "toolbar",
    rewind_buttons(),
    span(class = "hint", "or press Ctrl/Cmd + Z")
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput("region", "Region", choices = c("All", regions)),
      sliderInput("year", "Years", 2018, 2026, c(2018, 2026), sep = ""),
      checkboxInput("active_only", "Active customers only", FALSE),
      textInput("search", "Search customer", placeholder = "e.g. 042"),
      div(
        actionButton("reset", "Reset filters", class = "btn-default"),
        actionButton("pin", "Pin top customer", class = "btn-default")
      ),
      hr(),
      rewind_ui(label = "History", max_height = "16rem")
    ),

    mainPanel(
      width = 8,
      h4(textOutput("summary", inline = TRUE)),
      p(class = "hint", textOutput("selection", inline = TRUE)),
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {

  # One line. Everything below is ordinary Shiny.
  rewind_enable(
    inputs      = c("region", "year", "active_only", "search"),
    depth       = 40,
    coalesce_ms = 400
  )

  # Server-side state is invisible to rewind until registered.
  state <- reactiveValues(pinned = character(0))
  rewind_track(state, fields = "pinned", id = "pins")

  filtered <- reactive({
    out <- sales
    if (input$region != "All")   out <- out[out$region == input$region, ]
    out <- out[out$year >= input$year[1] & out$year <= input$year[2], ]
    if (isTRUE(input$active_only)) out <- out[out$active, ]
    if (nzchar(input$search)) {
      out <- out[grepl(input$search, out$customer, fixed = TRUE), ]
    }
    out[order(-out$revenue), ]
  })

  # Several changes, one undo step, one meaningful label.
  observeEvent(input$reset, {
    rewind_step(label = "Reset filters", {
      updateSelectInput(session, "region", selected = "All")
      updateSliderInput(session, "year", value = c(2018, 2026))
      updateCheckboxInput(session, "active_only", value = FALSE)
      updateTextInput(session, "search", value = "")
    })
  })

  observeEvent(input$pin, {
    top <- head(filtered()$customer, 1)
    if (length(top) && !top %in% state$pinned) {
      rewind_step(label = paste("Pin", top), {
        state$pinned <- c(state$pinned, top)
      })
    }
  })

  output$summary <- renderText({
    df <- filtered()
    sprintf(
      "%s customers, %s total revenue",
      format(nrow(df), big.mark = ","),
      format(sum(df$revenue), big.mark = ",")
    )
  })

  output$selection <- renderText({
    if (length(state$pinned) == 0L) {
      "Nothing pinned."
    } else {
      paste("Pinned:", paste(state$pinned, collapse = ", "))
    }
  })

  output$table <- renderTable({
    head(filtered(), 15)
  }, digits = 0)
}

shinyApp(ui, server)
