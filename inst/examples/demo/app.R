# rewind demo -----------------------------------------------------------
#
# Run with:
#   shiny::runApp(system.file("examples/demo", package = "rewind"))
#
# Try these operations:
#   * change some filters. Then press Ctrl+Z (Cmd+Z on macOS).
#   * move the year slider quickly. Then do one undo. The full movement is
#     one step, and not forty steps.
#   * click "Reset filters". Three inputs change, but it is one undo step.
#   * pin a customer. Then do an undo and a redo. The pin list is in
#     reactiveValues and not in an input. rewind_track() thus records it.
#   * click a step in the history rail to move to that step.
#   * type in the search box and press Ctrl+Z. The text undo of the browser
#     continues to operate, because rewind does nothing while you type.

library(shiny)
library(rewind)

# A small set of test data. The demo thus needs no external data.
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

  # This is the only line that rewind needs. The code below is normal Shiny.
  rewind_enable(
    inputs      = c("region", "year", "active_only", "search"),
    depth       = 40,
    coalesce_ms = 400
  )

  # rewind cannot see the state on the server. You must register it.
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

  # Several changes become one undo step with a clear label.
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
