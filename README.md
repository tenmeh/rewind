# rewind

**Undo and redo for Shiny applications.**

Shiny has bookmarking. It captures a state that you can send as a link.
Shiny also has `reactlog`. It lets *you* replay a session while you debug.
But Shiny has no undo. Users expect undo, because each other application
has it.

`rewind` adds undo in one line.

```r
library(shiny)
library(rewind)

ui <- fluidPage(
  rewind_buttons(),
  selectInput("region", "Region", c("North", "South", "East", "West")),
  sliderInput("year", "Year", 2018, 2026, 2024),
  plotOutput("plot")
)

server <- function(input, output, session) {
  rewind_enable()          # <- that's it

  output$plot <- renderPlot(plot_for(input$region, input$year))
}

shinyApp(ui, server)
```

`Ctrl+Z` now moves backwards through the filter choices of the user.
`Ctrl+Shift+Z` moves forwards.

## Installation

```r
# install.packages("remotes")
remotes::install_github("tenmeh/rewind")
```

Then run the demo:

```r
shiny::runApp(system.file("examples/demo", package = "rewind"))
```

## Building from source

roxygen2 makes `NAMESPACE` and `man/`. Do not edit those files. Edit the
roxygen comments above each function. Then make the documents again:

```r
devtools::document()      # rewrites NAMESPACE and man/ from the roxygen comments
devtools::test()          # runs tests/testthat
devtools::check()         # full R CMD check
```

## What it does

### Grouping, so that undo is useful

One movement of a slider sends many input events. An undo of each event is
of no use. Changes that occur within `coalesce_ms` (the default is 400) of
each other become one history entry. One movement is thus one undo step.

### Your own steps, when time is not the correct limit

A "reset filters" button changes four inputs together. It must make one
entry, with a name that a person wrote:

```r
observeEvent(input$reset, {
  rewind_step(label = "Reset filters", {
    updateSelectInput(session, "region", selected = "All")
    updateSliderInput(session, "year", value = c(2018, 2026))
    updateCheckboxInput(session, "active_only", value = FALSE)
  })
})
```

### State on the server, and not only inputs

`rewind` cannot see the values that you keep in `reactiveValues`. Register
them:

```r
state <- reactiveValues(pinned = character(0), zoom = 1)
rewind_track(state, fields = c("pinned", "zoom"))
```

### A history rail

`rewind_ui()` draws the stack as a scrubbable list. Click a step to move to
it. The labels come from the values that changed. The rail thus shows
`region, year` and not `state 7`.

```r
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(rewind_ui()),
    mainPanel(...)
  )
)
```

## How restore works

Read this part if you intend to extend the package.

To restore a value, you must put it back in the widget. The simple method
is a lookup table: `sliderInput` -> `updateSliderInput`, `selectInput` ->
`updateSelectInput`, and one entry for each other input type. That table is
never complete. It fails when a person uses a widget from a package that
you do not know.

`rewind` thus has no table. Each Shiny input registers an **input binding**
on its DOM element. Each binding gives a `setValue()` or a
`receiveMessage()` function. The code in the browser finds the binding and
calls that function:

```js
var binding = $el.data("shiny-input-binding");
if (typeof binding.setValue === "function") {
  binding.setValue(el, value);
} else {
  binding.receiveMessage(el, { value: value });
}
```

That is the full restore procedure. It also operates on inputs from other
packages, with no extra code.

### The echo problem

A restore sends the value to the browser. The browser sets the value. It
then sends the new value back to the server. There, the capture observer
sees a change and writes a new history entry. This is a loop. Packages of
this type usually fail because of it.

Two methods stop the loop. They overlap on purpose:

1. **Dedup.** The history does not accept a state that is the same as the
   current state. A restore moves the position to state S *before* the echo
   arrives. The echo is thus the same as the current state, and the history
   drops it. This method stops the loop in the usual conditions, and it
   needs no other code.
2. **An expectation guard.** The controller keeps the state that it sent to
   the browser. It ignores the states between until the echo agrees, or
   until two seconds pass. This method stops the loop when the inputs arrive
   in different flushes. Those inputs make a partial state, and dedup does
   not find it.

Neither method assumes a time for the trip to the browser and back. The
package is thus dependable, and not only usually correct.

## What rewind does not capture

- **Action buttons and links.** Their value is a click counter. A restore
  does nothing, or it starts the observers again by mistake.
- **`fileInput()`.** Its value points to a temporary file on the server.
  Shiny deletes that file at the next upload. An old snapshot would thus
  point to a file that does not exist.
- **Inputs with names that start with `.`**. These belong to Shiny.
- **Inputs with names that start with `rewind_`**. These belong to this
  package.

## Known limits

These are the limits of the package. Read them before you start:

- **Values go through JSON.** Dates come back as text, and integers come
  back as doubles. The comparison accepts this. But an input that holds an
  unusual R object does not survive. Keep such state in `reactiveValues`
  and track it there. No serialisation occurs there.
- **Modules.** You can call `rewind_enable()` inside a `moduleServer()`.
  This operation has tests. `rewind` captures the inputs with their
  module-local names. These are the same names that `input$` uses inside
  the module. `rewind` adds the namespace with `session$ns()` at a restore.
  All modules share `session$userData`. A call in a second module thus uses
  the same history. Call the function once, at the position in the module
  tree that is best for your application.
- **Undo does not undo side effects.** An observer can write to a database
  when a filter changes. An undo then changes the filter, but not the
  database.
- **Inputs that `renderUI` makes.** `rewind` captures them after they
  exist. An undo to a state before they existed keeps their current values.

## API

| Function | Purpose |
|---|---|
| `rewind_enable()` | Turn on capture for the session |
| `rewind_track()` | Add `reactiveValues` fields to the history |
| `rewind_step()` | Group changes into one labelled entry |
| `rewind_undo()`, `rewind_redo()`, `rewind_jump()` | Drive the history yourself |
| `rewind_clear()` | Drop everything but the present |
| `rewind_pause()`, `rewind_resume()` | Suspend capture around programmatic churn |
| `rewind_disable()` | Fully tear down capture for the session |
| `rewind_history()`, `rewind_can_undo()`, `rewind_can_redo()` | Inspect, reactively |
| `rewind_buttons()`, `rewind_ui()` | Drop-in UI |

## License

MIT
