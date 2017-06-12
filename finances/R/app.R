library(shiny)


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  titlePanel(""),
  fluidRow(
    column(2, strong("Date: ")),
    column(10, textOutput("date"))
  ),
  fluidRow(
    column(2, strong("Quantity: ")),
    column(10, textOutput("quantity"))
  ),
  fluidRow(
    column(2, strong("Comment: ")),
    column(10, textOutput("comment"))
  ),
  fluidRow(
    column(6, selectizeInput("payee", "Payee",
                             sort(unique(skrooge_transactions$payee)),
                             options = list(create = TRUE))),
    column(6, selectizeInput("category", "Category",
                             sort(unique(skrooge_transactions$category)),
                             options = list(create = TRUE)))
  ),
  fluidRow(
    column(1, actionButton("nextRow", "Submit & Next")),
    column(1, actionButton("skip", "Skip")),
    column(10, sliderInput("row", "Row", min = 1, max = nrow(candidates),
                           value = 1, step = 1)),
    column(2,   textOutput("change"), textOutput("skip"), textOutput("init"))
  )
))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  row <- reactive({ candidates[input$row, ] })

  output$date <- renderText({ as.character(row()$date) })
  output$quantity <- renderText({ row()$quantity })
  output$comment <- renderText({ row()$comment })

  output$init <- renderText({
    updateSelectizeInput(session, "payee", selected = row()$payee)
    updateSelectizeInput(session, "category", selected = row()$category)
    ""
  })

  output$change <- renderText({
    if (input$nextRow == 0) return()

    current <- as.integer(isolate(input$row))
    candidates[current, "payee"] <<- isolate(input$payee)
    candidates[current, "category"] <<- isolate(input$category)
    cat("update", current, "\n")

    updateSliderInput(session, "row", value = current + 1)
    ""
  })

  output$skip <- renderText({
    if (input$skip == 0) return()
    current <- isolate(input$row)
    updateSliderInput(session, "row", value = current + 1)
    ""
  })

}

shinyApp(ui = ui, server = server)
