library(shiny)

ui <- fluidPage(
  sidebarPanel(
    selectInput("Type", "Type", c(A = "A", B = "B")),
    conditionalPanel(
      condition = "input.Type == 'A'",
      sliderInput(inputId = "hiA", label = "", min = 0, max = 10, value = 5, step = 1)
    ),
    conditionalPanel(
      condition = "input.Type == 'B'",
      sliderInput(inputId = "hiB", label = "", min = 10, max = 100, value = 50, step = 1)
    ),
  ),
  mainPanel(
    verbatimTextOutput(outputId = "text")
  )
)

server <- function(input, output) {
  output$text <- renderPrint({
    switch(input$Type,
           A = input$hiA,
           B = input$hiB
    )
  })
}

shinyApp(ui, server)