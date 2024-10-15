library(shiny)

ui <- fluidPage(
  titlePanel("Basic Shiny App"),
  sidebarLayout(
    sidebarPanel(
      numericInput("inputValue", "Input Value:", value = 0)
    ),
    mainPanel(
      textOutput("outputText")
    )
  )
)

server <- function(input, output) {
  output$outputText <- renderText({
    paste("You entered:", input$inputValue)
  })
}

shinyApp(ui = ui, server = server)