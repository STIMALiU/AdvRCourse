library(shiny)

ui <- fluidPage(
  
  textInput(inputId = "curr", "Currency", "SEK"),
  numericInput(inputId = "p", 
               label = "Principal", 
               value = 100, min = 0),
  numericInput(inputId = "r", 
               label = "Interest Rate %", 
               value = 1, min = 0),
  numericInput(inputId = "t", 
               label = "Holding Period", 
               value = 1, min = 0),  
  textOutput(outputId = "amt")
)

server <- function(input, output) {
  
  int <- reactive(input$p*input$r*input$t/100)
  output$amt <- renderText({
    paste("Amount at Maturity (Rounded to 2 decimal places):", 
      input$curr, as.character(round(input$p + int(),  2)))
    })
  
}

shinyApp(ui, server)
