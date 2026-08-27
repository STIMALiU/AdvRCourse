library(shiny)

ui <- fluidPage(
  
  numericInput(inputId = "n", 
               label = "How many times you tossin' this fair coin?", 
               value = 4, min = 1, max = 100, step = 1),
  plotOutput(outputId = "probs")
  
)

server <- function(input, output) {
  
  output$probs <- renderPlot({
    barplot(height = dbinom(0:input$n, input$n, 0.5),
            names = 0:input$n,  xlab = "Number of Heads", 
            ylab = "Probability",
            main = "Probabilities that x Heads were Observed")
  })
  
}

shinyApp(ui, server)
