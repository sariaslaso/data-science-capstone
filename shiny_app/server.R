library(shiny)

source("helpers.R")

shinyServer(
    function(input, output) {
        
        predicted_words <- reactive({
            top_n_words <- top_3_words(input$text, model, input$num)
        })
        
        output$predicted_words <- renderUI(HTML(predicted_words()))
    }
)