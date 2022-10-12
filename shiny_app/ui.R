library(shiny)

shinyUI(fluidPage(
    
    titlePanel(h1("Next Word")),
    
    fluidRow(
            column(width = 6, offset = 4, 
                   textInput("text", h3("text input"), 
                             value = "Enter text..."),
                   sliderInput("num", h5("number of predictions"), 
                                value = 1, min = 1, max = 10, ticks = FALSE),
                   h6("word suggestion:"),
                   htmlOutput("predicted_words")
                   )
        )
    )
)
