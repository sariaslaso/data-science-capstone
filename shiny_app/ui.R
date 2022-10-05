library(shiny)

shinyUI(fluidPage(
    
    titlePanel("next word"),
    
    fluidRow(
            column(width = 5, offset = 1, 
                   textInput("text", h3("text input"), 
                             value = "Enter text..."),
                   numericInput("num", h4("k predictions"), 
                                value = 1, min = 1, max = 5)
                   )
        ),
    
    mainPanel(
        h2("word suggestion"),
        htmlOutput("predicted_words")
        )
    )
)
