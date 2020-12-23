#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#' ngram matching function
(WD <- getwd())
if (!is.null(WD)) setwd(WD)

#source(WD)
source("my_ngram.R")

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Capstone DataScience: Text Prediction Model"),
  
  p("This shiny app that takes  input phrase in a text box and outputs a prediction of the next word (german)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h2(" App instructions:"), 
      h4("1. Enter word or words in the text box"),
      h5("2. Predicted next word is highlighted in red"),
      h5("3. Hint: question if no prediction"),
      br()
    ),
    
    # Main input panel
    mainPanel(
      tabsetPanel(
        tabPanel("Predict next words...",
                 textInput("user_input", h3("Your text (german):"), 
                           value = "Hallo Welt"),
                 h3("Predicted Next Word:"),
                 h4(em(span(textOutput("ngram_output"), style="color:red"))))
        
     
      )   
    )
  )
)

#' Define server logic 
server <- function(input, output) {
  
  output$ngram_output <- renderText({
    ngrams(input$user_input)
  })
  
}

#' Run the application 
shinyApp(ui = ui, server = server)
