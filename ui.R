## ui.R

library(shiny)

shinyUI(fluidPage(
  
  # App title ----
  titlePanel("N-Grams Data Science Capstone!"),
  textAreaInput("sentence", " Input the string to predict"),
  #numericInput("freq_dist", "Number of bins", 10),
  #numericInput("countend_words", "Enter number of end words", 5),
  verbatimTextOutput("pred_results"),
  verbatimTextOutput("stats"),
  
  

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      plotOutput(outputId = "ngramPlot")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
    
      imageOutput("cloudImage")
      
    )
  )
))