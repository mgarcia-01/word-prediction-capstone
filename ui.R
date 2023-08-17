## ui.R

library(shiny)


shinyUI(fluidPage(
    tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #ffffff;
            border-color: white;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }'))
        ),
  # App title ----
  titlePanel("Word Prediction with N-Grams"),
  h4(" "),
  h4('Please wait for 5 seconds for app to load '),
  
  textAreaInput("sentence", " Input the string to predict"),
  #numericInput("freq_dist", "Number of bins", 10),
  #numericInput("countend_words", "Enter number of end words", 5),
  verbatimTextOutput("pred_results"),

  #h4('Entire Data Stats '),
  #verbatimTextOutput("stats"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "right",
    # Sidebar panel for inputs ----
    sidebarPanel(
      id="sidebar"
    ),
    
    # Main panel for displaying outputs ----
    mainPanel("Graphs",
    fluidRow(splitLayout(cellWidths = c("50%","50%"),imageOutput("cloudImage"),plotOutput(outputId = "plot1")))
     # imageOutput("cloudImage")    
    )
  )
)
)
