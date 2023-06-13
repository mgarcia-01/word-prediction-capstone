library(shiny)

if(!exists("foo", mode="function")) source("./wordPrediction.R")


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
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
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$ngramPlot <- renderPlot({
    ggplot(head(ngram_stats,15), aes(reorder(most_frequent_ngram,count_max), count_max)) +   
        geom_bar(stat="identity") + coord_flip() + 
        xlab("n-Grams") + ylab("Frequency") +
        ggtitle("Most frequent N-Grams")
  })

  output$pred_results <- renderPrint({
    #pred_words(input$sentence, input$freq_dist, input$countend_words)
    pred_words(input$sentence, 10, 5)
  })
  output$stats <- renderPrint({
    ngram_stats
  })
  output$cloudImage <- renderImage({
    outfile <- tempfile(fileext = '.png')
    png(outfile, pointsize = 10, res = 200)
    wordcloud(ngram_stats$most_frequent_ngram,ngram_stats$count_max,max.words=100,random.order = F,col=brewer.pal(8, "Dark2") , rot.per=0.3)
    dev.off()
    list(src = outfile,
        contentType = 'image/png',
        width = 700,
        height = 700
    )
  }, deleteFile = TRUE
  )
}

app <- shinyApp(ui = ui, server = server)

runApp(app)
