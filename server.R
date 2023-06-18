## server.R

library(shiny)
library(stringi)
library(wordcloud)
library(ggplot2)
library(tm)
library(data.table)


#if(!exists("foo", mode="function")) source("./wordPrediction.R")

#source("./wordPrediction.R", local=TRUE)
load("corpus_data.Rdata", envir = .GlobalEnv)

pred_words <- function(sentence, n = 10, num_end_words = 5){
  # predict function
  # simple back-off algorithm working its way from large 6-grams to tri/bi/unigrams
  # follow a similar preparation path as the large corpus
  sentence <- removeNumbers(sentence)
  sentence <- removePunctuation(sentence)
  sentence <- tolower(sentence)
  
  # split sentences into words
  words <- unlist(strsplit(sentence, split = " " ))
  
  words <- tail(words, num_end_words)
  
  word1 <- words[1];word2 <- words[2];word3 <- words[3];word4 <- words[4];word5 <- words[5];
  datasub <- data.table()
  
  if (nrow(datasub)==0 & !is.na(word5)) {
    if(nrow(datasub) == 0) datasub <- subset(ngram6, w1==word1 & w2==word2 & w3==word3 & w4==word4 & w5==word5)
    if(nrow(datasub) == 0) datasub <- subset(ngram5, w1==word2 & w2==word3 & w3==word4 & w4==word5)
    if(nrow(datasub) == 0) datasub <- subset(ngram4, w1==word3 & w2==word4 & w3==word5)
    if(nrow(datasub) == 0) datasub <- subset(ngram3, w1==word4 & w2==word5)
    if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word5)
  }
  
  if (nrow(datasub)==0 & !is.na(word4)) {
    if(nrow(datasub) == 0) datasub <- subset(ngram5, w1==word1 & w2==word2 & w3==word3 & w4==word4)
    if(nrow(datasub) == 0) datasub <- subset(ngram4, w1==word2 & w2==word3 & w3==word4)
    if(nrow(datasub) == 0) datasub <- subset(ngram3, w1==word3 & w2==word4)
    if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word4)
  }
  
  if (nrow(datasub)==0 & !is.na(word3)) {
    if(nrow(datasub) == 0) datasub <- subset(ngram4, w1==word1 & w2==word2 & w3==word3)
    if(nrow(datasub) == 0) datasub <- subset(ngram3, w1==word2 & w2==word3)
    if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word3)
  }
  
  if (nrow(datasub)==0 & !is.na(word2)) {
    if(nrow(datasub) == 0) datasub <- subset(ngram3, w1==word1 & w2==word2)
    if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word2)
  }
  
  if (nrow(datasub)==0 & !is.na(word1)) {
    if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word1)
    if(nrow(datasub) == 0) datasub <- head(ngram1)
  }
  
  if(nrow(datasub) > 0){
    datasub$freq <- datasub$count / sum(datasub$count)
    as.data.frame(head(datasub[order(-freq)], min(n, nrow(datasub))))
  }
  
}

shinyServer(function(input, output) {

  output$ngramPlot <- renderPlot({
    ggplot(head(ngram_stats,15), aes(reorder(most_frequent_ngram,count_max), count_max)) +   
        geom_bar(stat="identity") + coord_flip() + 
        xlab("n-Grams") + ylab("Frequency") +
        ggtitle("Most frequent N-Grams")
  })

  output$plot1 <- renderPlot({
    validate(
      need(input$sentence, "Waiting on Word Input")
    )
    prdf <- pred_words(input$sentence, 10, 5)
    prdf$merged <- apply(prdf[2:(ncol(prdf) - 1)], 1, paste, collapse = " ")

    ggplot(head(prdf,15), aes(reorder(merged,freq), freq)) +   
        geom_bar(stat="identity") + coord_flip() + 
        xlab("n-Grams") + ylab("Frequency") +
        ggtitle("Most frequent N-Grams")
        })

  output$pred_results <- renderPrint({
    #pred_words(input$sentence, input$freq_dist, input$countend_words)
    pred_words(input$sentence, 10, 5)
  })

  ##TODO: Need to remove the stats
  #output$stats <- renderPrint({
  # ngram_stats
  #})

  output$cloudImage <- renderImage({
    validate(
      need(input$sentence,"Waiting on Word Input")
    )
      outfile <- tempfile(fileext = '.png')
      png(outfile, pointsize = 8, res = 100)
      prdf <- pred_words(input$sentence, 10, 5)
      prdf$merged <- apply(prdf[2:(ncol(prdf) - 1)], 1, paste, collapse = " ")
      wordcloud(prdf$merged, prdf$freq,max.words=100,random.order = F,col=brewer.pal(8, "Dark2") , rot.per=0.3)
      #wordcloud(ngram_stats$most_frequent_ngram,ngram_stats$count_max,max.words=100,random.order = F,col=brewer.pal(8, "Dark2") , rot.per=0.3)
      dev.off()
      list(src = outfile,
          contentType = 'image/png',
          width = 500,
          height = 500)
          }
          , deleteFile = TRUE
         )
}
)
