# init
rm(list = ls())
gc()

# libs
if (!require("NLP"))   { install.packages("NLP") }
if (!require("stringi"))      { install.packages("stringi") }
if (!require("wordcloud"))    { install.packages("wordcloud") }
if (!require("ggplot2"))      { install.packages("ggplot2") }
if (!require("tm"))           { install.packages("tm") }
if (!require("RColorBrewer")) { install.packages("RColorBrewer") }
if (!require("dplyr"))        { install.packages("dplyr") }
if (!require("slam"))         { install.packages("slam") }
if (!require("data.table"))   { install.packages("data.table") }




#TODO: Need to fix the temp file existence to work when it is not a tmp

zipdownloader <- function(source_url,zip_file, list = NULL){
    
    tempFile <- tempfile()

    if (!file.exists(zip_file)) {
      print("The file doesnt exist...")
      download.file(source_url, tempFile)
    } else if (file.exists(zip_file)) {
      print("the file DOES exist...")
      tempFile <- zip_file
    }
    if (is.null(list)==TRUE){
      unzip(tempFile, list=T)
      } else if (is.null(list)==FALSE) {
          zx <- unzip(tempFile,list=T)
          return(zx)
      }
    unlink(tempFile)
    }


ziphandler <- function(x){
  
  zip_files <- x
  zip_files$Date <- NULL
  zip_files$Language <- substr(zip_files$Name, 7, 8)
  zip_files$Length_in_Mb <- zip_files$Length/(1024^2)
  zip_files <- zip_files[zip_files$Length>0,]
  ggplot(zip_files, aes(x = Name, y = Length_in_Mb, fill = Language)) + 
    geom_bar(stat="identity") + 
    coord_flip() +
    theme_light() + 
    xlab("") + ylab("File size in Mb")
  # only load US files
  zip_files <- zip_files[ grep("en_US", zip_files$Name),  ]
  return(zip_files)
}


filedatareader <- function(file_df){
  
  for (file in file_df$Name) {
    con <- file(file, "r")
    file_content <- readLines(con, encoding = "UTF-8")
    print(  paste("File:", file, "has in-memory size of:") )
    print(object.size(file_content), units="Mb")
    close(con)
    
    # dynamically create variable names
    assign(basename(file), file_content, envir = .GlobalEnv)
    file_content <- NULL
  }
}

cleanObjects <- function(objlist) {
  for (i in objlist) {
    objs <- ls(pos = ".GlobalEnv")
    rm(list = objs[grep(i, objs)], pos = ".GlobalEnv")
  }
}


gen_stats <- function(txtlist, multiplier) {
  
  ds <<- c()
  for (i in txtlist){
    var = eval(parse(text = i))
    set.seed(1984); assign(paste0("ds.",i), sample(var,   0.2 * length(var)), envir = .GlobalEnv)
    ds <<- c(ds,assign(paste0("ds.",i), sample(var,   0.2 * length(var)), envir = .GlobalEnv))
  }
  
  hist(stri_count_words(ds), breaks=30, col=rainbow(50), main = paste("Number of words distribution for", prettyNum(length(ds), scientific=FALSE, big.mark=","), "documents" ))
  
  length(ds)
  
  # word summaries
  summary(stri_count_words(ds))
  
  # summary number of characters
  summary(sapply(ds, nchar) )
  
  # calculate how much memory each object requires, and list the largest 10
  tail(sort(sapply(ls(), function(x) object.size(get(x)))), 10)
  print(paste0("list of obj",ls()))
  
  cleanObjects(txtlist)

}

#Volatile Corpus generator
genCorpus <- function(txtlist) {
  # text mining on sampled data
  cp <- VCorpus(VectorSource(txtlist))

  # start the tm_map transformations 

  # switch encoding: convert character vector from UTF-8 to ASCII
  cp <- tm_map(cp, function(x)  iconv(x, 'UTF-8', 'ASCII'))

  # eliminate white spaces
  cp <- tm_map(cp, stripWhitespace)

  # convert to lowercase
  cp <- tm_map(cp, tolower)

  # Remove punctuation
  cp = tm_map(cp, removePunctuation)

  # Remove numbers
  cp = tm_map(cp, removeNumbers)

  # assign TEXT flag
  cp <- tm_map(cp, PlainTextDocument)
  
  return(cp)
}

ngramGenerator <- function(corpus_object) {
  ## Modified
  corp <- corpus_object
  for(i in 1:6) {
    print(paste0("Extracting", " ", i, "-grams from corpus"))
    tokens <- function(x) unlist(lapply(ngrams(words(x), i), paste, collapse = " "), use.names = FALSE)
    tdm <- TermDocumentMatrix(corp, control = list(tokenize = tokens))
    tdmr <- sort(slam::row_sums(tdm, na.rm = T), decreasing=TRUE)
    tdmr.t <- data.table(token = names(tdmr), count = unname(tdmr)) 
    tdmr.t[,  paste0("w", seq(i)) := tstrsplit(token, " ", fixed=TRUE)]
    # remove source token to save memory
    tdmr.t$token <- NULL
    
    
    print(paste0("Loaded in memory ", nrow(tdmr.t), " ", i, "-grams, taking: "))
    print(object.size(tdmr.t), units='Mb')
    
    #frequency distribution
    print(table(tdmr.t$count))
    
    # dynamically create variable names
    assign(paste0("ngram",i), tdmr.t, envir = .GlobalEnv)
  }
}



stat_generator <- function(startrange, endrange, stats_df){

  for (i in startrange:endrange) {
    s <- summary(eval(parse(text = paste0('ngram',i,'$count'))) )
    w <- eval(parse(text = paste0('ngram',i,'[1,seq(',i,')+1, with=F]')))
    most_frequent_ngram <- paste(unlist(w), sep=" ", collapse = " ")
    m <- paste(round(object.size(eval(parse(text = paste0('ngram',i))))/1024^2,1),'Mb')
    xdf <-  data.frame(ngram = paste0('ngram',i),
                      length = nrow(eval(parse(text = paste0('ngram',i)))),
                      count_min = s[[1]],
                      count_median = s[[3]],
                      count_mean = round(s[[4]],1),
                      count_max = s[[6]],
                      most_frequent_ngram = most_frequent_ngram,
                      mem = m)
    stats_df <- rbind(stats_df, xdf)
    }
  return(stats_df)
}


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


run_tasks <- function() {
  if (!file.exists("corpus_data.Rdata")) {
    zx <<- zipdownloader("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","Coursera-SwiftKey.zip", list = F)
    zip_test <<- ziphandler(zx)
    filedatareader(zip_test)
    filelist <<- c("en_US.blogs.txt", "en_US.twitter.txt", "en_US.news.txt")
    gen_stats(filelist, 0.2)
    corp <<- genCorpus(ds)
    ngramGenerator(corpus_object = corp)
    # memory size of our initial corpus
    #print(object.size(corp), units="Mb")

    # environment size (all n-grams including corpus)
    #print(object.size(x=sapply(ls(), get)), units="Mb")
    
    ngram_stats <<- data.frame(ngram = '', length = 0, count_min = 0, count_median = 0, count_mean = 0, count_max = 0, most_frequent_ngram='', mem = '')
    ngram_stats <<- stat_generator(1,6, ngram_stats)
    ngram_stats <<- ngram_stats[-1,]
    # save data for the Shiny app
    save.image("corpus_data.Rdata")
  } else if (file.exists("corpus_data.Rdata")) {
     load("corpus_data.Rdata", envir = .GlobalEnv)
  }
  print("Prediction Objects Loaded.")
}


run_tasks()