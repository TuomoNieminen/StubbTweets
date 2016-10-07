
# author: tuomo.a.nieminen@gmail.com


# Libraries
# ---------

if(!require(RCurl)) {install.packages("RCurl"); library(RCurl)}
if(!require(httr)) {install.packages("httr"); library(httr)}
set_config( config( ssl_verifypeer = 0L ) )

if(!require(devtools)) {install.packages("devtools"); library(devtools)}

install <- FALSE # set this to true if you need to install the packages below

doInstall <- c("ROAuth", "igraph", "ggplot2", "wordcloud", "devtools", "tm",
               "R2WinBUGS", "rmongodb", "scales")
if(install) {
  install.packages(doInstall, repos = "http://cran.r-project.org")
  install_github("streamR", "pablobarbera", subdir="streamR")
  install_github("smappR", "SMAPPNYU")
}
library(streamR)
library(smappR)

if(!require(twitteR)){ install.packages("twitteR"); library(twitteR)}
if(!require(slam)) { install.packages("slam"); library("slam")}
if(!require(tm)){install.packages("tm");library(tm)}
if(!require(wordcloud)){install.packages("wordcloud"); library(wordcloud)}
if(!require(RColorBrewer)){install.packages("RColorBrewer"); library(RColorBrewer)}
if(!require(stringr)) {install.packages("stringr"); library(stringr)}
if(!require(topicmodels)) {install.packages("topicmodels"); library(topicmodels)}
if(!require(textcat)) {install.packages("textcat"); library(textcat)}
if(!require(data.table)) {install.packages("data.table"); library(data.table)}
if(!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
if(!require(plyr)) {install.packages("plyr"); library(plyr)}
library(grid)


# Getting the raw data
# ------------------------


# wrapper for a twitteR search function
# keeps receiving older tweets until no more can be received
# example: 
# tweet_person <- "alexstubb"
# tw = super_search(search = "userTimeline", 
#                   args=list(user=tweet_person, n = 20))
# tw = twListToDF(tw)

super_search <- function(search = "userTimeline", 
                         args, stopper=1) {
  
  # call the search function
  tweets <- do.call(search, args)
  
  # get tweet ids and set the maxid for the next search to recceive earlier tweets
  ids <- sapply(tweets, function(tweet) tweet$getId())
  args[["maxID"]] <- min(as.numeric(ids)) -1
  n_new <- length(ids)  
  
  # loop until can only receive a small amount tweets (controlled by stopper)
  while(n_new > stopper) {
    new_tweets <- do.call(search, args)
    ids <- sapply(new_tweets, function(tweet) tweet$getId())
    args[["maxID"]] <- min(as.numeric(ids)) -1
    tweets <- c(tweets, new_tweets)
    
    n_new <- length(ids)
    Sys.sleep(5)
  }  
  return(tweets)
}


# get tweetdata by date

get_datedata <- function(tweet_df, date="2015-12-16") {
  format <- ifelse(nchar(date)>8,"%Y-%m-%d","%Y-%m")
  interest_date <- as.Date(date,format=format)
  datematch <- as.Date(tweet_df$created, format=format) == interest_date
  tweet_df[datematch,]
}


# sorts text by language (textcat package)

get_languages<-function(text_v, init_langs) {
  profile <- TC_byte_profiles[names(TC_byte_profiles) %in% init_langs]
  textcat(text_v, p = profile)
}

# returns a term document matrix from a vector of texts (tweets)
# used by twitter_wordfreq()

get_documentTermMatrix <- function(search_text, searchwords=NULL,language="fi") {
  
  cleantext <- clean_text(search_text)
  # create a corpus
  result_corpus = Corpus(VectorSource(cleantext),
                         readerControl = list(language = language))
  
  # get stopwords for filtering out filler words
  if(language=="fi") {
    finwords <- get(load("utility/fi_stopwords.Rda"))
  } else {
    finwords <- NULL
  }
  my_stopwords <- unique(c(finwords, stopwords(language), stopwords("en")))
  
  # create a term document matrix applying some transformations
  dtm <- DocumentTermMatrix(result_corpus,
                            control = list(removePunctuation = TRUE,
                                           stopwords = c(searchwords, my_stopwords),
                                           removeNumbers = FALSE, tolower = TRUE,
                                           wordLengths=c(2, Inf)))
  
  # remove rows with no words and return
  rowTotals <- apply(dtm, 1, sum)
  dtm   <- dtm[rowTotals> 0, ]  
  return(dtm)
}

# removes unwanted characters from text

clean_text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("https\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("\n","",some_txt)
  
  some_txt = gsub("[^[:alnum:] ]", "",some_txt)
  
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  names(some_txt) = NULL
  return(some_txt)
}


# Summaries
# ------------------

# returns a sorted data frame with word frequencies
# example:
# tweets <- searchTwitter("talous", since="2016-01-01")
# tw_df <- twListToDF(tweets)
# dtm <- get_documentTermMatrix(tw_df$text)
# freqs <- twitter_wordfreq(dtm)
# head(freqs)

twitter_wordfreqs <- function(documentTermMatrix) {
  
  # define dtm as matrix
  m = as.matrix(documentTermMatrix)
  # get word counts in decreasing order
  word_freqs = sort(colSums(m), decreasing=TRUE)
  
  # create a data frame with words and their frequencies
  dm = data.frame(word=names(word_freqs), freq=word_freqs)
  dm$word = as.character(dm$word)
  
  return(dm) 
}


# a function that takes a vector of tweets (text) and 
# returns the frequency of hastags

# example: 
# tw = userTimeline("BarackObama", cainfo = x1, n = 3200)
# tw = twListToDF(tw)
# head(extract.hashes(tw$text),50)

extract_hashes = function(tweet_text){
  
  hash.pattern = "#[[:alnum:]]+"
  have.hash = grep(x = tweet_text, pattern = hash.pattern)
  
  hash.matches = gregexpr(pattern = hash.pattern,
                          text = tweet_text[have.hash])
  extracted.hash = regmatches(x = tweet_text[have.hash], m = hash.matches)
  
  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("tag","freq")
  df = df[order(df$freq,decreasing = TRUE),]
  rownames(df) <- NULL
  return(df)
}


# popularity of tweets by a defined time period (month or week)

get_popularity_bydate <- function(data,period="monthly") {
  form <- switch(period,
                 "monthly" = "%Y %m",
                 "weekly" = "%Y %m %W")
  data <- data.table(data)
  data$time <- format(data$created,form)
  
  pop <- data[, list(tweets = length(suosio), 
                     likes=sum(suosio)),
              by= time]
  
  pop$suosio <- pop$likes / pop$tweets
  pop$by <- period
  
  #order by date
  dates <- as.numeric(as.character(gsub(" ","",pop$time)))
  pop <- pop[order(dates),]
  
  return(pop)
  
}

# popularity summary

get_summary <- function(data,by="kuukausi") {
  if(by=="kuukausi") {
    data$aika <- format(data$created,"%Y %m")
    
    data$keski_suosio <- sapply(data$aika, function(m) {
      sub <- data[data$aika==m,]
      mean(sub$suosio)
    })
    
  } else {
    data$aika <- data$tunti
    data$keski_suosio <- sapply(data$aika, function(h) {
      sub <- data[data$aika==h,]
      mean(sub$suosio)
    })
  }
  
  ddply(
    data,
    .(aika, keski_suosio),
    summarize,
    tweets=length(aika)
  )
}
