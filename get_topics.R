source("utility/functions.R")

# #load data

tweeter <- "alexstubb"
filepath <- paste0("data/",tweeter,"_tweets.Rda")
tw <- get(load(filepath))
tweeter_languages <- unique(tw$lang)

# prepare data for topic modelling

# separate by language
TW <- lapply(tweeter_languages, function(l) tw[tw$lang==l,])
names(TW) <- tweeter_languages

# get document term matrix by language
DTM <- lapply(TW, function(tw) get_documentTermMatrix(tw$tweet))

# save(file=paste0(tweeter,"_DTM.Rda"),DTM)

# get tfidf and remove most used words
TFIDF <- lapply(DTM, function(dtm) {
  tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
    log2(nDocs(dtm)/col_sums(dtm > 0))
})

#term frequencies before cuts
lapply(DTM,function(dtm) summary(col_sums(dtm)))

#use 0.1 quantile as tfidf cutoff
DTM2 <- list()
temp <- mapply(function(dtm,tfidf,lang) {
  cutoff <- quantile(tfidf, 0.1)
  dtm <- dtm[,tfidf >= cutoff]
  dtm <- dtm[,col_sums(dtm)>1]
  DTM2[[lang]] <<- dtm[row_sums(dtm) > 0,]
  return()
}, DTM, TFIDF, tweeter_languages)

#term frequencies after cuts
lapply(DTM2,function(dtm) summary(col_sums(dtm)))


# fit topic models using three methods (separately for each language)
k <- 4 # number of topics

TM <- lapply(DTM2, function(dtm) {
  list(
    VEM = LDA(dtm, k = k),
    Gibbs = LDA(dtm, k = k, method = "Gibbs",
                control = list(burnin = 1000,thin = 10, iter = 10000)),
    CTM = CTM(dtm, k = k)
  )
})

# for each language, use the model with lowest entropy
ENTR <- lapply(TM, function(tm) {
  sapply(tm, function(x) {
    mean(apply(posterior(x)$topics,
               1, function(z) - sum(z * log(z))))
  })})
model_choice <- sapply(ENTR, function(e) names(e[e==min(e)]))
model_choice

# keep only the models with lowest entropy
MODELS <- mapply(TM, model_choice, FUN = function(tm, choice) tm[[choice]])
names(MODELS) <- tweeter_languages

# topic distributions
TOPICS <- lapply(MODELS, topics)
lapply(TOPICS, table)

# label the tweets by topic and add the frequency of the most likely topic
for(i in 1:length(TW)) {
  tpcs <- TOPICS[[i]]
  havelabels <- as.numeric(names(tpcs))
  TW[[i]]$topic <- NA
  TW[[i]][havelabels,]$topic <- tpcs
  posteriors <- posterior(MODELS[[i]])$topics
  TW[[i]]["P_max"] <- NA
  TW[[i]][havelabels,]["P_max"] <- apply(posteriors,1,max)
  
  for(j in 1:k) {
    colname <- paste0("P_topic",j)
    TW[[i]][colname] <- NA
    TW[[i]][havelabels,][colname] <- posteriors[,j]
  }
}

# now all the tweets have a topic distribution and a most likely topic attached to them
head(TW[["finnish"]])


# Some exploration
# ----------------

# get the most frequent terms by topic
for(i in 1:length(MODELS)) {
  trms <- terms(MODELS[[i]],40)
  write.csv(file=paste0("data/",tweeter,"_top_terms_by_topics_",
                        names(MODELS[i]),".csv"),
            row.names=F,
            trms)
}

# get 10 'most representative' tweets from each topic
for(lang in tweeter_languages) {
  tw <- TW[[lang]]
  tw <- tw[!is.na(tw$topic),]
  path <- paste0("data/",tweeter,"_topicsample_",lang,".csv")
  write(file=path,paste("Top 10",lang,"tweets by topic \n"))
  write(file=path,"Topic distributions: \n",append=T)
  suppressWarnings(
   write.table(file=path,table(TOPICS[[lang]]),
              row.names=F,col.names=c("Topic","Freq"),append=T))
  for(t in sort(unique(tw$topic))) {
    tw_sub <- tw[tw$topic==t,]
    tw_sub <- tw_sub[with(tw_sub,order(P_max,decreasing=T)),]
    s <- tw_sub$text[1:min(10,nrow(tw_sub))]
    write(file=path,paste("\n ### TOPIC",t,"### \n"), append=T)
    j <- 1
    for(tweet in s) {
      max_post <- round(tw_sub$P_max[j],2)
      write(file=path,paste0("P(T",t,")=",max_post," ",tweet," \n "),append=T)
      j <- j + 1
    }
  }
}

# save all data frames as a list
save(file=paste0("data/",tweeter,"_topicdata.Rda"),TW)
