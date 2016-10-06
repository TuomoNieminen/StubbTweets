#load data
tweeter <- "alexstubb"
filepath <- paste0("data/",tweeter,"_tweets.Rda")
tw <- get(load(filepath))

# get n most popular tweets
pop <- function(d,n=2) {
  d[order(d$popularity,decreasing=T),][1:n,c(1,5,17)]
  }

# most popular tweet at 7 am 
tmp <- tw[tw$tunti==7,]
pop(tmp)

# most popular kokoomus (stubb's political party) tweet
tmp <- tw[grepl("#kokoomus", tw$text),]
pop(tmp)

# most popular from a given day
day = "16.12.2015"
tmp <- tw[format(tw$created,"%d.%m.%Y")==day,]
pop(tmp)


# most popular from a topic
t = 4
tw_fi <- get(load(paste0("data/",tweeter,"_topicdata.Rda")))[["finnish"]]
tmp <- tw_fi[tw_fi$topic==t,]
pop(tmp,10)
