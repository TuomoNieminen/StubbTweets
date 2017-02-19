# Retrieve data from twitter (Alexander Stubb tweets)
# auth: tuomo.a.nieminen@gmail.com
# 01.02.2016


# Get the data
# -----------

source("utility/functions.R")
access_libraries()

tweeter <- "alexstubb"
timezone <- 2
tweeter_languages <- c("english","finnish","swedish")

filepath <- paste0("data/",tweeter,"_tweets.Rda")

# authetication to twitter
setup_twitter_oauth(consumer_key = "",
                    consumer_secret = "",
                    access_token = "",
                    access_secret = "")

# call wrapper for twitteR library search functions
tw = super_search(search = "userTimeline", stopper=0,
                  args=list(user=tweeter, n = 20))

tw = twListToDF(tw) # to data.farme


# Data cleaning and variable engineering
# -------------------------------------

# remove duplicate tweets and change timezone
tw$id <- as.numeric(tw$id)
tw <- tw[!duplicated(tw$id),]
tw$created <- tw$created + timezone*60*60
tw$tunti <- as.numeric(format(tw$created,"%H"))

# define popularity as a sum of favorites and retweets
tw$suosio <- tw$favoriteCount + tw$retweetCount

# clean the tweets and add language variable
tw$tweet <- clean_text(tw$text)
tw <- tw[tw$tweet !="",]
tw$lang <- get_languages(tw$tweet, tweeter_languages)
tw <- tw[!is.na(tw$lang),]

# some basic stats
mindate <- format(min(tw$created),"%d.%m.%Y")
maxdate <- format(max(tw$created),"%d.%m.%Y")
ntweets <- nrow(tw)
ntweets
mindate
maxdate

# save to file
save(file=filepath,tw)
