



# Wellcome

description of the project here

## Explorative analysis

Some basic exploration of the data via pics. Timelines of tweets.

### Scope



mindate     | maxdate     | tweets
----------- | ----------- | -------
14.02.2015 | 19.02.2016 | 1740


### Monthly tweets


```r
par(mar=c(8,9,6,4))
h <- hist(tw$created, main = paste(tweeter, "tweets by month"),
     breaks="month", freq=T, 
     xlab="",ylab="", las=2,
     labels=T, cex.axis=1.3, cex.lab=1.5,
     ylim= c(0,400), format="%Y-%m",
     col="cadetblue3")
mtext(side = 2, text = "tweets", line = 5, cex = 1.5)
```

<img src="figure/monthly_tweets-1.png" title="plot of chunk monthly_tweets" alt="plot of chunk monthly_tweets" width="600px" />

### Daily tweets


```r
par(mar=c(7,9,5,3))
h <- hist(tw$created, main = paste(tweeter, "tweets by day"),
          breaks="days", freq=T, 
          xlab="", ylab="",col="grey55", lty=0,
          cex.axis=1.3 ,cex.lab=1.5, tck=0.05, xaxt="n")
peaks=2
tickpos <- h$breaks[order(h$counts,decreasing=T)[1:peaks]]
labels <- names(sort(table(format(tw$created,"%d.%m.%Y")),decreasing=T))[1:peaks]
axis(1, at=tickpos, labels=labels,cex.axis=1.5)
mtext(side = 2, text = "tweets", line = 5, cex = 1.5)
mtext(side = 1, text = "day", line = 3, cex = 1.5)
```

<img src="figure/daily_tweets-1.png" title="plot of chunk daily_tweets" alt="plot of chunk daily_tweets" width="600px" />

### Hourly tweets


```r
hourly <- table(format(tw$created,"%H"))
hourly_prc <- paste(round(100*hourly/sum(hourly),1),"%")
par(mar=c(8,10,5,3))
bp <- barplot(hourly, main = paste(tweeter, "tweets by hour"),
              space=0.5, ylim=c(0,250),
              ylab="",cex.lab=1, cex.names = 1.2,
              cex.axis=1.2, las=2, col = "deepskyblue3",
              xlab="")
text(bp+0.1, hourly, hourly_prc, pos=3, cex=0.7) 
mtext(side = 2, text = "tweets", line = 5, cex = 1.3)
mtext(side = 1, text = "time", line = 5, cex = 1.3)
```

<img src="figure/tweets_hour-1.png" title="plot of chunk tweets_hour" alt="plot of chunk tweets_hour" width="700px" />

### Most active tweet days


```r
top5tweetdays <- sort(table(format(tw$created,"%Y-%m-%d")),decreasing=T)[1:5]
top5tweetdays
```

```
## 
## 2015-12-16 2015-04-18 2015-04-01 2015-04-16 2015-03-10 
##         43         36         22         22         21
```


### Tweet times during the most active days


```r
top5days <- names(top5tweetdays)

topdaydata <- lapply(1:4,function(day) {
  get_datedata(tw, top5days[day])
})

newpar <- par(mfrow=c(2,2))
for(i in 1:4){
  time <- topdaydata[[i]]$created
  hist(time, breaks=100,freq=T,border=NULL,ylab="Tweets",
       tck=0,cex.axis=0.8, cex.lab=0.8, xlab="",
       main=paste0(tweeter," ",top5days[i]),
       cex.main=1, ylim=c(0,8))
}
```

<img src="figure/top_days-1.png" title="plot of chunk top_days" alt="plot of chunk top_days" width="600px" />

