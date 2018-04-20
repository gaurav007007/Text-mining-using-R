# Text-mining-using-R


## **** Steps for authorization to connect and extract***

setwd("E:/gaurav_back up/Spring 2018/Data_Mining_Professor_M_Yousuf/Project/SentimentAnalysis for twitter")
#https://cran.r-project.org/web/packages/available_packages_by_date.html
# install.packages("twitteR")
# install.packages("syuzhet")
# install.packages("tm")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("httr")
# install.packages("wordcloud")
# install.packages("sentiment")
# install.packages("RCurl")
# install.packages("wordcloud")


library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentiment)
library(RCurl)
library(syuzhet)
library(tm)

# #handshake with the twitter.
# oauth_endpoint(authorize="https://api.twitter.com/oauth",
#                access="https://api.twitter.com/oauth/access_token")
# #connect to API
# download.file(url='http://curl.haxx.se/ca/cacert.pem',destfile='cacert.pem')
# reqURL <- 'https://api.twitter.com/oauth/request_token'
# accessURL <-'https://api.twitter.com/oauth/access_token'
# authURL <- 'https://api.twitter.com/oauth/authorize'

### Twitter Application
consumerKey <-'6ZRHBYyhBmCFaGw4KbwxNrmLR'
consumerSecret <-'kwmV62c3a2pCLB3aG3EmXH3DZnB9n0SxsUat58QDCaL0DUEMEN'
accesstoken <-'147268830-yPBIvYslwYQ38BpVK7g92QkiMIgTK4kYSMRLTlwh'
accesssecret <-'xXkKqxgwigpaKD5LxmIFG1IwRRCTchOcvASK2Hg73h9tR'

#Cred <- OAuthFactory$new(consumerKey=consumerKey,
#                         consumerSecret=consumerSecret,
#                         requestURL=reqURL,
#                         accessURL=accessURL,
#                         authURL=authURL)
#Cred$handshake(cainfo =system.file('CurlSSL','cacert.pem',package='RCurl'))

#### Authorization PIN- DYNAMIC
#save(Cred,file='twitter authentication.Rdata')
#load('twitter authentication.Rdata')

# once you launch the code first time, start from this line in future.
setup_twitter_oauth(consumerKey,consumerSecret,accesstoken,accesssecret)

#test the connection
some_tweets= searchTwitter("pakistan",n=4000,since="2017-09-01",lang="en")

#retweets(952404098992803000,100)
# Explore Tweets
length.some_tweets <- length(some_tweets)


 # convert the text format into dataframe using function  ldply of library plyr.
 some_tweets.df <- ldply(some_tweets,function(x) x$toDataFrame())
#class(some_tweets.df)
 # save the tweets to csv
 write.csv(some_tweets.df,"tweets.csv")
 
 # get the text using Sapply function 
 some_txt = sapply(some_tweets,function(x) x$getText())

# Cleaning 1 remove RT using regular expressions.
 # gsub  is used for data cleaning. it will find the pattern and substitute the pattern we want.
 some_txt1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",some_txt)
# Cleaning 2- remove html links
some_txt2 =gsub("http[^[:blank:]]+","",some_txt1) 
# Cleaning 3- remove people name
some_txt3 = gsub("@\\w+","",some_txt2)
# Cleaning 4 - remove Punctuations
some_txt4 = gsub("[[:punct:]]"," ",some_txt3)

 # Cleaning 5: remove Punctuations
some_txt5 = gsub( "[^[:alnum:]]"," ",some_txt4)
# exporting to excel
write.csv(some_txt5,"tweets1.csv")

# create a word corpus or dictionary(group of words). we will use corpus function of library tm.

some_txt6 <- Corpus(VectorSource(some_txt5))
# TM_map or tranformational mapping, the corpus to itself
some_txt6 <- tm_map(some_txt6,removePunctuation)
some_txt6 <- tm_map(some_txt6,content_transformer(tolower))
# stopwords like " a, am, i think, because.."
some_txt6 <- tm_map(some_txt6,removeWords,stopwords("english"))
some_txt6 <- tm_map(some_txt6,stripWhitespace)


# Building word cloud. Larger the word the more frequently it happens.
pal <- brewer.pal(8,"Dark2")
x11()
wordcloud(some_txt6, min_frq=5,max.words= 500,width=1000,height=1000,random.order=FALSE,color=pal)


# Sentiment Analysis
# function usage

 #syuzhet library has get_nrc_sentiment function defined,and if we pass any corpus/sentence to it. It will give us the sentiments.

get_nrc_sentiment(" iphone is waste device. it is bad")
# remember to pass some_txt5 and not some_txt6( it is a corpus, a bunch of words with no sense) but the sentiments are also defiend by the semanti
#cs as well.
mysentiment <- get_nrc_sentiment(some_txt5)

#fix(mysentiment)
SentimentScores <- data.frame(colSums(mysentiment[,]))
fix(SentimentScores)
names(SentimentScores) <- "Score"

SentimentScores <- cbind("sentiment" = rownames(SentimentScores),SentimentScores)

rownames(SentimentScores) <- NULL

ggplot(data=SentimentScores, aes(x=sentiment, y=Score)) +
  geom_bar(aes(fill=sentiment),stat="identity") +
  theme(legend.position="none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
  
  





