##### Twitter sentiment analysis - Twitter.R - https://apps.twitter.com/app/14417297


### Necessary libraries

library(twitteR) ### for fetching the tweets
library(plyr) ## for breaking the data into manageable pieces
library(ROAuth) ## for R authentification
library(httr)
library(rjson)
library(stringr) ## for string processing
library(ggplot2) ## for plotting the results

##### Set API Keys (entering keys from the Twitter app)
api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#### Read in dictionary of positive and negative words
  ## Download the positive-words.txt and negative-words.txt from GitHub
posText <- read.delim("/Users/pbarbizet/Desktop/Polytechnique/Cours_R/Homework_R/CryptoR/positive-words.txt",
                      header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim("/Users/pbarbizet/Desktop/Polytechnique/Cours_R/Homework_R/CryptoR/negative-words.txt",
                      header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))

pos.words = c(posText, 'upgrade')
neg.words = c(negText, 'wtf', 'wait', 'waiting','epicfail', 'mechanical')


#### Grab data about bitcoin - the following lines of code extract the n latest mentions of Bitcoin
tweets_bitcoin <- searchTwitter('#bitcoin', n=5000)
tweets_ether <- searchTwitter('#ether', n=5000)
tweets_litecoin <- searchTwitter('#litecoin', n=5000)

#### Loop over tweets and extract text - it extracts all the text from the tweets
bitcoin_txt = sapply(tweets_bitcoin, function(t) t$getText() )
ether_txt = sapply(tweets_ether, function(t) t$getText() )
litecoin_txt = sapply(tweets_litecoin, function(t) t$getText() )

#### Number of tweets for each element
noof_tweets = c(length(bitcoin_txt), length(ether_txt),length(litecoin_txt))

#### Combine the text of all the cryptocurrencies
kcurrencies <- c(bitcoin_txt,ether_txt,litecoin_txt)


#### Function which puts a score to the sentence depending on the bad and good words it has
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of positive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  # create a simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

#### Process the tweets to compute the sentiment score
scores = score.sentiment(kcurrencies, pos.words,neg.words , .progress='text')

#### Create a variable in the dataframe
scores$kcurrencies = factor(rep(c("Bitcoin", "Ether","Litecoin"), noof_tweets))

#### Compute the number of positive, negative and neutral statements
scores$positive <- as.numeric(scores$score >0)
scores$negative <- as.numeric(scores$score >0)
scores$neutral <- as.numeric(scores$score==0)

#### Split the data frame into individual datasets for each cryptocurrency
bitcoin_currency <- subset(scores, scores$kcurrencies=="Bitcoin")
ether_currency <- subset(scores,scores$kcurrencies=="Ether")
litecoin_currency <- subset(scores,scores$kcurrencies=="Litecoin")

#### Create polarity variable for each cryptocurrency
bitcoin_currency$polarity <- ifelse(bitcoin_currency$score >0,"positive",
    ifelse(bitcoin_currency$score < 0,"negative",ifelse(bitcoin_currency$score==0,"Neutral",0)))
ether_currency$polarity <- ifelse(ether_currency$score >0,"positive",
    ifelse(ether_currency$score < 0,"negative",ifelse(ether_currency$score==0,"Neutral",0)))
litecoin_currency$polarity <- ifelse(litecoin_currency$score >0,"positive",
    ifelse(litecoin_currency$score < 0,"negative",ifelse(litecoin_currency$score==0,"Neutral",0)))


#### Polarity plot - customer sentiments Bitcoin
qplot(factor(polarity), data=bitcoin_currency, geom="bar", 
  fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Sentiments - Bitcoin Currency")

#### Sentiment score - Bitcoin
qplot(factor(score), data=bitcoin_currency, geom="bar",
  fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Sentiment Scores - Bitcoin Currency")

#### Polarity plot - customer sentiments Ether
qplot(factor(polarity), data=ether_currency, geom="bar", 
      fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Sentiments - Ether Currency")

#### Sentiment score - Ether
qplot(factor(score), data=ether_currency, geom="bar",
      fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Sentiment Scores - Ether Currency")

#### Polarity plot - customer sentiments Litecoin
qplot(factor(polarity), data=litecoin_currency, geom="bar", 
      fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Sentiments - Litecoin Currency")

#### Sentiment score - Litecoin
qplot(factor(score), data=litecoin_currency, geom="bar",
      fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Sentiment Scores - Litecoin Currency")
