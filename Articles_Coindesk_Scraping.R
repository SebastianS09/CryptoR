#### Program to run sentiment analysis on the articles titles

library(rvest) # Necessary library for web scraping
library(dplyr) # Necessary for the pipe

# Import the content of CoinDesk in the R session

              # Content of the first page of CoinDesk
          coindesk <- read_html("https://www.coindesk.com/")
          art1 <- coindesk %>% 
              html_nodes("h3") %>%
              html_text()
          mart1 <- as.matrix(art1)
          
            # Content of the payments page of CoinDesk
          coindesk_payments <- read_html("https://www.coindesk.com/category/business-news/use-cases-verticals/payments/")
          art2 <- coindesk_payments %>% 
            html_nodes("h3") %>%
            html_text()
          mart2 <- as.matrix(art2)
          
            # Content of the Capital Markets page of CoinDesk
          coindesk_capitalmarkets <- read_html("https://www.coindesk.com/category/business-news/use-cases-verticals/capital-markets/")
          art3 <- coindesk_capitalmarkets %>% 
            html_nodes("h3") %>%
            html_text()
          mart3 <- as.matrix(art3)
          
            # Content of the Banking page of CoinDesk
          coindesk_banking <- read_html("https://www.coindesk.com/category/business-news/use-cases-verticals/banking/")
          art4 <- coindesk_banking %>% 
            html_nodes("h3") %>%
            html_text()
          mart4 <- as.matrix(art4)
          
          # Content of the Insurance page of CoinDesk
          coindesk_insurance <- read_html("https://www.coindesk.com/category/business-news/use-cases-verticals/insurance/")
          art5 <- coindesk_insurance %>% 
            html_nodes("h3") %>%
            html_text()
          mart5 <- as.matrix(art5)
          
          # Content of the Supply Chain page of CoinDesk
          coindesk_supply_chain <- read_html("https://www.coindesk.com/category/business-news/use-cases-verticals/supply-chain/")
          art6 <- coindesk_supply_chain %>% 
            html_nodes("h3") %>%
            html_text()
          mart6 <- as.matrix(art6)
          
          # Content of the Security page of CoinDesk
          coindesk_security <- read_html("https://www.coindesk.com/category/business-news/use-cases-verticals/security/")
          art7 <- coindesk_security %>% 
            html_nodes("h3") %>%
            html_text()
          mart7 <- as.matrix(art7)
          
          # Content of the Identity page of CoinDesk
          coindesk_identity <- read_html("https://www.coindesk.com/category/business-news/use-cases-verticals/identity/")
          art8 <- coindesk_identity %>% 
            html_nodes("h3") %>%
            html_text()
          mart8 <- as.matrix(art8)
          
          # Content of the Healthcare page of CoinDesk
          coindesk_health <- read_html("https://www.coindesk.com/category/business-news/use-cases-verticals/healthcare/")
          art9 <- coindesk_health %>% 
            html_nodes("h3") %>%
            html_text()
          mart9 <- as.matrix(art9)
          
          # Content of the Energy page of CoinDesk
          coindesk_energy <- read_html("https://www.coindesk.com/category/business-news/use-cases-verticals/energy/")
          art10 <- coindesk_energy %>% 
            html_nodes("h3") %>%
            html_text()
          mart10 <- as.matrix(art10)
          
          # Content of the IoT page of CoinDesk
          coindesk_iot <- read_html("https://www.coindesk.com/category/business-news/use-cases-verticals/internet-of-things/")
          art11 <- coindesk_iot %>% 
            html_nodes("h3") %>%
            html_text()
          mart11 <- as.matrix(art11)
          
          # Content of the Merchants page of CoinDesk
          coindesk_merchants <- read_html("https://www.coindesk.com/category/business-news/use-cases-verticals/uses-cases-verticals-merchants/")
          art12 <- coindesk_merchants %>% 
            html_nodes("h3") %>%
            html_text()
          mart12 <- as.matrix(art12)
          
          # Content of the Regulation page of CoinDesk
          coindesk_regulation <- read_html("https://www.coindesk.com/category/business-news/legal/regulation-legal/")
          art13 <- coindesk_regulation %>% 
            html_nodes("h3") %>%
            html_text()
          mart13 <- as.matrix(art13)
          
          # Content of the Central Banking page of CoinDesk
          coindesk_central_banking <- read_html("https://www.coindesk.com/category/business-news/legal/central-banking/")
          art14 <- coindesk_central_banking %>% 
            html_nodes("h3") %>%
            html_text()
          mart14 <- as.matrix(art14)
          
          # Content of the Crime page of CoinDesk
          coindesk_crime <- read_html("https://www.coindesk.com/category/business-news/legal/tax/")
          art15 <- coindesk_crime %>% 
            html_nodes("h3") %>%
            html_text()
          mart15 <- as.matrix(art15)
          
          # Content of the US & Canada page of CoinDesk
          coindesk_america <- read_html("https://www.coindesk.com/category/business-news/legal/us-canada/")
          art16 <- coindesk_america %>% 
            html_nodes("h3") %>%
            html_text()
          mart16 <- as.matrix(art16)
          
          # Content of the Asia page of CoinDesk
          coindesk_asia <- read_html("https://www.coindesk.com/category/business-news/legal/asia-pacific/")
          art17 <- coindesk_asia %>% 
            html_nodes("h3") %>%
            html_text()
          mart17 <- as.matrix(art17)
          
          # Content of the Europe page of CoinDesk
          coindesk_europe <- read_html("https://www.coindesk.com/category/business-news/legal/europe/")
          art18 <- coindesk_europe %>% 
            html_nodes("h3") %>%
            html_text()
          mart18 <- as.matrix(art18)

#### Combine all the matrix to have a final list of all the articles
    # mart is a column vector (used in the following sentiment analysis)
          
mart <- rbind(mart1,mart2,mart3,mart4,mart5,mart6,mart7,mart8,mart9,mart10,mart11,mart12,mart13,mart14,mart15,mart16,mart17,mart18)

                #### Run a sentiment analysis (positive/negative) to see if Coindesk articles are more up or down
                # Fortunately, there are only 10 articles per page, before scrolling, so we volontarily analysed these articles only to have a
                # Trend of the last week
                
                    ### Necessary libraries
                    
                    library(plyr) ## for breaking the data into manageable pieces
                    library(ROAuth) ## for R authentification
                    library(httr)
                    library(rjson)
                    library(stringr) ## for string processing
                    library(ggplot2) ## for plotting the results
                
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
                          scores = score.sentiment(mart, pos.words,neg.words , .progress='text')
                          
                          #### Compute the number of positive, negative and neutral statements
                          scores$positive <- as.numeric(scores$score >0)
                          scores$negative <- as.numeric(scores$score >0)
                          scores$neutral <- as.numeric(scores$score==0)
                
                          #### Create polarity variable for the CoinDesk output
                          scores$polarity <- ifelse(scores$score >0,"positive",
                                                  ifelse(scores$score < 0,"negative",ifelse(scores$score==0,"Neutral",0)))
                
                          #### Polarity plot - customer sentiments Bitcoin
                          #qplot(factor(polarity), data=bitcoin_currency, geom="bar", 
                          #  fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Sentiments - Bitcoin Currency")
                
                          #### Sentiment score - CoinDesk
                          qplot(factor(score), data=scores, geom="bar",
                                fill=factor(score))+xlab("Optimism") + ylab("Frequency")+ ggtitle("CoinDesk - Analysis")
