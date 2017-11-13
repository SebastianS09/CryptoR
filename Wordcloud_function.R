#### Create a function for word cloud

#### Create a wordcloud to see what are the most used words when it comes to cryptocurrencies

#### Install packages
  install.packages("tm")  # for data mining
  install.packages("SnowballC") # for text stemming
  install.packages("wordcloud") # to generate wordclouds
  install.packages("RColorBrewer") # for different colours
  install.packages("rvest")

#### Necessary libraries
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer")
  library(rvest)

word_cld <- function(topic) {

      #### Enter the name of the topic (do not forget the Capital letter)
      text_extract <- function(topic) {
          demanded_page <- read_html(paste0("https://en.wikipedia.org/wiki/",topic))
          raw_text <- demanded_page %>%
          html_nodes("p") %>%
          html_text() %>%
          strsplit(split = "/n") %>% 
          unlist()
          raw_text
      }
      
      raw_text <- text_extract(topic)
      
      #### Load the data in a corpus
      raw_text <- Corpus(VectorSource(raw_text))
      
      # Replace useless special characters
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      raw_text <- tm_map(raw_text, toSpace, "/")
      raw_text <- tm_map(raw_text, toSpace, "@")
      raw_text <- tm_map(raw_text, toSpace, "\\|")
      
      # Convert text in all lowercase
      raw_text <- tm_map(raw_text, content_transformer(tolower))
      
      # Remove numbers
      raw_text <- tm_map(raw_text, removeNumbers)
      
      # Remove stopwords (i.e. words with little influence: a, an, the...)
      raw_text <- tm_map(raw_text, removeWords, stopwords("english"))
      
      # Remove undesired words (chosen after having displayed the list of words)
      raw_text <- tm_map(raw_text, removeWords, c("bitcoins", "bitcoin","one", "can", "first", "use")) 
      
      # Remove punctuation
      raw_text <- tm_map(raw_text, removePunctuation)
      
      # Remove spaces
      raw_text <- tm_map(raw_text, stripWhitespace)
      
      # Text stemming - removing all the derivatives of a word (plural, conjuguaison)
      #bitcoin_txt_dirty <- tm_map(bitcoin_txt_dirty, stemDocument)
      # Decided not to do it because then "transaction" becomes "transact" so it blurries the output
      
      # Generate the matrix of terms with their frequence
      dtm <- TermDocumentMatrix(raw_text) # gives the matrix of terms
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      head(d,20)
      
      # Create the wordcloud of words
      set.seed(1234)
      wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                max.words=100, random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(8, "Dark2"))
    
    }
