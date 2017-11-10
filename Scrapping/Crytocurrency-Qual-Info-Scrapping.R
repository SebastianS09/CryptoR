## ------------------------------------------------------------------------
library(rvest)
library(stringr)
library(dplyr)

# parce que les sigles comportent tous 3 ou 4 lettres
remove_not34 <- function (char) {
  char <- str_replace_all(char, fixed(" "), "")
  if (nchar(char) < 3 | nchar(char) > 4) {
    char <- NULL
  }
  return (char)
}


remove_numbers <- function (lis) {
  numbers <- c("1","2","3","4","5","6","7","8","9","10")
  for (i in seq_along(lis) ) {
    copie <- as.character(substring(lis[[i]], 1,1))
    if (copie %in% numbers) {
      lis[i] <- "O"
    }
    else if (copie == "$") {
      lis[i] <- "O"
    }
  }
  return(lis)
}

clean_list <- function (lis) {
  lis <- lapply(lis, remove_not34)
  lis <- lis [-which(sapply(lis, is.null))]
  lis <- remove_numbers(lis) 
  return (lis)
}

coinmarketcap <- read_html("https://coinmarketcap.com/all/views/all/")
coin_list <- coinmarketcap %>%
  html_nodes("#currencies-all > tbody") %>% 
  html_text() %>% 
  strsplit(split = "\n") %>% 
  unlist() %>% 
  clean_list() %>%
  lapply(toupper) %>% 
  unique(incomparables = FALSE)

coin_list500 <- coin_list[c(1:500)]

## ------------------------------------------------------------------------
banking_system <- list("bank", "banking", "lending", "interest rate", "debt", "credit", "loan") 

insurance <- list("insurance", "risk")

exchange_services <- list("trading", "market", "marketplace", "market_place", "portfolio", "exchange", "transaction")

payment_services <- list("payment services", "payment service", "means of payment", "payment", "funds", "transfer")

IT_services <- list("cloud", "storage")


categories <- list(banking_system,insurance,exchange_services,payment_services,IT_services)
names(categories) <- c("banking_system","insurance","exchange_services","payment_services","IT_services")

## ------------------------------------------------------------------------

library(rvest)
library(stringr)
library(dplyr)


get_text <- function (already_read_url, id_selector){
text <- already_read_url %>% 
         html_nodes(id_selector) %>% 
         html_text()
return (text)
}


lis_paragraphs <- function (url, lis){
# en recevant une liste de CSS selector, retourne le texte (sous forme de string)
# l'argument url doit déjà avoir été lu avec read_html
  result <- list()
  for (i in seq_along(lis)) {
    result[[i]] <- get_text(url,lis[[i]])
  }
  result <- unlist(result)
  return(result)
  }
  

text_filter <- function (text, filter_list){
  words <- unlist(strsplit(text, ' '))
  found_words <- intersect(words, unlist(filter_list))
  return (found_words )
}

text_filter <- function (filter_list,text){
  words <- unlist(strsplit(text, ' '))
  found_words <- intersect(words, unlist(filter_list))
  if (!identical(found_words, character(0))) {return (found_words)}
  else {return (NULL)}
}

## ------------------------------------------------------------------------
business_mole <- read_html("http://www.businessmole.com/best-altcoins-cryptocurrencies-invest-2017/")

ripple_id_selectors <- list(
  "#post-947 > section > p:nth-child(19)",
  "#post-947 > section > p:nth-child(20)",
  "#post-947 > section > p:nth-child(21)",
  "#post-947 > section > p:nth-child(22)",
  "#post-947 > section > p:nth-child(23)")

test <- lis_paragraphs(business_mole, ripple_id_selectors)

matches <- function(list,paragraphs) {
  temp <- function(x) {text_filter(x,paragraphs)}
  lapply(list,temp)
}

