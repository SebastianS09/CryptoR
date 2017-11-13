# Qualtitative information 

# The aim here is to obtain some deeper information about the specificities
# of each cryptocurrency through the scraping of dedicated websites.

# But before we explain our methodology, let's start with
# a list of the 500 major crptocurrencies.

library(rvest)
library(stringr)
library(dplyr)
library(glue)

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


# In which economic fields do we find cryptocurrencies ?  

# So we begin by defining potential economic fields through a list of keywords:
# (from what we know it's mostly the service industry)

banking_system <- list("bank", "banking", "lending", "interest rate", "debt", "credit", "loan") 

insurance <- list("insurance", "risk", "contracts")

exchange_services <- list("trading", "market", "marketplace", "market_place", "portfolio", "exchange", "transaction")

payment_services <- list("payment services", "payment service", "means of payment", "payment", "funds", "transfer")

IT_services <- list("cloud", "storage")

categories <- list(Banking_system = banking_system,
                   Insurance = insurance,
                   Exchange_services = exchange_services,
                   Payment_services = payment_services,
                   IT_services = IT_services)


# Here are the functions that we are going to use:

get_text <- function (already_read_url, id_selector){
  text <- already_read_url %>% 
    html_nodes(id_selector) %>% 
    html_text()
  return (text)
}


text_filter <- function (text, filter_list){
  words <- unlist(strsplit(as.character(text), ' '))
  found_words <- intersect(words, unlist(filter_list))
  if (!identical(found_words, character(0))) {return (found_words)}
  else {return (NULL)}
}


one_single_character <- function (chars){
  if (length(chars) == 0 ){
    return (NA)
  }
  else{
    result <- ""
    for (i in 1:length(chars)){
      result <- paste(chars[i], result, sep = ", ")}
    result <- gsub('.{2}$', '', result)
    return (result) }
}


multiple_paragraphs_on_wiki <- function (lis) {
  result <- list()
  for (i in seq_along(lis)) {
    parameter <- as.character(lis[i])
    url <- glue("https://en.wikipedia.org/wiki/{parameter}")
    crypto <- read_html(url) %>%
      html_nodes("p") %>%
      html_text()
    crypto <- crypto %>%
      str_replace_all(pattern = "\n", replacement = " ") %>%
      str_replace_all(pattern = "[[:punct:]]", replacement = "") %>%
      str_replace_all(pattern = "[[:digit:]]", replacement = "")
    
    result[[paste0("crypto", i)]] <- crypto
  }
  return(result)
}


# We are going to scrape the Wikipedia pages of the 5 major cryptocurrencies and put the results in a table:

five_major <- list("Bitcoin","Ethereum", "Litecoin", "Ripple_(payment_protocol)", "Dash_(cryptocurrency)")

tableau_def <- data.frame(matrix(ncol = 5, nrow = 5))
col <- c("Bitcoin", "Ether", "Litecoin", "Ripple", "Dash")
colnames(tableau_def) <- col
rownames(tableau_def) <- names(categories)


# Bitcoin
tableau_def[1,1] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = banking_system)[1], use.names = FALSE))

tableau_def[2,1] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = insurance)[1], use.names = FALSE))

tableau_def[3,1] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = exchange_services)[1], use.names = FALSE))

tableau_def[4,1] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = payment_services)[1], use.names = FALSE))

tableau_def[5,1] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = IT_services)[1], use.names = FALSE))


# Ethereum
tableau_def[1,2] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = banking_system)[1], use.names = FALSE))

tableau_def[2,2] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = insurance)[2], use.names = FALSE))

tableau_def[3,2] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = exchange_services)[2], use.names = FALSE))

tableau_def[4,2] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = payment_services)[2], use.names = FALSE))

tableau_def[5,2] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = IT_services)[2], use.names = FALSE))


# Litecoin
tableau_def[1,3] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = banking_system)[3], use.names = FALSE))

tableau_def[2,3] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = insurance)[3], use.names = FALSE))

tableau_def[3,3] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = exchange_services)[3], use.names = FALSE))

tableau_def[4,3] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = payment_services)[3], use.names = FALSE))

tableau_def[5,3] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = IT_services)[3], use.names = FALSE))


# Ripple
tableau_def[1,4] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = banking_system)[4], use.names = FALSE))

tableau_def[2,4] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = insurance)[4], use.names = FALSE))

tableau_def[3,4] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = exchange_services)[4], use.names = FALSE))

tableau_def[4,4] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = payment_services)[4], use.names = FALSE))

tableau_def[5,4] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = IT_services)[4], use.names = FALSE))


# Dash
tableau_def[1,5] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = banking_system)[5], use.names = FALSE))

tableau_def[2,5] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = insurance)[4], use.names = FALSE))

tableau_def[3,5] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = exchange_services)[5], use.names = FALSE))

tableau_def[4,5] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = payment_services)[5], use.names = FALSE))

tableau_def[5,5] <- one_single_character(
  unlist(lapply(parag_five, text_filter, filter_list = IT_services)[5], use.names = FALSE))

tableau_def[is.na(tableau_def)] <- ""

View(tableau_def)


# Export in to pdf:;

library(gridExtra)
pdf("Qualtitative Information-recap.pdf", height=8, width=14)
grid.table(tableau_def)
dev.off()


