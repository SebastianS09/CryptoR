### Libraries 
library(jsonlite)
library(xts)
library(ggplot2)
library(dygraphs)
library(PerformanceAnalytics)

### GETTING RAW FINANCIAL DATA FROM CRYPTOCOMPARE API###

#note: the financial data functions are designed to either run with the shiny app, either be sourced from the file directly. In the second case, raw financial data and subsequent transformation will be assigned directly to new objects, the only parameter being "day" or "hour"


#set digits for timestamp editing (xts package requirement)
options(digits.secs=6)

#creating a function to make xts time series from raw data
make_xts <- function(x) {
    ##convert unix timestamps to usual date format
    set_time <- function(x) {
      if (is.null(x) == FALSE) {
        y <- as.POSIXct(x, origin="1970-01-01", tz="UTC")
        return(y)}
        }
  for (i in 1:length(x)) {x[[i]]$time <- set_time(x[[i]]$time)}
    
  tickers_day <- lapply(function(x) {if (!length(x) == FALSE) {xts(x[,-1],order.by = x$time)}}, X = x)
  return(tickers_day)}

#export ticker data in csv (direct use only)
export_tickers <- function(x,type) {
  if (!dir.exists(paste0("Ticker_Data_",type))) {dir.create(paste0("Ticker_Data_",type))} 
  filename = paste0(getwd(),"/Ticker_Data_",type,"/",names(x), ".csv")
  
  for (i in seq_along(x)) {
    write.csv(x[[i]], filename[i])
  }
}

##DATA GENERATION

#get symbols from CryptoCompare
symbols_full <- jsonlite::fromJSON("https://min-api.cryptocompare.com/data/all/coinlist")$Data

symbols <- list()
for (i in 1:length(symbols_full)) {symbols[i] <- symbols_full[[i]]["CoinName"]}
names(symbols) <- names(symbols_full)

top_crypto <- function(x) {jsonlite::fromJSON(paste0("https://api.coinmarketcap.com/v1/ticker/?start=0&limit=",x))$symbol}
top_API_symbols <- function(x) {intersect(top_crypto(x),names(symbols))}
symbol_list <- top_API_symbols(100)

metrics <- c("close","high","low","open","volumefrom","volumeto")

#SUMMARY: generate data through cryptocompare API, save locally and generate XTS list

generate_data <- function(type,export = FALSE, verbose = FALSE) {
  if (type == "day") {  url0 <- "https://min-api.cryptocompare.com/data/histoday?fsym="
                        url1 <- "&tsym=USD&allData=true&aggregate=1&extraParams=raise"} 
  
  else if (type == "hour") { url0 <- "https://min-api.cryptocompare.com/data/histohour?fsym="
                            url1 <- "&tsym=USD&limit=2000&aggregate=1&extraParams=raise"}   
  
  else  {stop("please enter day or hour")}

    #get historical data from cryptocompare (- whithout exchange for the time being)
    raw <- function() {
      get_ticker <- function(x) {
        out <- jsonlite::fromJSON(paste(url0,x,url1,sep = ""))$Data
        return(out)
      }
      raw_tickers <- lapply(get_ticker,X = symbol_list)
      names(raw_tickers) <- symbol_list
    return(raw_tickers)}
  
  
    #make xts table
    x <- do.call(raw,args = list()) 
    if (export == TRUE) {export_tickers(x,type)}
    if (verbose == TRUE) {return(make_xts(x))}
    else {assign(paste0("xts_",type),make_xts(x), envir = globalenv())}
    }

#function for use in shiny (same structure but single object to be able to make loops (necessary for progress bar))
generate_data_ind <- function(type,tick_num) {
  if (type == "day") {url0 <- "https://min-api.cryptocompare.com/data/histoday?fsym="
                      url1 <- "&tsym=USD&allData=true&aggregate=1&extraParams=raise"}

  else if (type == "hour") {url0 <- "https://min-api.cryptocompare.com/data/histohour?fsym="
                            url1 <- "&tsym=USD&limit=2000&aggregate=1&extraParams=raise"}   
  else  {stop("please enter day or hour")}

  i <- tick_num
  raw_tickers <- jsonlite::fromJSON(paste(url0,symbol_list[[i]],url1,sep = ""))$Data
  
  #make xts table
    set_time <- function(x) {
      if (is.null(x) == FALSE) {
        y <- as.POSIXct(x, origin="1970-01-01", tz="UTC")
        return(y)}
    }
    
  raw_tickers$time <- set_time(raw_tickers$time)
  if (!is.null(dim(raw_tickers))) {out <- xts(raw_tickers[,-1],order.by = raw_tickers$time)} else {out <- xts(,seq.Date(as.Date("2017-01-01"),Sys.Date(),by="day"))}
  return(out)
  }
                                   





