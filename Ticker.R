### getting ticker data ###

library(httr)
library(jsonlite)
library(xts)

#set digits for timestamp editing 
options(digits.secs=6)

##convert unix timestamps to usual date format
set_time <- function(x) {
  if (is.null(x) == FALSE) {
    y <- as.POSIXct(x, origin="1970-01-01", tz="UTC")
    return(y)
  }
}

#create xts time series from raw data
make_xts <- function(x) {
  for (i in 1:length(x)){
    x[[i]]$time <- set_time(x[[i]]$time)
  }
  tickers_day <- lapply( function(x) {
    if (!length(x) == FALSE) {xts(x[,-1],order.by = x$time)}}, X = x)
  return(tickers_day)}

#export ticker data in csv
export_tickers <- function(x,type) {

  filename = paste("/Users/Sebastian/Documents/Data/Lugo/HitBTC/ticker_data_",type,"/",names(x), ".csv",sep = "")
  for (i in seq_along(x)) {
    write.csv(x[[i]], filename[i])
  }
}

##DATA GENERATION

#get symbols from HitBTC
symbols <- fromJSON("https://api.hitbtc.com/api/1/public/symbols")[[1]]
  #json <- GET("https://api.hitbtc.com/api/1/public/symbols")
  #str(content(json, "parsed"))
symbol_list <- as.list(unique(symbols$commodity))

metrics <- c("close","high","low","open","volumefrom","volumeto")

#get historical daily data from cryptocompare (- whithout exchange for the time being)

raw_day <- function() {
  get_ticker_day <- function(x) {
  out <- fromJSON(paste("https://min-api.cryptocompare.com/data/histoday?fsym=",x,"&tsym=USD&allData=true&aggregate=1&extraParams=raise",sep = ""))$Data
  return(out)
  }
  raw_tickers_day <- lapply(get_ticker_day,X = symbol_list)
  names(raw_tickers_day) <- symbol_list
return(raw_tickers_day)}

#get historical hourly data from cryptocompare (- whithout exchange for the time being)

raw_hour <- function() {
  get_ticker_hour <- function(x) {
  out <- fromJSON(paste("https://min-api.cryptocompare.com/data/histohour?fsym=",x,"&tsym=USD&limit=2000&aggregate=1&extraParams=raise",sep = ""))$Data
  return(out)
}
  raw_tickers_hour <- lapply(get_ticker_hour,X = symbol_list)
  names(raw_tickers_hour) <- symbol_list
  return(raw_tickers_hour)}

#SUMMARY: Import data, save locally and generate XTS list

generate_data <- function(type,export = FALSE, fetch = FALSE) {
    if (fetch == TRUE) {
      x <- do.call(paste("raw_",type,sep = ""),args = list())} 
    else {x <- get(paste("raw_tickers_",type,sep = ""))}
    if (export == TRUE) {export_tickers(x,type)}
    return(make_xts(x))
  }







