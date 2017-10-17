

library(data.table)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)

### Generation useful data for analysis
#generate xts tickers first using Ticker.R
#generate aggregated xts on set of commodities for particular metric

aggr <- function(input,metric = "close",coin = symbol_list) {
  col_data <- function(coin) {input[[coin]][,metric]}
  list <- lapply(X = coin, FUN = col_data)
  for(i in 1:length(list)) {if (is.null(list[[i]]) == TRUE) {list[[i]] <- NA}}
  out <- do.call(cbind,list)
  out[is.na(out)] <- 0
  names(out) <- names(input)
  return(out)
}

var_p <- function(input,start_date = '2017/01/01') {
  out <- (input[paste(start_date,"/",sep = ""),] - lag(input[paste(start_date,"/",sep = ""),]))/lag(input[paste(start_date,"/",sep = ""),])
  out[is.nan(out)] <- 0
  out[is.na(out)] <- 0
  return(out)
}
  
cumulative_var <- function(input) {
  out <- cumprod(input+1)
  return(out)
}

trading_data <- function(input,vars = metrics) {
  aggr2 <- function(x) {aggr(input,x)}
  out <- lapply(X= vars,FUN = aggr2)
  names(out) <- vars
  return(out)
}

returns <- function(input,start_day = '2017/01/01') {
  lapply(X = input, FUN = var_p, start_day)
}

      





