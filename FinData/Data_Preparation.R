

library(data.table)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)


# generate data chope les infos par crypto:
  # COSS
  # high   low   open
  # 0.35   0.31  0.33
  #
  # DASH
  # high   low   open
  # 1.01   0.87  0.90

# aggr agrège les données de manière à avoir des infos sur toutes les cryptos en même temps:
  # 


aggr <- function(input,metric = "tclose",coin = symbol_list) {
  col_data <- function(coin) {input[[coin]][,metric]}
  list <- lapply(X = coin, FUN = col_data)
  # applique une fonction à tous les éléments d'une liste et retourne une liste ; obligé d'utiliser une fonction
  for(i in 1:length(list)) {if (is.null(list[[i]]) == TRUE) {list[[i]] <- NA}}
  out <- do.call(cbind,list)
  out[is.na(out)] <- 0
  names(out) <- unlist(coin)
  return(out) 
}

# calcule les retours (variation d'une date donnée à une autre)
# input est ici une table de cryptos

var_p <- function(input,start_date = '2017/01/01') {
  out <- (input[paste(start_date,"/",sep = ""),] - lag(input[paste(start_date,"/",sep = ""),]))/lag(input[paste(start_date,"/",sep = ""),])
  out[is.nan(out)] <- 0
  out[is.na(out)] <- 0
  # is.nan = is not a number
  # is.na = pas une valeur
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

returns <- function(input, start_day = '2017/01/01') {
  lapply(X = input, FUN = var_p, start_day)
}

      





