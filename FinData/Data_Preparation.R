# generate data chope les infos par crypto:
  # COSS
  # high   low   open volumefrom volumeto
  # 0.35   0.31  0.33 1000        1500
  #
  # DASH
  # high   low   open volumefrom volumeto
  # 1.01   0.87  0.90 2000        3549

#creates list of xts objects (one per financial metrics) with the price and volume for a given day for every currency
trading_data <- function(type,vars = metrics, coins = symbol_list) {
  # aggr creates one xts object per financial metric with all the cryptocurrencies as columns from raw API data stored in xts_day or xts_hour
  aggr <- function(input,metric,coin = coins) {
    col_data <- function(coin) {input[[coin]][,metric]}
    # applique une fonction à tous les éléments d'une liste et retourne une liste ; obligé d'utiliser une fonction
    list <- lapply(X = coin, FUN = col_data)
    
    for(i in 1:length(list)) {if (is.null(list[[i]]) == TRUE) {list[[i]] <- NA}}
    
    out <- do.call(cbind,list)
    out[is.na(out)] <- 0
    names(out) <- unlist(coin)
    return(out) 
  }
  
  #this function creates a list for all specified metrics in var (default is high low open close volumefrom volumeto)
  input <- get(paste0("xts_",type),envir = .GlobalEnv)
  aggr2 <- function(x) {aggr(input,x)}
  out <- lapply(X= vars,FUN = aggr2)
  names(out) <- vars
  
  #outputs the list of xts object called trading_day or trading_hour
  assign(paste0("trading_",type),out,.GlobalEnv)
}

#creates list of xts objects (one per financial metrics) with the % returns for a given day for every currency
returns <- function(type, start_day = '2017/01/01') {
  
  # var_p calculates daily / hourly returns in % for one metric
  var_p <- function(input,start_date = start_day) {
    out <- (input[paste(start_date,"/",sep = ""),] - lag(input[paste(start_date,"/",sep = ""),]))/lag(input[paste(start_date,"/",sep = ""),])
    out[is.nan(out)] <- 0
    out[is.na(out)] <- 0
    # is.nan = is not a number
    # is.na = pas une valeur
    return(out)
  }
  
  # input is the trading_day or trading_hour list 
  input <- get(paste0("trading_",type),envir = .GlobalEnv)
  out <- lapply(X = input, FUN = var_p, start_day)
  
  #outputs the list of xts object called returns_day or returns_hour
  assign(paste0("returns_",type),out,.GlobalEnv)
}


#creates list of xts objects (one per financial metrics) with the cumulative returns for a given day for every currency
cumreturns <- function(type) {
  
  # cum_var calculates cumulative returns
    cum_var <- function(input) {
    out <- cumprod(input+1)
    return(out)
  }
  
  # input is the returns_day or returns_hour list 
  input = get(paste0("returns_",type),envir = .GlobalEnv)
  out <- lapply(X = input, FUN = cum_var)
  
  #outputs the list of xts object called returns_day or returns_hour
  assign(paste0("cum_returns_",type),out,.GlobalEnv)
}
      





