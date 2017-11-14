### FUNCTIONS TO AGGREGATE THE RAW FINANCIAL DATA


#trading data creates list of xts objects (one per financial metrics) with the price and volume for a given day for every currency
#note: same as before, when called "tradig_data("days")", automatically takes as input "xts_day" and automatically generates "trading_day"
trading_data <- function(type,vars = metrics, coins = symbol_list,verbose = FALSE, inp=NULL) {
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
  
  #this function creaées a list for all specified metrics in var (default is high low open close volumefrom volumeto)
  if (verbose == TRUE) {input <- inp} else {input <- get(paste0("xts_",type),envir = .GlobalEnv)}
  aggr2 <- function(x) {aggr(input,x)}
  out <- lapply(X= vars,FUN = aggr2)
  names(out) <- vars
  
  #outputs the list of xts object called trading_day or trading_hour
  if (verbose == TRUE) {return(out)} else {assign(paste0("trading_",type),out,.GlobalEnv)}
}

#creates list of xts objects (one per financial metrics) with the % returns for a given day for every currency
#note: same logic
returns <- function(type, start_day = '2017-01-01', verbose = FALSE,inp=NULL) {
  
  # var_p calculates daily / hourly returns in % for one metric
  var_p <- function(input,start_date = start_day) {
    out <- (input[paste0(start_date,"::"),] - stats::lag(input[paste0(start_date,"::"),]))/stats::lag(input[paste0(start_date,"::"),])
    out[is.nan(out)] <- 0
    out[is.na(out)] <- 0
    # is.nan = is not a number
    # is.na = pas une valeur
    return(out)
  }
  
  # input is the trading_day or trading_hour list 
  if (verbose == TRUE) {out <- lapply(X = inp, FUN = var_p, start_day)
                        return(out)}
  #outputs the list of xts object called returns_day or returns_hour
  else {input <- get(paste0("trading_",type),envir = .GlobalEnv)
    out <- lapply(X = input, FUN = var_p, start_day)
    assign(paste0("returns_",type),out,.GlobalEnv)}
}


#creates list of xts objects (one per financial metrics) with the cumulative returns for a given day for every currency
#note: same logic
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
      
#Volume Analysis

#dyChart custom plots  
dyBarChart <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "BarChart",
            path = system.file("examples/plotters/barchart.js", package = "dygraphs"))
        }
      
dyMultiColumn <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "MultiColumn",
            path = system.file("examples/plotters/multicolumn.js", package = "dygraphs"))
        }
