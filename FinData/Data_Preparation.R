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





trading_data <- function(type,vars = metrics) {
  
  aggr <- function(input,metric = "tclose",coin = symbol_list) {
    col_data <- function(coin) {input[[coin]][,metric]}
    # applique une fonction à tous les éléments d'une liste et retourne une liste ; obligé d'utiliser une fonction
    list <- lapply(X = coin, FUN = col_data)
    
    for(i in 1:length(list)) {if (is.null(list[[i]]) == TRUE) {list[[i]] <- NA}}
    
    out <- do.call(cbind,list)
    out[is.na(out)] <- 0
    names(out) <- unlist(coin)
    return(out) 
  }
  
  input <- get(paste0("xts_",type),envir = .GlobalEnv)
  
  aggr2 <- function(x) {aggr(input,x)}
  
  out <- lapply(X= vars,FUN = aggr2)
  names(out) <- vars
  
  assign(paste0("trading_",type),out,.GlobalEnv)
}

returns <- function(type, start_day = '2017/01/01') {
  
  # calcule les retours (variation d'une date donnée à une autre)
  # input est ici une table de cryptos
  
  var_p <- function(input,start_date = start_day) {
    out <- (input[paste(start_date,"/",sep = ""),] - lag(input[paste(start_date,"/",sep = ""),]))/lag(input[paste(start_date,"/",sep = ""),])
    out[is.nan(out)] <- 0
    out[is.na(out)] <- 0
    # is.nan = is not a number
    # is.na = pas une valeur
    return(out)
  }
  
  input <- get(paste0("trading_",type),envir = .GlobalEnv)
  
  out <- lapply(X = input, FUN = var_p, start_day)
  
  assign(paste0("returns_",type),out,.GlobalEnv)
}

cumreturns <- function(type) {
  
  cum_var <- function(input) {
    out <- cumprod(input+1)
    return(out)
  }
  
  input = get(paste0("returns_",type),envir = .GlobalEnv)
  
  out <- lapply(X = input, FUN = cum_var)
  
  assign(paste0("cum_returns_",type),out,.GlobalEnv)
}
      





