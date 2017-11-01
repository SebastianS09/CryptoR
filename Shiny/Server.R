# Define server logic 

#generate data needed globally
server = function(input, output, session) {
  
  
  # Move to results page
  updateNavbarPage(session, "mainNavbarPage", selected="taboutput")
  #source("Global.R") 
  test <- list()
  n <- length(symbol_list)
  withProgress(message = "Fetching Data", detail = "Cryptocompare API", value = 0, {
    for (i in 1:n) {
      test[i] <- generate_data_ind("day",i)
      incProgress(1/n, detail = paste("getting",symbol_list[[i]]))
          }
    
  })
  names(test) <- symbol_list
 print(summary(test)) 
}


