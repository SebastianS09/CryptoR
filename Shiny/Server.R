# Define server logic 
library(lubridate)
#generate data needed globally
server <- function(input, output, session) {
 
  #import daily data with progress bar
  xts_day <- list()
  n <- length(symbol_list)
  withProgress(message = "Fetching Data", detail = "Cryptocompare API", value = 0, {
    for (i in 1:n) {
      xts_day[i] <- generate_data_ind("day",i)
      incProgress(1/n, detail = paste("getting",symbol_list[[i]],"|",i,"of",n))
          }
  })

  names(xts_day) <- symbol_list
  
  #Tab Descriptions
  output$InputDesc <- renderText("Summary of Cryptocurrency inputs")
  output$FinDesc <- renderText("Financial Summary")
  
  #table tab
  output$table <- renderTable(symbols)
    
  #plot tab
  startdate <- as.Date.POSIXct(unlist(lapply(FUN = function(x) { if (is.null(x)) {NULL} else {start(x)}},xts_day)))
    startplot <- ggplot(data = data.frame(startdate),aes(x=startdate)) +
      geom_histogram(bins = n, fill = "skyblue", col = "black") + ylim(c(0,10)) + 
      stat_bin(aes(y=..count.., label=ifelse(..count.. > 0, ..count.., "")), bins = n, geom="text", vjust=-1)  +
      ggtitle("New Cryptocurrencies per month") + ylab("") + xlab("Date") + 
      theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank()) 
  output$plot <- renderPlot(startplot)
  
  #Summary tab
  output$summary <- renderText(paste(n,"symbols loaded from CryptoCompare API"))
  
  #financial plot
  output$finsimple <- renderDygraph({dyCandlestick(dygraph(xts_day[[input$fintick]][,1:4]))})
  
}


