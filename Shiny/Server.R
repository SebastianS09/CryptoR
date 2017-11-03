# Define server logic 
library(lubridate)
library(PerformanceAnalytics)

#generate data needed globally
server <- function(input, output, session) {
  
  xts_day2 <- reactiveValues()
  n <- length(symbol_list)
  
  #import daily data with progress bar
  
  observeEvent(input$Generate, {withProgress(message = "Fetching Data", detail = "Cryptocompare API", value = 0, {
    for (i in 1:n) {
      xts_day2$data[[i]] <- generate_data_ind(input$Type,i)
      names(xts_day2$data)[i] <- symbol_list[[i]]
      incProgress(1/n, detail = paste("getting",symbol_list[[i]],"|",i,"of",n))
      }
    })
  })
  
  #generate trading data as reac_trading
  reac_trading <- reactiveValues()
  
  observe({if (!is.null(xts_day2$data)) {reac_trading$data <- trading_data(NULL,verbose = TRUE, inp = xts_day2$data)}})
  #observe({reac_trading$data <- CalculateReturns(xts_day2$data[["BTC"]][,"close"])})
  
  

  #generate return data as reac_returns
      reac_returns <- reactiveValues()
      observe({rebaseDate <- input$rebase
        if (sum(c(gregexpr("-",rebaseDate)[[1]])==c(5,8))==2) {reac_returns$data <- returns(NULL,start_day = input$rebase,verbose = TRUE,inp = reac_trading$data)}
      })
  
    Ret_reac_plot <- eventReactive(input$RetRefresh,reac_returns$data[["close"]][,input$TickRetPlot])
    output$RetPlot <- renderPlot(charts.PerformanceSummary(Ret_reac_plot(),main = "Crypto Perfomance"))

  #generate cumulative return data as reac_cumret -- unnecessary so far 
  #reac_cumret <- reactive({lapply(reac_returns$data, function(x) {cumprod(x+1)})})
  
  #Tab Descriptions
  output$InputDesc <- renderText("Summary of Cryptocurrency inputs")
  output$FinDesc <- renderText("Financial Summary")
  
  #Summary tab
  output$summary <- renderText(paste(n,"symbols loaded from CryptoCompare API"))
  
  #plot tab
  summaryplot <- reactiveValues()
  observe({summaryplot$data <- as.Date.POSIXct(unlist(lapply(FUN = function(x) { if (is.null(x)) {NULL} else {start(x)}},xts_day2$data)))})
  observe({summaryplot$plot <- ggplot(data = data.frame(summaryplot$data),aes(x=summaryplot$data)) +
      geom_histogram(bins = n, fill = "skyblue", col = "black") + ylim(c(0,12)) + 
      stat_bin(aes(y=..count.., label=ifelse(..count.. > 0, ..count.., "")), bins = n, geom="text", vjust=-1)  +
      ggtitle("New Cryptocurrencies per month") + ylab("") + xlab("Date") + 
      theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank())}) 
  output$plot <- renderPlot(summaryplot$plot)
  
  #table tab
  output$table <- renderTable(symbols)
  
  
  #financial plot
  output$finsimple <- renderDygraph({dyCandlestick(dygraph(xts_day2$data[[input$fintick]][,1:4]))})
  
}
