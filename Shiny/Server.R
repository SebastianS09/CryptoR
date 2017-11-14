#library(xts)
library(jsonlite)
library(ggplot2)
#library(dygraphs)
#library(PerformanceAnalytics)

##### Set API Keys for twitter (entering keys from the Twitter app)
#twitter authentication override
# 
# api_key <- "2HseC7OxKqXBlVMH4v7Y9Gkf0"
# api_secret <- "Bxw0mLKLsFtXKqLlOemidBcrxnvEDujHKwOKncmptwI1iE3sFU"
# access_token <- "722722544197988352-06Kc8MOizeFVYtZPbk3y4xmrxgo9CNp"
# access_token_secret <- "3fb0RuY0GsQ6lP2VcBcUd58Yx97Qzid21VaIGPmyxlSEC"
# 
# setup_twitter_oauth(api_key,api_secret)

server <- function(input, output, session) {


  symbols_full <- jsonlite::fromJSON("https://min-api.cryptocompare.com/data/all/coinlist")$Data

  source("https://raw.githubusercontent.com/SebastianS09/CryptoR/master/FinData/Ticker.R")
  source("https://raw.githubusercontent.com/SebastianS09/CryptoR/master/FinData/Data_Preparation.R")
  source("https://raw.githubusercontent.com/SebastianS09/CryptoR/master/Twitter/Sentiment_Analysis_Twitter.R")
  #source("Untitled.R")
  
  updateNavbarPage(session = session, "mainNavbarPage", selected="Inputs")
  symbol_list <- eventReactive(input$Generate,top_API_symbols(input$CryptoNumber))
  n <- reactive({length(symbol_list())})
  
  #import daily data with progress bar
  xts_day2 <- reactiveValues()
  observeEvent(input$Generate, {withProgress(message = "Fetching Data", detail = "Cryptocompare API", value = 0, {
    for (i in 1:n()) {
      xts_day2$data[[i]] <- generate_data_ind(input$Type,i)
      names(xts_day2$data)[i] <- symbol_list()[[i]]
      incProgress(1/n(), detail = paste("getting",symbol_list()[[i]],"|",i,"of",n()))
    }
  })
  })
  
  #generate trading data as reac_trading
  reac_trading <- reactiveValues()
  observe({if (!is.null(xts_day2$data)) {reac_trading$data <- trading_data(NULL,verbose = TRUE, inp = xts_day2$data)}})
  
  #generate return data as reac_returns
  reac_returns <- reactiveValues()
  observe({if (!is.null(input$rebase)) {
    rebaseDate <- input$rebase
    if (sum(c(gregexpr("-",rebaseDate)[[1]])==c(5,8))==2) {reac_returns$data <- returns(NULL,start_day = input$rebase,verbose = TRUE,inp = reac_trading$data)}
  }})
  
  
  ##OUTPUTS  
  #Tab Descriptions
  output$InputDesc <- renderText("Summary of Cryptocurrency inputs")
  output$FinDesc <- renderText(paste("<b>","Financial Summary","<br>"))
  
  #Summary tab
  observe({if (is.null(xts_day2$data)) {output$summary <- renderText(paste("<b>",length(symbols_full),"symbols available from CryptoCompare API","<br>"))} else {output$summary <- renderText(paste("<b>",n(),"symbols loaded from CryptoCompare API","<br>"))}})
  
  #plot tab
  summaryplot <- reactiveValues()
  
  observe({if (!is.null(xts_day2) & input$Type == "day") {
    summaryplot$data <- as.Date.POSIXct(unlist(lapply(FUN = function(x) { if (is.null(x)) {NULL} else {start(x)}},xts_day2$data)))
    summaryplot$plot <- ggplot(data = data.frame(summaryplot$data),aes(x=summaryplot$data)) +
      geom_histogram(bins = n(), fill = "skyblue", col = "black") + ylim(c(0,20)) + 
      stat_bin(aes(y=..count.., label=ifelse(..count.. > 0, ..count.., "")), bins = n(), geom="text", vjust=-1)  +
      ggtitle("New Cryptocurrencies per month") + ylab("") + xlab("Date") + 
      theme(text = element_text(size=14), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank())}
    else {
      summaryplot$data <- NULL
      summaryplot$plot <- ggplot(data.frame(summaryplot$data),aes(x=summaryplot$data))}
  })
  
  observe({output$plot <- renderPlot(summaryplot$plot)})
  
  #table tab
  output$table <- renderDataTable(data.frame(do.call(rbind,symbols_full[unlist(symbol_list())])))
  
  #financial plot
  output$finsimple <- renderDygraph({dyCandlestick(dygraph(xts_day2$data[[input$fintick]][,1:4]))})
  
  #Returns plot
  date <- reactiveValues()
  observe({if(!is.null(input$TickRetPlot)) {date$min <- max(as.Date.POSIXct(unlist(lapply(FUN = function(x) 
  {if (is.null(x)) {NULL} else {start(x)}},xts_day2$data[input$TickRetPlot]))))} else {date$min <- Sys.Date()}})
  observe({date$max <- Sys.Date()})
  
  observe({output$dateslider <- renderUI({
    sliderInput("rebase","base date for returns:",min = date$min ,max= date$max, value = date$min)})})
  
  output$cryptochoiceFin <- renderUI({
    selectInput(inputId = "fintick", label = "Choose Cryptocurrency: ", choices = unlist(symbol_list()))})
  
  output$cryptochoiceRet <- renderUI({
    checkboxGroupInput("TickRetPlot", "Cryptocurrencies to plot", choices = unlist(symbol_list()), selected = "BTC", inline = TRUE)})
  
  output$cryptochoiceVol <- renderUI({
    checkboxGroupInput("TickVolPlot", "Cryptocurrencies to plot", choices = unlist(symbol_list()), selected = "BTC", inline = TRUE)})
  
  
  Ret_reac_plot <- eventReactive(input$RetRefresh, reac_returns$data[["close"]][,input$TickRetPlot])
  output$RetPlot <- renderPlot(charts.PerformanceSummary(Ret_reac_plot(),main = "Cryptocurrency Perfomance",bg="transparent"))

  #Volume Plot
  vol <- reactiveValues()
  observe({if(input$VolType == "USD") {vol$type <- "volumeto"} else {vol$type <- "volumefrom"}})
  lengthVol <- reactive({length(input$TickVolPlot)})  

    output$text <- renderText(strVol())
  
  Vol_reac_plot <- eventReactive(input$VolRefresh, reac_trading$data[[vol$type]][,input$TickVolPlot]/(1000000*(vol$type=="volumeto")+1))
  observe({if (lengthVol()<=1) {
              output$VolPlot <- renderDygraph(dyBarChart(dygraph(Vol_reac_plot())))}
          else if (lengthVol()>1) {output$VolPlot <- renderDygraph(dyMultiColumn(dygraph(Vol_reac_plot())))}
  })
  
  output$waiting <- renderText("Please check console for progress and event notification Watch out for API call limitations")
  output$TwitterDesc <- renderText("Twitter sentiment analysis")  
  
  
  ###Social 
  Twitt_reac_plot <- eventReactive(input$TwittRefresh, crypto_sentiment(input$TwittIn))
  output$TwittOut <- renderPlot(Twitt_reac_plot())
  
    #Authentication
  in_cred <- reactiveValues()
  
  observe({in_cred$data <- list(input$key_in,input$secret_in,input$token_in,input$token_secret_in)})
  
  #observeEvent(input$TwittAuth, {
   #               myapp <- oauth_app("Sebastian S",
    #                                 key = input$key_in,
     #                                secret = input$secret_in)
      #                
       #           twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)})
  
  observeEvent(input$TwittAuth,
                {local(setup_twitter_oauth(in_cred$data[[1]],in_cred$data[[2]],in_cred$data[[3]],in_cred$data[[4]]))})
}
