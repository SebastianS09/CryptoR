# Define UI for app that draws a histogram ----
library(dygraphs)  

ui <- navbarPage(
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           height: 100px;
           width: 800px;
           position:fixed;
           top: calc(50% - 50px);;
           left: calc(50% - 400px);;
           }","body {background-color: #EFFBFB; }"),
      type = "text/css", '#finwell {width: 130%}')),
  
  title="CryptoCurrency Explorer", id="mainNavbarPage",
  
  tabPanel("Inputs",
    sidebarLayout(
      sidebarPanel(width=4,
                numericInput(inputId = "CryptoNumber", label = "Top n cryptocurrencies", value = 100, min = 1, max = length(symbols_full)),
                selectInput(inputId = "Type", label = "Frequency: ", choices = c("day","hour")),
                actionButton(inputId = "Generate", label = "Generate Data")),
        mainPanel(
            tabsetPanel(type = "tabs",
              tabPanel("Summary", htmlOutput("summary"), align="center"),
              tabPanel("Plot", plotOutput("plot")),
              tabPanel("Table", dataTableOutput("table")))
                  )
                )
              ),
                  
  tabPanel("Financials",  
      sidebarLayout(
        htmlOutput("FinDesc",align = "center"),
        mainPanel(wellPanel(id='finwell',
          tabsetPanel(type = "tabs",
                      tabPanel("Candle Stick Graphs", 
                               uiOutput('cryptochoiceFin'),
                               #selectInput(inputId = "fintick", label = "Choose Cryptocurrency: ", choices = unlist(symbol_list),
                              dygraphOutput("finsimple")),
                      tabPanel("Returns",
                               sidebarLayout(
                                sidebarPanel(
                                  uiOutput('dateslider'),
                                  uiOutput('cryptochoiceRet'),
                                  actionButton(inputId = "RetRefresh", label = "Refresh")),
                               mainPanel(plotOutput("RetPlot"))
                               )),
                      tabPanel("Volume"
                               ,sidebarLayout(
                                  sidebarPanel(
                                    uiOutput('cryptochoiceVol'),
                                    actionButton(inputId = "VolRefresh", label = "Refresh")),
                                  mainPanel(selectInput(inputId = "VolType", label = "Volume in: ",choices = c("same cryptocurrency","USD"), selected = "USD"),
                                    dygraphOutput("VolPlot"))
                                    )
                               )
                        
                  )
              )
        )    
      )),
  tabPanel("Social",  
           sidebarLayout(
             sidebarPanel(
               textInput(inputId = "TwittIn", label = "CrytoCurrency to analyse", value = "Bitcoin", placeholder = "Cryptocurrency"),
               actionButton(inputId = "TwittRefresh", label = "Launch Analysis"),
               textOutput("waiting")),
             mainPanel(textOutput("TwitterDesc"),
               plotOutput("TwittOut")))
  )
)  