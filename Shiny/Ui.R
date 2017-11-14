###Shiny UI

library(dygraphs)  
symbols_full <- jsonlite::fromJSON("https://min-api.cryptocompare.com/data/all/coinlist")$Data

ui <- navbarPage(
  tags$head(tags$style(
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
                numericInput(inputId = "CryptoNumber", label = "Top n cryptocurrencies", value = 20, min = 1, max = length(symbols_full)),
                selectInput(inputId = "Type", label = "Frequency: ", choices = c("day","hour")),
                actionButton(inputId = "Generate", label = "Generate Data")),
        mainPanel(
            tabsetPanel(type = "tabs",
              tabPanel("Summary", 
                      h5(read.table("https://raw.githubusercontent.com/SebastianS09/CryptoR/master/Data/Intro.txt",sep="\t"),align="left"),
                      h4(htmlOutput("summary")), align="center"),
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
                              dygraphOutput("finsimple")),
                      tabPanel("Returns",
                               sidebarLayout(
                                sidebarPanel(
                                  uiOutput('dateslider'),
                                  uiOutput('cryptochoiceRet'),
                                  actionButton(inputId = "RetRefresh", label = "Refresh")),
                               mainPanel(plotOutput("RetPlot"))
                               )),
                      tabPanel("Volume",
                               sidebarLayout(
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
  )
),
  tabPanel("Social",  
           sidebarLayout(htmlOutput("SocDesc",align = "center"),
              mainPanel(wellPanel(id='socialwell',
               tabsetPanel(type = "tabs",
                 tabPanel("Twitter",
                        sidebarLayout(
                          sidebarPanel(
                            textInput("TwittIn", "CrytoCurrency to analyse", value = "Bitcoin", placeholder = "Cryptocurrency"),
                                        actionButton("TwittRefresh", "Launch Analysis"),
                                        textOutput("waiting")),
                         mainPanel(textOutput("TwitterDesc"),
                                    plotOutput("TwittOut")))),
                 tabPanel("Twitter Configuration",
                          textInput("key_in","API key", value = "XXX"),
                          textInput("secret_in","API secret", value = "XXX"),
                          textInput("token_in","API token", value = "XXX"),
                          textInput("token_secret_in","API token secret", value = "XXX"),
                          actionButton("TwittAuth", "Authenticate"),
                          h5("Check Rstudio Console on first Authentication. Please find the API description ", a("here", href="https://developer.twitter.com/en/docs/basics/authentication/overview/authentication-and-authorization.html"))),
                 tabPanel("Wikipedia Category Scrapping",
                          dataTableOutput("scrapping")))
        )
      )
    )
  )
)