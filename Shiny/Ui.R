# Define UI for app that draws a histogram ----
library(dygraphs)  

ui <- navbarPage(tags$head(
    tags$style(
      HTML(".shiny-notification {
           height: 100px;
           width: 800px;
           position:fixed;
           top: calc(50% - 50px);;
           left: calc(50% - 400px);;
           }
           "))),
           
           title="CryptoCurrency Explorer", id="mainNavbarPage",
                   
  tabPanel("Inputs",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "Type", label = "Frequency: ", choices = c("day","hour")),
        actionButton(inputId = "Generate", label = "Generate Data")),
        mainPanel(
            tabsetPanel(type = "tabs",
              tabPanel("Summary", textOutput("summary")),
              tabPanel("Plot", plotOutput("plot")),
              tabPanel("Table", tableOutput("table"))
                        )
                  )
                  )
              ),
                  
  tabPanel("Financials",  
      sidebarLayout(
        textOutput("FinDesc"),
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Candle Stick Graphs", 
                               selectInput(inputId = "fintick", label = "Choose Cryptocurrency: ",choices = unlist(symbol_list)),
                              dygraphOutput("finsimple")),
                      tabPanel("Returns",
                               sidebarLayout(
                                sidebarPanel(
                                  
                                  uiOutput('dateslider'),
                                  
                                  checkboxGroupInput(inputId = "TickRetPlot", label = "Cryptocurrencies to plot", choices = unlist(symbol_list), selected = "BTC", inline = TRUE),
                                  actionButton(inputId = "RetRefresh", label = "Refresh")),
                               mainPanel(
                                 plotOutput("RetPlot")
                                 ))),
                      tabPanel("Volume"
                               ,sidebarLayout(
                                  sidebarPanel(
                                    selectInput(inputId = "VolType", label = "Volume in: ",choices = c("same cryptocurrency","USD"), selected = "USD"),
                                    checkboxGroupInput(inputId = "TickVolPlot", label = "Cryptocurrencies to plot", choices = unlist(symbol_list), selected = "BTC", inline = TRUE),
                                    actionButton(inputId = "VolRefresh", label = "Refresh")),
                                  mainPanel(dygraphOutput("VolPlot") 
                                    )
                               )
                        )
                  )
              )
          )     
      ),
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