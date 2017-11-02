# Define UI for app that draws a histogram ----
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
        textOutput("InputDesc"),
        mainPanel(
            tabsetPanel(type = "tabs",
              tabPanel("Summary", verbatimTextOutput("summary")),
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
                               selectInput(inputId = "fintick", label = "Choose Crypto: ",choices = unlist(symbol_list)),
                              dygraphOutput("finsimple")),
                      tabPanel("Plot"),
                      tabPanel("Table")
                      )
                  )
              )
               
      )
  )
                 
    