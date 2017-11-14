# CryptoR
A package to gather cryptocurrency-related information 

The R project we have chosen to lead had an intimate link with a broader subject that particularly attracts us: cryptocurrencies. As a matter of fact, we wanted to take advantage of the exercise to create indicators of the state of a cryptocurrency (i.e. its financial or social perception). Gathering all this information – through APIs, websites or social networks – has enabled us to code a Shiny application, displaying the output of our researches. 

The application includes three parts: financial, Twitter sentiment analysis and a table summarizing the sector which the cryptocurrency belongs to. To dig in deeper in what we have done, the financial part is composed of various elements: the price, the volume and the returns for up to 1700 registered cryptocurrencies we obtained thanks to the API of cryptocompare.com and coinmarketcap.com. The Twitter sentiment analysis scans the 500 latest tweets containing the word Bitcoin for instance and tells if it is more “positive” or “negative” thanks to a lexicon of positive and negative words we have put on GitHub. Eventually, the table lays out if a currency belongs to the “payment”, or “exchange” sectors for example, i.e. the goal of the currency.  

Please find in the summary tab of the Shiny Application a quick paragraph we have written which summarizes the reasons behind our interest for cryptocurrencies.


To run the app, please run 

source("https://raw.githubusercontent.com/SebastianS09/CryptoR/master/Shiny/Run.R")

in your RStudio console, which will source a file checking for package requirements and remotely execute the App directly from github.


All files contain comments explaining their logic. 

As we want to further work on this project, some files are not used in the app for the time being (coindesk scrapping and word cloud). 

We are available to answer any question you may have,



The CryptoR team
