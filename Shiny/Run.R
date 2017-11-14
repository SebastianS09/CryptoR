##Run.R Exec file 

is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 

needed <- c("jsonlite","xts","ggplot2","dygraphs","PerformanceAnalytics","twitteR","plyr","ROAuth","httr","stringr")

is_inst_list <- lapply(needed,is.installed)
names(is_inst_list) <- needed 

for ( i in seq_along(needed)) {
  if (!is_inst_list[[i]]) {
    print(paste("installing",needed[i]))
    install.packages(needed[i])}
}

shiny::runGitHub("SebastianS09/CryptoR",subdir = "Shiny")
