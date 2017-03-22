####################
#### TITLE:     Code to test creating plots 
#### Contents:  Raw Code
#### Author: Han Bossier
#### Source Files: //Github/HBossier
#### First Modified: 11/03/2017
#### Notes:
#################



##
##########
### Preparation
##########
##

# Load in packages
library(shiny)
library(tidyquant)
library(ggplot2)
library(dplyr)
library(tidyr)
require(repmis)
require(grid)
require(gridExtra)
require(bdscale)

##
##########
### Stocks
##########
##

# Check whether we are running the app locally (correct WD)
WD <- getwd()
if(grepl(pattern = 'StocksApp', x = WD)){
  # Load the stocks for which we will get data
  load('Euronext.RDa')
}else{
  repmis::source_data("https://github.com/HBossier/ShinyStocks/blob/master/R/Euronext.RDa?raw=true")
}

# Markets
markets <- Stocks %>% select(Market) %>% unique() %>% filter(grepl('Euronext', x = Market, ignore.case = TRUE))

# Select stock
STOCK <- "ABLYNX"

# Get data 
symbol <- Stocks %>% filter(Naam == STOCK) %>% select(Symbol)
suffix <- Stocks %>% filter(Naam == STOCK) %>% select(suffix)
HighLowData <- tq_get(paste0(symbol, ".", suffix), get = "stock.prices", from = " 1990-01-01")

# Weighted average
WA  <-  0
ManWA <- 50

# Other parameters
end <- today()
start <- end - weeks(10)
PlotType  <-  "Line Bar"

##
##########
### Plots
##########
##

# Line bar 
HighLowData %>% 
  ggplot(aes(x = date, y = close)) + 
     geom_line() +
    labs(title = paste0(STOCK, ": ", PlotType),
        subtitle = "Volatility",
         x = "", y = "Closing Price") +
    coord_x_date(xlim = c(start, end),
        ylim = c(HighLowData %>% filter(date > start & date < end) %>% select(low) %>% min(),
        HighLowData %>% filter(date > start & date < end) %>% select(high) %>% max())) 

# Volume
HighLowData %>% filter(date > start & date < end) %>%
  ggplot(aes(x = date, y = volume)) + geom_bar(stat = 'identity') +
  bdscale::scale_x_bd(business.dates = sort(HighLowData$date, decreasing = FALSE), max.major.breaks = 5)







#### OLDER TEST CODE
# 
# HighLowData %>% filter(date > start & date < end) %>%
#   ggplot(aes(x = factor(format(date, format = '%d%b%y')), y = volume)) + geom_bar(stat="identity")  + theme_minimal() + 
#   #scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
#   #coord_x_date(xlim = c(start, end)) + 
#   theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=90))
# 
# HighLowData %>% filter(date > start & date < end) %>%
#   ggplot(aes(x = date, y = volume)) + geom_bar(stat = 'identity') +
#   scale_x_discrete(name = 'Date', breaks = NULL)
# 
# HighLowData %>% filter(date > start & date < end) %>%
#   ggplot(aes(x = factor(format(date, format = '%d%b')), y = volume)) + geom_bar(stat = 'identity') +
#   scale_x_discrete(name = 'Date', breaks = waiver(), labels = rep('test',9))
# 
# 
# HighLowData %>% filter(date > start & date < end) %>%
#   ggplot(aes(x = seq(1:dim(.)[1]), y = volume)) + geom_bar(stat = 'identity') +
#   scale_x_continuous(name = 'Date', breaks = seq(1,9,length.out = 4), labels = 
#                        format(date, format = '%d%b')))
# 
# 
# xlim = c(start, end)
#   
# scale_x_date(labels = HighLowData$date)
# 
# 
# test <- HighLowData %>% filter(date > start & date < end) %>% mutate(ID = seq_along(date))
# 
# test <- HighLowData %>% filter(date > start & date < end) %>% mutate(ID = seq_along(date), 
#                     DayOrigin = as.numeric(as.Date(x = date, origin = origin)))
# dateFU <- function(...,start){
#   as.Date(..., origin = start)
# }
# dateFU(1, start = start)
# test %>%  ggplot(aes(x = ID, y = volume)) + geom_bar(stat = 'identity') +
#   scale_x_continuous(name = 'Date', labels = as.Date(test$DayOrigin))
# 
# test <- HighLowData %>% filter(date > start & date < end) 
# test %>%  ggplot(aes(x = date, y = volume)) + geom_bar(stat = 'identity') +
#   bdscale::scale_x_bd(business.dates = sort(test$date, decreasing = FALSE), max.major.breaks = 5)
# 
# 
# ?bdscale::scale_x_bd
# 
# factor(format(test$date, format = '%d%b'))
# format(test$date, format = '%d%b')

