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

# Load the stocks for which we will get data
repmis::source_data("https://github.com/HBossier/ShinyStocks/blob/master/R/Euronext.RDa?raw=true")

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
start <- end - weeks(40)
PlotType  <-  "Line Bar"

##
##########
### Plots
##########
##

# Line bar 
LB <- HighLowData %>% 
  ggplot(aes(x = date, y = close)) + 
     geom_line() +
    labs(title = paste0(STOCK, ": ", PlotType),
        subtitle = "Volatility",
         x = "", y = "Closing Price") +
    coord_x_date(xlim = c(start, end),
        ylim = c(HighLowData %>% filter(date > start & date < end) %>% select(low) %>% min(),
        HighLowData %>% filter(date > start & date < end) %>% select(high) %>% max())) 

# Volume
VOL <- HighLowData %>% filter(date > start & date < end) %>%
  ggplot(aes(x = date, y = volume)) + geom_bar(stat = 'identity', width = 1) +
  scale_y_continuous(labels = scales::scientific, position = 'left') +
  bdscale::scale_x_bd(business.dates = sort(HighLowData$date, decreasing = FALSE), max.major.breaks = 5)


# Together using scale_facet
FacettingData <- HighLowData %>% filter(date > start & date < end) %>%
  select(date, close, volume) %>% gather("statistic","value",2:3)
StockDataMain <- HighLowData %>% filter(date > start & date < end) %>%
  select(-volume, - adjusted) %>% mutate(statistic = "close")
StockDataVolume <- HighLowData %>% filter(date > start & date < end) %>%
  select(date, volume) %>% mutate(statistic = "volume")

ggplot(FacettingData, aes(x = date, y = value)) + 
  facet_grid(statistic ~. , scales="free_y") + 
  geom_line(data = StockDataMain, aes(x = date, y = close)) +
  geom_bar(data = StockDataVolume, aes(x = date, y = volume), stat = 'identity', width = 1) +
  labs(title = paste0(STOCK, ": ", PlotType),
       subtitle = "Volatility",
       x = "", y = "") +
    bdscale::scale_x_bd(business.dates = sort(HighLowData$date, decreasing = FALSE), max.major.breaks = 5) + theme_minimal()

DefScipen <- getOption('scipen')  
getOption('digits')  
options(digits = 2)
options(scipen=-2)
1000

# Try mapping the plot with gtable and grid
x <- seq(1992, 2002, by=2)

d1 <- data.frame(x=x, y=rnorm(length(x)))
xy <- expand.grid(x=x, y=x)
d2 <- data.frame(x=xy$x, y=xy$y, z= jitter(xy$x + xy$y))

p1 <-  ggplot(data = d1, mapping = aes(x = x, y = y)) + 
  geom_line(stat = "identity") 

p2 <-  ggplot(data = d2, mapping = aes(x=x, y=y, fill=z)) + 
  geom_tile()

## convert plots to gtable objects
library(gtable)
library(grid) # low-level grid functions are required
g1 <- ggplotGrob(p1)
g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
g2 <- ggplotGrob(p2)
g <- rbind(g1, g2, size="first") # stack the two plots
g$widths <- unit.pmax(g1$widths, g2$widths) # use the largest widths
# center the legend vertically
g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
grid.newpage()
grid.draw(g)


# Now with LB and VOL
g1 <- ggplotGrob(LB)
g2 <- ggplotGrob(VOL)
g <- rbind(g1, g2, size="first") # stack the two plots
g2$heights
g$widths
grid.newpage()
grid.draw(g)



# The problem now is that we first filter the dates, but then need to calculate the weighted average
HighLowData %>% mutate(SMA = SMA(close, n = 10)) %>% filter(date > start & date < end)
  

LBWMA <- HighLowData %>% mutate(SMA = WMA(close, n = 10)) %>% filter(date > start & date < end) %>%
  ggplot(aes(x = date, y = close)) + 
  geom_line(aes(x = date, y = close)) +
  geom_line(aes(x = date, y = SMA), colour = 'purple', linetype = 'dashed', size = 1) +
  bdscale::scale_x_bd(business.dates = sort(HighLowData$date, decreasing = FALSE), max.major.breaks = 5) + 
  labs(title = paste0(STOCK, ": ", PlotType),
       subtitle = "Volatility",
       x = "", y = "Closing Price") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

g1 <- ggplotGrob(LBWMA)
g2 <- ggplotGrob(VOL)
g <- rbind(g1, g2, size="first") # stack the two plots
grid.newpage()
grid.draw(g)

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

