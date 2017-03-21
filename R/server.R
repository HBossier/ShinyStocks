####################
#### TITLE:     Shiny App to check key stock information: server.R part
#### Contents:  Raw Code
#### Author: Han Bossier
#### Source Files: //Github/HBossier
#### First Modified: 11/03/2017
#### Notes:
#################


##
##########
### Notes
##########
##


# This is a Shiny web application. To run, open this file in RStudio and click the
# 'Run App' button above.

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

##
##########
### Server
##########
##


server <- function(input, output) {

  # Generate data, based on selection of the stock
  output$stockSelection <- renderUI({
      selectInput("selectedstock", label = 'Stocks', choices = Stocks %>% filter(Market == input$markets) %>% select(Naam), selected = 'ABLYNX')
  })
  

  # Candle plot 
  output$candlePlot <- renderPlot({
    if(is.null(input$selectedstock)){
      STOCK <- 'ABLYNX'
    }else{
      STOCK <- input$selectedstock
    }
    # Get WA
    if(is.null(input$WA)){
     WA <- 0
     ManWA <- 0
    }else{
      WA  <-  input$WA
      ManWA <- input$manWA
    }
    # Get parameters
    end <- today()
    start <- end - weeks(input$weeks)
    PlotType  <-  input$PlotType
    symbol <- Stocks %>% filter(Naam == STOCK) %>% select(Symbol)
    suffix <- Stocks %>% filter(Naam == STOCK) %>% select(suffix)
    HighLowData <- tq_get(paste0(symbol, ".", suffix), get = "stock.prices", from = " 1990-01-01")
        # Run plot 
      HighLowData %>% 
        ggplot(aes(x = date, y = close)) + {
          if( PlotType == "Line Bar") geom_line() else geom_candlestick(aes(open = open, close = close, high = high, low = low)) 
        } + {
          # Weigthed moving averages with n the number of days
          if( "1" %in% WA ) geom_ma(ma_fun = WMA, n = 150, color = "red", linetype = 4, size = 1)
        } + {
          if( "2" %in% WA)  geom_ma(ma_fun = WMA, n = 50, color = "green", linetype = 4, size = 1)
        } + {
          if( "3" %in% WA ) geom_ma(ma_fun = WMA, n = 20, color = "blue", linetype = 4, size = 1)
        } + {
          if("4" %in% WA) geom_ma(ma_fun = WMA, n = max(1, ManWA), color = "purple", linetype = 4, size = ifelse(ManWA == 0, 0, 1))
        } +
      labs(title = paste0(STOCK, ": ", PlotType),
           subtitle = "Volatility",
           x = "", y = "Closing Price") +
      coord_x_date(xlim = c(start, end),
                   ylim = c(HighLowData %>% filter(date > start & date < end) %>% select(low) %>% min(),
                            HighLowData %>% filter(date > start & date < end) %>% select(high) %>% max())) 
  })
  


  
  
  # # Volume 
  # output$distPlot <- renderPlot({
  #   STOCK <- input$selectedstock
  #   symbol <- Stocks %>% filter(Naam == STOCK) %>% select(Symbol)
  #   suffix <- Stocks %>% filter(Naam == STOCK) %>% select(suffix)
  #   end <- today()
  #   start <- end - weeks(input$weeks)
  #   HighLowData <- tq_get(paste0(symbol, ".", suffix), get = "stock.prices", from = " 1990-01-01")
  #   
  #   HighLowData %>% filter(date > start & date < end) %>%
  #     ggplot(aes(x = date, y = close)) +
  #     geom_candlestick(aes(open = open, close = close, high = high, low = low)) + 
  #     geom_segment(aes(x = date, y = 0, xend = date, yend = ((volume / max(volume)) * (HighLowData %>% select(high) %>% max())))) +
  #     labs(title = paste0(STOCK, ": Candlestick"),
  #          subtitle = "Volatility",
  #          x = "", y = "Closing Price")
  # })
  
}