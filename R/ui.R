####################
#### TITLE:     Shiny App to check key stock information: ui.R part
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


# We first define the available stocks by reading in Euronext stocks
LOCAL <- FALSE
if(isTRUE(LOCAL)){
  # Working directory and load data directly from HD
  wd <- '/Users/hanbossier/Dropbox/StocksApp/'
  load(paste0(wd, 'raw_data/Euronext.RDa'))
}else{
  repmis::source_data("https://github.com/HBossier/ShinyStocks/blob/master/raw_data/Euronext.RDa?raw=true")
}

# Markets
markets <- Stocks %>% select(Market) %>% unique() %>% filter(grepl('Euronext', x = Market, ignore.case = TRUE))
allStocks <- Stocks %>% filter(Market == 'Euronext Brussels') %>% select(Naam) 

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("wtclassic's Stock Picks"),
  
  # Define the sidebar
  sidebarLayout(
    sidebarPanel(
      # Drop down menu to select the market
      selectInput("markets", label = 'Markets', choices = markets, selected = 'Euronext Brussels'),
      
      # Drop down menu to select the stocks: this depends on choice of market, defined in server function below.
      uiOutput("stockSelection")
      #selectInput("selectedstock", label = 'Stocks', choices = allStocks, selected = 'ABLYNX')
    ),
    # Show the candle plot
    mainPanel(
      plotOutput("candlePlot")
    )
  ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

