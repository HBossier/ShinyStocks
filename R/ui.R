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

# Header of ui and server need to include both loading of Stocks!

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
### UI
##########
##


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
      uiOutput("stockSelection"),
      
      # Number of weeks to plot
      numericInput("weeks", "Number of weeks", value = 52, min = 1, max = 260),
      
      # Drop down menu to select plot type
      selectInput("PlotType", label = 'Type of Plot', choices = c("Line Bar", "Candlestick"), selected = 'Stock Prices'),

      # Checkbox for weighted averages
      checkboxGroupInput("WA", label = "Weighted average over x days",
                                choices = list("20" = 1,
                                               "50" = 2, "150" = 3, "Custom" = 4),
                                selected = NULL, inline = TRUE),
      conditionalPanel(condition = "(input.WA[0] == 4) | (input.WA[1] == 4) | (input.WA[2] == 4) | (input.WA[3] == 4)",
        numericInput("manWA", "days:", value = 0, min = 1, max = 1800)
      )
    ),
    # Show the candle plot
    mainPanel(
      plotOutput("candlePlot"),
      plotOutput("Plot10")
    )
  )
  


  # # Sidebar with a slider input for number of bins 
  # sidebarLayout(
  #   sidebarPanel(
  #     sliderInput("bins",
  #                 "Number of weeks:",
  #                 min = 1,
  #                 max = 260,
  #                 value = 20)
  #   ),
  #   
  #   # Show a plot of the generated distribution
  #   mainPanel(
  #     plotOutput("distPlot")
  #   )
  # )
)

