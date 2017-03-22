####################
#### TITLE:     Shiny App to check key stock information: global.R part
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
