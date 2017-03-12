####################
#### TITLE:     Get Stock Symbols for Euronext
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

# Process the Euronext Equities file from raw_data to get the stock symbols.
# This file is manually downloaded from: https://www.euronext.com/nl/equities-directory
# Needs manual update from time to time.



##
##########
### Read Data
##########
##

# Library
library(dplyr)
library(data.table)

# Base location 
raw_data <- '~/Dropbox/StocksApp/raw_data/'

# Last version of data
TimePoints <- list(
  'March2017' = 'Euronext_Equities_EU_2017-03-11'
)

# Read in raw data 
Stocks.raw <- fread(file = paste0(raw_data, TimePoints[['March2017']],'.csv')) %>% 
        slice(-(1:3)) %>% tbl_df

# Select name, symbel and market
Stocks <- Stocks.raw %>% select(Naam, Symbol, Market)

save(Stocks, file = paste0(raw_data, 'Euronext.RDa'))








