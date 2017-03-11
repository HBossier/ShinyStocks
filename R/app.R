####################
#### TITLE:     Shiny App to check key stock information
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

# Working directory
wd <- '/Users/hanbossier/Dropbox/StocksApp/'


##
##########
### Stocks
##########
##


# We first define the available stocks by reading in Euronext stocks
load(paste0(wd, 'raw_data/Euronext.RDa'))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
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

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

