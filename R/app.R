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

# Markets
markets <- Stocks %>% select(Market) %>% unique()
allStocks <- Stocks %>% filter(Market == 'Euronext Brussels') %>% select(Naam) 

# allStocks %>% slice(-c(1323,1324))
# Stocks %>% select(Naam) %>% by_row(.) %>% nchar(.)
# max(apply(allStocks, 1, nchar))
# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("wtclassic Stock Picks"),
   
   # # Sidebar with controls to select a dataset and specify the
   # # number of observations to view
   # sidebarLayout(
   #   sidebarPanel(
   #     selectInput("dataset", "Choose a dataset:", 
   #                 choices = c("rock", "pressure", "cars")),
   #     
   #     numericInput("obs", "Number of observations to view:", 10)
   #   ),
   #   
   #   # Show a summary of the dataset and an HTML table with the 
   #   # requested number of observations
   #   mainPanel(
   #     verbatimTextOutput("summary"),
   #     
   #     tableOutput("view")
   #   )
   # ),
   
   # Define the sidebar
   sidebarLayout(
     sidebarPanel(
      # Drop down menu to select the market
     selectInput("markets", label = 'Markets', choices = markets, selected = 'Euronext Brussels'),
     
     # Drop down menu to select the stocks
     selectInput("selectedstock", label = 'Stocks', choices = allStocks, selected = 'ABLYNX')
     ),
    # check http://stackoverflow.com/questions/34929206/r-shiny-selectinput-that-is-dependent-on-another-selectinput
    # for dynamic intput.
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  # 
  # # Return the requested dataset
  # datasetInput <- reactive({
  #   switch(input$dataset,
  #          "rock" = rock,
  #          "pressure" = pressure,
  #          "cars" = cars)
  # })
  # 
  # # Generate a summary of the dataset
  # output$summary <- renderPrint({
  #   dataset <- datasetInput()
  #   summary(dataset)
  # })
  # 
  # # Show the first "n" observations
  # output$view <- renderTable({
  #   head(datasetInput(), n = input$obs)
  # })
  # 
  # Basic candle plot 
  output$candlePlot <- renderPlot({
    
    STOCK <- input$selectedstock
    symbol <- Stocks %>% filter(Naam == STOCK) %>% select(Symbol)
    #print(paste0('You selected' ,STOCK))
    end <- today()
    start <- end - weeks(20)
    HighLowData <- tq_get(paste0(symbol, ".BR"), get = "stock.prices", from = " 1990-01-01")
    HighLowData %>%
      ggplot(aes(x = date, y = close)) +
      geom_candlestick(aes(open = open, close = close, high = high, low = low)) + 
      geom_bbands(aes(high = high, low = low, close = close),
                  ma_fun = SMA, n = 20, sd = 2, size = 1) + 
      labs(title = "Ablynx: Candlestick",
           subtitle = "Volatility",
           x = "", y = "Closing Price") +
      coord_x_date(xlim = c(start, end),
                   ylim = c(HighLowData %>% filter(date > start & date < end) %>% select(low) %>% min(),
                            HighLowData %>% filter(date > start & date < end) %>% select(high) %>% max()))
  })
  
  output$plot1 <- renderPlot({
    ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
      facet_grid(. ~ cyl) +
      theme_bw()
  })
   
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

