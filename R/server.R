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

# See global.R

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
  
  # Current selected stock
  currentStock <- reactive({
    if(is.null(input$selectedstock)){
      STOCK <- 'ABLYNX'
    }else{
      STOCK <- input$selectedstock
    }
    return(STOCK)
  })
  
  # Data, based on current stock
  currentStockData <- reactive({
    STOCK <- currentStock()
    symbol <- Stocks %>% filter(Naam == STOCK) %>% select(Symbol)
    suffix <- Stocks %>% filter(Naam == STOCK) %>% select(suffix)
    HighLowData <- tq_get(paste0(symbol, ".", suffix), get = "stock.prices", from = " 1990-01-01")
    return(HighLowData)
  })
  
  # Main plot
  output$Mainplot <- renderPlot({
    # Get the current selected stock
    STOCK <- currentStock()
    # Get the stock data
    HighLowData <- currentStockData()
    # Get weighted average
    if(is.null(input$WA)){
     WA <- 0
     ManWA <- 0
    }else{
      WA  <-  input$WA
      ManWA <- input$manWA
    }
    # Other parameters
    end <- today()
    start <- end - weeks(input$weeks)
    PlotType  <-  input$PlotType
    
    # Set option for scientific notation
    options(digits = 2)
    options(scipen=-2)
    
    # Create the data frames for grid_extra
    FacettingData <- HighLowData %>% filter(date > start & date < end) %>% 
          rename(price = close) %>% select(date, price, volume) %>% 
          gather("statistic","value",2:3)
    StockDataMain <- HighLowData %>% filter(date > start & date < end) %>%
          select(-volume, - adjusted) %>% mutate(statistic = "price")
    StockDataVolume <- HighLowData %>% filter(date > start & date < end) %>%
          select(date, volume) %>% mutate(statistic = "volume")
    
    # Now the facetted plot
    ggplot(FacettingData, aes(x = date, y = value)) + 
      facet_grid(statistic ~. , scales="free_y") + {
          if( PlotType == "Line Bar"){
            geom_line(data = StockDataMain, aes(x = date, y = close)) 
          }else{
            geom_candlestick(data = StockDataMain, aes(open = open, close = close, high = high, low = low)) 
          }
        } + {
          # Weigthed moving averages with n the number of days
          if( "1" %in% WA ) geom_ma(ma_fun = WMA, n = 20, color = "red", linetype = 4, size = 1)
        } + {
          if( "2" %in% WA)  geom_ma(ma_fun = WMA, n = 50, color = "green", linetype = 4, size = 1)
        } + {
          if( "3" %in% WA ) geom_ma(ma_fun = WMA, n = 150, color = "blue", linetype = 4, size = 1)
        } + {
          if("4" %in% WA) geom_ma(ma_fun = WMA, n = max(1, ManWA), color = "purple", linetype = 4, size = ifelse(ManWA == 0, 0, 1))
        } +
      geom_bar(data = StockDataVolume, aes(x = date, y = volume), stat = 'identity', width = 1) +
      labs(title = paste0(STOCK, ": ", PlotType),
           subtitle = "Volatility",
           x = "", y = "") +
      bdscale::scale_x_bd(business.dates = sort(HighLowData$date, decreasing = FALSE), max.major.breaks = 5) + theme_minimal()
    
    # Now reset options for digits: THIS DOES NOT WORK
    #options(digits = 7)
    #options(scipen = 0)
    
    # # First plot 
    # plot1 <- HighLowData %>% filter(date > start & date < end) %>%
    #     ggplot(aes(x = date, y = close)) + {
    #       if( PlotType == "Line Bar") geom_line() else geom_candlestick(aes(open = open, close = close, high = high, low = low)) 
    #     } + {
    #       # Weigthed moving averages with n the number of days
    #       if( "1" %in% WA ) geom_ma(ma_fun = WMA, n = 20, color = "red", linetype = 4, size = 1)
    #     } + {
    #       if( "2" %in% WA)  geom_ma(ma_fun = WMA, n = 50, color = "green", linetype = 4, size = 1)
    #     } + {
    #       if( "3" %in% WA ) geom_ma(ma_fun = WMA, n = 150, color = "blue", linetype = 4, size = 1)
    #     } + {
    #       if("4" %in% WA) geom_ma(ma_fun = WMA, n = max(1, ManWA), color = "purple", linetype = 4, size = ifelse(ManWA == 0, 0, 1))
    #     } +
    #   labs(title = paste0(STOCK, ": ", PlotType),
    #        subtitle = "Volatility",
    #        x = "", y = "Closing Price") +
    #   bdscale::scale_x_bd(business.dates = sort(HighLowData$date, decreasing = FALSE), max.major.breaks = 5) + theme_minimal() +
    #   coord_x_date(xlim = NULL,
    #                ylim = c(HighLowData %>% filter(date > start & date < end) %>% select(low) %>% min(),
    #                         HighLowData %>% filter(date > start & date < end) %>% select(high) %>% max())) 
    # 
    # # Second plot
    # plot2 <- HighLowData %>% filter(date > start & date < end) %>%
    #   ggplot(aes(x = date, y = volume)) + geom_bar(stat = 'identity') +
    #   bdscale::scale_x_bd(business.dates = sort(HighLowData$date, decreasing = FALSE), max.major.breaks = 5) + theme_minimal()
    # 
    # # Plot them together
    # gridExtra::grid.arrange(plot1, plot2, ncol = 1, heights = c(3,1))
  })
  
  # Plot with highest and lowest value over 10 years
  output$Plot10 <- renderPlot({
    # Get the currentStockData
    HighLowData <- currentStockData()
    # Go 10 years back
    x_year <- year(today()) - 10:1
    # Filter, group by year, summarize through min and max and transfrom
    y10 <- HighLowData %>% filter(year(date) %in% x_year) %>% 
              group_by(year = year(date)) %>% summarise(low10y = min(low), high10y = max(high)) %>%
              gather("LowHigh", "value", 2:3)
    # Draw lines
    ggplot(y10, aes(x = year, y = value)) +  
      geom_line(aes(group = year)) + scale_y_continuous(name = "Max/min value")
  })
  
  

}