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
    
    # First calculate the main plot 
    MainPlot <- HighLowData %>% filter(date > start & date < end) %>%
      ggplot(aes(x = date, y = close)) + {
          if( PlotType == "Line Bar"){
            geom_line(aes(x = date, y = close)) 
          }else{
            geom_candlestick(aes(open = open, close = close, high = high, low = low)) 
          }
        } + {
          # Weigthed moving averages with n the number of days
          if( "1" %in% WA ){
              HighLowData %>% mutate(SMA = WMA(close, n = 20)) %>% filter(date > start & date < end) %>%
              geom_line(data = ., aes(x = date, y = SMA), colour = 'red', linetype = 4, size = 1)
            } 
        } + {
          if( "2" %in% WA){
            HighLowData %>% mutate(SMA = WMA(close, n = 50)) %>% filter(date > start & date < end) %>%
              geom_line(data = ., aes(x = date, y = SMA), colour = 'green', linetype = 4, size = 1)
          }
        } + {
          if( "3" %in% WA ){
            HighLowData %>% mutate(SMA = WMA(close, n = 150)) %>% filter(date > start & date < end) %>%
              geom_line(data = ., aes(x = date, y = SMA), colour = 'blue', linetype = 4, size = 1)
          } 
        } + {
          if("4" %in% WA){
            HighLowData %>% mutate(SMA = WMA(close, n = max(1, ManWA))) %>% filter(date > start & date < end) %>%
              geom_line(data = ., aes(x = date, y = SMA), colour = 'purple', linetype = 4, size = ifelse(ManWA == 0, 0, 1))
          } 
        } +
       labs(title = paste0(STOCK, ": ", PlotType),
           subtitle = "Closing Price",
           x = "", y = "") +
        bdscale::scale_x_bd(business.dates = sort(HighLowData$date, decreasing = FALSE), max.major.breaks = 5) + theme_minimal()
    
    # Volume
    VolumePlot <- HighLowData %>% filter(date > start & date < end) %>%
      ggplot(aes(x = date, y = volume)) + geom_bar(stat = 'identity', width = 1) +
      scale_y_continuous(name = "", labels = scales::scientific, position = 'left') +
      labs(subtitle = "Volume",
           x = "", y = "") +
      bdscale::scale_x_bd(business.dates = sort(HighLowData$date, decreasing = FALSE), max.major.breaks = 5) + theme_minimal()

    # Now combine main plot and volume plot in grid using gtable and grid library
    g1 <- ggplotGrob(MainPlot)
    g2 <- ggplotGrob(VolumePlot)
    g <- rbind(g1, g2, size="first") # stack the two plots
    grid.draw(g)
    
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