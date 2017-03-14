server <- function(input, output) {

  output$stockSelection <- renderUI({
    selectInput("selectedstock", label = 'Stocks', choices = Stocks %>% filter(Market == input$markets) %>% select(Naam), selected = 'ABLYNX')
  })
  
  # Basic candle plot 
  output$candlePlot <- renderPlot({
    
    STOCK <- input$selectedstock
    symbol <- Stocks %>% filter(Naam == STOCK) %>% select(Symbol)
    suffix <- Stocks %>% filter(Naam == STOCK) %>% select(suffix)
    end <- today()
    start <- end - weeks(input$weeks)
    HighLowData <- tq_get(paste0(symbol, ".", suffix), get = "stock.prices", from = " 1990-01-01")
    HighLowData %>%
      ggplot(aes(x = date, y = close)) +
      geom_candlestick(aes(open = open, close = close, high = high, low = low)) + 
      geom_bbands(aes(high = high, low = low, close = close),
                  ma_fun = SMA, n = 20, sd = 2, size = 1) + 
      labs(title = paste0(STOCK, ": Candlestick"),
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
    STOCK <- input$selectedstock
    symbol <- Stocks %>% filter(Naam == STOCK) %>% select(Symbol)
    suffix <- Stocks %>% filter(Naam == STOCK) %>% select(suffix)
    end <- today()
    start <- end - weeks(input$weeks)
    HighLowData <- tq_get(paste0(symbol, ".", suffix), get = "stock.prices", from = " 1990-01-01")
    HighLowData %>% filter(date > start & date < end) %>%
      ggplot(aes(x = date, y = close)) +
      geom_candlestick(aes(open = open, close = close, high = high, low = low)) + 
      geom_segment(aes(x = date, y = 0, xend = date, yend = ((volume / max(volume)) * (HighLowData %>% select(high) %>% max())))) +
      labs(title = paste0(STOCK, ": Candlestick"),
           subtitle = "Volatility",
           x = "", y = "Closing Price")
  })
  
}