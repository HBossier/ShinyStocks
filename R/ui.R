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

# See global.R

##
##########
### UI
##########
##


# Define UI for application
ui <- shinyUI(navbarPage("Financial analysis",
          tabPanel("Stocks",
          fluidPage(
            fluidRow(
              column(3),
                column(3,
                  selectInput("markets", label = 'Market', choices = markets, selected = 'Euronext Brussels')
                ),
                column(4,
                uiOutput("stockSelection")
              )
              ),
              
            sidebarPanel( width = 3,
              conditionalPanel(condition="input.conditionedPanels==1",
              h3(uiOutput("Maintitle")),
              h6(uiOutput("Subtitle1")),
              h6(uiOutput("Subtitle2")),
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
                    ),
                    
                    # Plot 10 year
                    plotOutput("Plot10")
                    ),
                    conditionalPanel(condition="input.conditionedPanels==2",
                      radioButtons("dist", "Select data:",
                        c("Financial data" = "Fin_data",
                        "Expectations" = "Expectations",
                        "Analist expectation" = "Analist",
                        "Results" = "Res"))
                        ) 
                      ),
mainPanel(
  tabsetPanel(
    tabPanel("Technical analysis", value=1, 
    plotOutput("Mainplot")), 
    tabPanel("Fundamental analysis", value=2),
    tabPanel("Agenda", value=3),
    tabPanel("Advices", value=4),
    tabPanel("Financials", value=5)
      , id = "conditionedPanels"
      )
    )
  )
),                         
tabPanel("Bonds"),
tabPanel("ETFS"),
tabPanel("Funds"),
tabPanel("Commodities"),
tabPanel("Futures"),
tabPanel("Disclaimer")
))

