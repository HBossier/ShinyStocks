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


# Define UI for application: 
# - Top bar with market
# - side bar with
# - main panel


ui <- shinyUI(navbarPage("Financial analysis",
          tabPanel("Stocks",
          fluidPage(
            # -----------------------------------------------------------------
            # SIDE PANEL 
            # -----------------------------------------------------------------
            sidebarPanel(width = 4,
              conditionalPanel(condition="input.conditionedPanels==1",
              h3(uiOutput("Maintitle")),
              h6(uiOutput("Subtitle1")),
              h6(uiOutput("Subtitle2")),
              # Number of weeks to plot
              numericInput("weeks", "Number of weeks", 
                value = 52, min = 1, max = 260),
              
              # Drop down menu to select plot type
              selectInput("PlotType", label = 'Type of Plot', 
                choices = c("Line Bar", "Candlestick", "Trapgrafiek", "Oppervlakte", 
                  "OHLC", "Staafdiagram", "Points"), selected = 'Stock Prices'),
  
              # Select the period: day, week or month
              radioButtons("period", label = "Display",
               choices = list("Day" = 1, "Week" = 2, "Month" = 3), 
               selected = 1, inline = TRUE),
              
              # Checkbox for weighted averages: depending on period selected 
              # First if input period = days
              conditionalPanel(condition = "(input.period == 1)", 
                checkboxGroupInput("WA", label = "Weighted average over x days", 
                  choices = list("20" = 1, "50" = 2, "150" = 3, "Custom" = 4),
                  selected = NULL, inline = TRUE),
                conditionalPanel(condition = "(input.WA[0] == 4) | (input.WA[1] == 4) | (input.WA[2] == 4) | (input.WA[3] == 4)",
                  numericInput("manWA", "days:", value = 0, min = 1, max = 1800))),
              # Second if input period = weeks 
              conditionalPanel(condition = "(input.period == 2)",
                checkboxGroupInput("WA2", label = "Weighted average over x weeks", 
                  choices = list("5" = 1,"15" = 2, "30" = 3, "Custom" = 4),
                  selected = NULL, inline = TRUE),
                conditionalPanel(condition = "(input.WA2[0] == 4) | (input.WA2[1] == 4) | (input.WA2[2] == 4) | (input.WA2[3] == 4)",
                 numericInput("manWA", "weeks:", value = 0, min = 1, max = 1800))
                ),
  
              # Tick boxes for scale: linear, procentual or log scale 
              radioButtons("scale", label = "Scale", 
                choices = list("Linear" = 1, "Procentual" = 2, "Logaritmic" = 3),
                selected = 1, inline = TRUE),
  
              # Check box for adding index 
              checkboxInput("Index", label = uiOutput("market")),
  
              # Check box for adding benchmark 
              checkboxInput("Bench", label = 'Add Benchmark'),
                   conditionalPanel(condition = "(input.Bench == 1)",
                      selectInput("markets2", label = 'Add Benchmark', 
                        choices = markets, selected = 'Euronext Brussels'),
              uiOutput("benchSelection")),
                    
              # Plot 10 year
              plotOutput("Plot10")
              # End of first condition in side panel 
              ),
              # Second condition in side panel: type of data 
              conditionalPanel(condition="input.conditionedPanels==2",
                radioButtons("dist", "Select data:",
                  c("Financial data" = "Fin_data",
                  "Expectations" = "Expectations",
                  "Analist expectation" = "Analist",
                  "Results" = "Res"))
                  ) 
                ),
  
            # -----------------------------------------------------------------
            # MAIN PANEL 
            # -----------------------------------------------------------------
            mainPanel(
             fluidRow(
                column(5,
                  selectInput("markets", label = 'Market', choices = markets, selected = 'Euronext Brussels')
                ),
                column(5,
                  uiOutput("stockSelection")
                )
              ),
  
             # Tabs on panel 
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
  )
)

