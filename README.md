# Shiny Stock Picks
A Shiny app based on the [tidyquant package](https://github.com/mdancho84/tidyquant) to visualize and bring financial analysis from Euronext stocks to Shiny.

**Under Development**

# Requirements
To see the app under development stage, there are two options. 

### Run from Github
1. Have RStudio installed and running.
2. Have the following packages installed:
  * shiny
  * tidyquant
  * ggplot2
  * dplyr
  * tidyr
  * repmis

  ```r
  install.packages(c("shiny", "tidyquant","ggplot2" , "dplyr", "tidyr", "repmis)) 
  ``` 
4. Then run the command: 
```r
shiny::runGitHub('ShinyStocks', 'HBossier', subdir = "/R") 
```

### Run locally
1. Have RStudio installed and running 
2. Install the packages
  ```r
  install.packages(c("shiny", "tidyquant","ggplot2" , "dplyr", "tidyr", "repmis")) 
  ``` 
3. Download this repository and enter the directory from within RStudio
4. Run the following in **R**
```r
shiny::runApp()
```
 
# Info
First Modified: 11/03/2017
