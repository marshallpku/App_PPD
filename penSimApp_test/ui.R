#*********************************************************************
#                            Version Info
#*********************************************************************

version <-  "0.1.5"
  
#*********************************************************************
#                            Configure Shinyapp.io
#*********************************************************************

# Shinyapp.io with gmail account marshallpku@gmail.com
library(rsconnect)

# rsconnect::setAccountInfo(name='yimengyin',
#                           token='3585B8F296D58AE9DDC0392D37BCDD78',
#                           secret='')

# http://docs.rstudio.com/shinyapps.io/index.html

#*********************************************************************
#                            References
#*********************************************************************
# Progress Bar
 # https://shiny.rstudio.com/articles/progress.html
 # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r

# Plotly
 # https://plotly-book.cpsievert.me/merging-plotly-objects.html

#*********************************************************************
#                            Load packages
#*********************************************************************

gc()
library(rsconnect)
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)
library(plotly)

library(knitr)
#library(data.table)
library(gdata) # read.xls
#library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
#library(doParallel)
library(doSNOW)
library(readxl)
library(stringr)
library(zoo)

source("Model/Functions.R")


#*********************************************************************
#                            Load data
#*********************************************************************
load("./Data/DataPPD.RData")

PPD_data %<>% 
  mutate(planName_wID = paste0("PPD #", ppd_id," ", planName))

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #C0C0C0;}"))
  ),
  
  # Application title
  titlePanel(tags$div(
                  tags$h1("Public Pension Simulation Model"),
                  tags$h4(paste0("Version ", version)))
             ),
             
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
      
      # Selecting Plan (use obsereEvent in server to reflect the change)
      selectInput("planName", "Select a plan",
                  PPD_data$planName_wID),
      
      hr(),
      
      h4("Distribution of Investment Returns"),
      # Expected return and volatility
      numericInput("expReturn_geo", "Expected long-term compound return (%)", 7.5, min = -50, max = 50, width = "50%"),
      numericInput("sd",            "Standard deviation (%)",                 12,  min = 0, max = 100,  width = "50%"),
      
      hr(),
      
      h4("Funding Policies"), 
      # Funding policy inputs (defined using renderUI in server.)
      uiOutput("fundingPolicyUI"),
      
     
      hr(),
      
      radioButtons("ifFixedERCrate", "Employer contribution is a fixed percentage of payroll (This will override other funding policies)",
                   c("No"  = FALSE,
                     "Yes" = TRUE), inline = TRUE),
      
      conditionalPanel(
        condition = "input.ifFixedERCrate == 'TRUE'",
        numericInput("fixedERCrate", "   Employer contribution as a fixed percentage of payroll (%)", 10,  min = 0, max = 100, width = "50%")
      ),
      
      hr(),
      
      # Button to run model
      numericInput("nsim", "Number of simulations",  500,  min = 1, max = 2000, width = "50%"),
      actionButton("run", "Run Model")
      
            
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 9,
       tabsetPanel(
         tabPanel("Distribution of Outcomes",
                  # plotOutput("plot_FRdist",  width = 600, height = 500),
                  # plotOutput("plot_ERCdist", width = 600, height = 500)
                  # plotOutput("plot_dist",  width = 1200, height = 500)
                  
                  fluidRow(
                    column(6,
                           plotlyOutput("plot_FRdist",  width = 600, height = 650)),
                    column(6,
                           plotlyOutput("plot_ERCdist", width = 600, height = 650))
                  )
                  
                  ),
         
         tabPanel("Measures of Risk",
                  
                  fluidRow(
                    column(6,
                           plotlyOutput("plot_FR40less",   width = 600, height = 600)),
                    column(6,
                           plotlyOutput("plot_ERChike",    width = 600, height = 600))
                    )
                  
                  
                  
                  #plotlyOutput("plot_risk",    width = 1200, height = 500)
         )
       )
      )
    )
   )
  )

