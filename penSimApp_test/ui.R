

library(shiny)
library(rsconnect)
library(ggplot2)
library(tidyverse)
# library(shinydashboard)
#setwd("./penSimApp_test/")



# Loading data
load("./Data/DataPPD.RData")
#PPD_data$ppd_id



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Public Pension Simulation Model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # Selecting Plan (use obsereEvent in server to reflect the change)
      selectInput("planName", "Select Plan",
                  PPD_data$planName),
      
      
      # Expected return and volatility
      numericInput("expReturn_geo", "Expected long-term compound return (%)", 7.5, min = -50, max = 50),
      numericInput("sd",              "standard deviation (%)",                 12,  min = 0, max = 100),
       

      # Button to run model
      
      numericInput("nsim", "Number of simulations",  10,  min = 1, max = 2000),
      actionButton("run", "Run Model")
            
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       #textOutput("text2")
       plotOutput("text2")
    )
  )
)
)
