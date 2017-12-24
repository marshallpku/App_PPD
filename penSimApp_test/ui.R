


library(rsconnect)
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)
library(plotly)

# Loading data
load("./Data/DataPPD.RData")





# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #C0C0C0;}"))
  ),
  
  # Application title
  titlePanel(tags$h1("Public Pension Simulation Model")),
             
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # Selecting Plan (use obsereEvent in server to reflect the change)
      selectInput("planName", "Select Plan",
                  PPD_data$planName),
      
      hr(),
      
      h4("Distribution of Investment Returns"),
      # Expected return and volatility
      numericInput("expReturn_geo", "Expected long-term compound return (%)", 7.5, min = -50, max = 50),
      numericInput("sd",              "standard deviation (%)",                 12,  min = 0, max = 100),
      
      hr(),
      
      h4("Funding Policies Policies"), 
      # Funding policy inputs (defined using renderUI in server.)
      uiOutput("fundingPolicyUI"),
      
      hr(),
       
      # Button to run model
      numericInput("nsim", "Number of simulations",  100,  min = 1, max = 2000),
      actionButton("run", "Run Model")
            
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(

       tabsetPanel(
         tabPanel("Distribution of outcome", 
                  # plotOutput("plot_FRdist",  width = 600, height = 500),
                  # plotOutput("plot_ERCdist", width = 600, height = 500)
                  plotOutput("plot_dist",  width = 1200, height = 500)
                  ),
         
         tabPanel("Measures of Risk", 
                  plotlyOutput("plot_risk",    width = 1200, height = 500)
         )
         
    )
  )
)



)
)

