
library(shiny)
library(tidyverse)

get_Aggliab <- function(ppd_id){
  load(paste0("./Data/Outputs_liab/liabScn_A1/liab_A1_",ppd_id, ".RData"))
  AggLiab
}


RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"
RIG.orange <- "#fc9272"

demo.color6 <- c(RIG.red,
                 RIG.orange,
                 RIG.purple,
                 RIG.green ,
                 RIG.blue,
                 RIG.yellow.dark)


RIG.theme <- function(){
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0, size = 9))
}




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
    # 1. Obtain data of selected plan
    rv <- reactiveValues(ppd_id = 0,
                         Aggliab = list(0))
    
    observeEvent(input$planName, {
      rv$ppd_id  <- PPD_data$ppd_id[which(PPD_data$planName == input$planName)]
      rv$Aggliab <- get_Aggliab(PPD_data$ppd_id[which(PPD_data$planName == input$planName)])
      #print(rv$ppd_id)
      print(rv$Aggliab$planData_list$inputs_singleValues$i)
    })
    
    # output$text1 <- renderText({rv$ppd_id})
    
    # 2. Run model
    outputs_list <- eventReactive(input$run, {
      source("./Model/ModelApp_RunControl.R", local = TRUE)
    
      returnScn_sim$r.sd        <- input$sd/100
      returnScn_sim$r.geoMean   <- input$expReturn_geo/100  
      returnScn_sim$r.arithMean <- returnScn_sim$r.geoMean + returnScn_sim$r.sd ^2/2
  
      rv$Aggliab$planData_list$inputs_singleValues$nsim <- input$nsim
      
      source("./Model/Model_InvReturns.R", local = TRUE, echo = TRUE)
      i.r <- gen_returns(rv$Aggliab$planData_list$inputs_singleValues, returnScn_sim )
      
      source("./Model/Model_sim.R", local = TRUE, echo = TRUE)
      penSim_results <- run_sim(rv$Aggliab,
                                i.r,
                                rv$Aggliab$planData_list$init_amort_unadj,
                                rv$Aggliab$planData_list$init_unrecReturns_unadj,
                                rv$Aggliab$planData_list$inputs_singleValues)
      
      source("./Model/Model_Master_sim.R", local = TRUE, echo = TRUE)
      df_riskMeasure
    })
    
    output$text2 <- renderPlot({
      
      
      
      
      # Distribution of funded ratio 
      fig.title    <- "Distribution across simulations of funded ratio for the aggregate of 170 large public pension plans"
      fig.subtitle <- NULL
      fig_FRdist <- outputs_list()  %>% 
        select(year, FR.q25, FR.q50, FR.q75) %>% 
        gather(type, value, -year) %>% 
        ggplot(aes(x = year, y = value,
                   color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25")),
                   shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25")))) + 
        theme_bw() + 
        #facet_grid(.~returnScn) + 
        geom_line() + 
        geom_point(size = 2) + 
        geom_hline(yintercept = 100, linetype = 2, size = 1) +
        coord_cartesian(ylim = c(40,160)) + 
        scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5),2046)) + 
        scale_y_continuous(breaks = seq(0, 500, 20)) + 
        scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red),  name = NULL, 
                           label  = c("75th percentile", "50th percentile", "25th percentile")) + 
        scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL, 
                           label  = c("75th percentile", "50th percentile", "25th percentile")) +
        labs(title = fig.title,
             subtitle = fig.subtitle,
             x = NULL, y = "Total market-asset value \nas a percentage of total liability (%)") + 
        theme(axis.text.x = element_text(size = 8)) + 
        RIG.theme()
      fig_FRdist
      })
      
})
