

#*********************************************************************
#                            Load data 
#*********************************************************************

get_Aggliab <- function(ppd_id){
  load(paste0("Data/Outputs_liab/liabScn_A1/liab_A1_",ppd_id, ".RData"))
  AggLiab
}

load("Data/DataPPD.RData")
PPD_data %<>% 
  mutate(planName_wID = paste0("PPD #", ppd_id," ", planName))


dir_outputs_liab    <- "Data/Outputs_liab/"
file_Scn_return <- "Model/Scn_return.xlsx"
df_returnScn <- read_excel(file_Scn_return, sheet = "returnScn", "A5:I11" )

model_sim_liabScn <- "A1"
model_sim_returnScn <- "return75"
model_sim_ppd_id <- 0 # ppd_id_all[-c(1:40)]
returnScn_sim <- df_returnScn %>% filter(returnScn == model_sim_returnScn) 


#*********************************************************************
#                            RIG Style
#*********************************************************************
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
      rv$ppd_id   <- PPD_data$ppd_id[which(PPD_data$planName_wID == input$planName)]
      rv$Aggliab  <- get_Aggliab(PPD_data$ppd_id[which(PPD_data$planName_wID == input$planName)])
      rv$planName <- PPD_data$planName[which(PPD_data$planName_wID == input$planName)]
      #print(rv$ppd_id)
      print(rv$Aggliab$planData_list$inputs_singleValues$i)
    })
    
     # output$text1 <- renderText({rv$ppd_id})
    
    # 2. defult values of funding policy inputs that depend on the choice of plan 
    output$fundingPolicyUI <- renderUI({
      tagList(
        
        radioButtons("amort_openclosed", "Amortization method: Open or Closed",
                     c("Open" = "open",
                       "Closed" = "closed"),
                     selected = rv$Aggliab$planData_list$inputs_singleValues$amort_openclosed,
                     inline = T),
        
        radioButtons("amort_pctdol", "Amortization: Constant Dollar or Constant Percent",
                     c("Constant Dollar"   = "cd",
                       "Constant Percent"  = "cp"),
                     selected = rv$Aggliab$planData_list$inputs_singleValues$amort_pctdol,
                     inline = T),
        
        sliderInput("amort_year", "Amortization Period",    rv$Aggliab$planData_list$inputs_singleValues$amort_year, min = 1, max = 40, width = "100%"),
        
        sliderInput("asset_year", "Asset Smoothing Period", rv$Aggliab$planData_list$inputs_singleValues$asset_year, min = 0, max = 20, width = "100%")
      )
    })
    
    
    
    # 2. Run model
    outputs_list <- eventReactive(input$run, {
      #source("./Model/ModelApp_RunControl.R", local = TRUE)
    
      # Applying input values
      returnScn_sim$r.sd        <- input$sd/100
      returnScn_sim$r.geoMean   <- input$expReturn_geo/100  
      returnScn_sim$r.arithMean <- returnScn_sim$r.geoMean + returnScn_sim$r.sd ^2/2
  
      rv$Aggliab$planData_list$inputs_singleValues$amort_pctdol     <- input$amort_pctdol
      rv$Aggliab$planData_list$inputs_singleValues$amort_openclosed <- input$amort_openclosed 
      rv$Aggliab$planData_list$inputs_singleValues$amort_year       <- input$amort_year
      rv$Aggliab$planData_list$inputs_singleValues$asset_year       <- input$asset_year
      
      rv$Aggliab$planData_list$inputs_singleValues$ifFixedERCrate   <- input$ifFixedERCrate
      
      if(input$ifFixedERCrate) rv$Aggliab$planData_list$inputs_singleValues$fixedERCrate <- input$fixedERCrate/100
      
      rv$Aggliab$planData_list$inputs_singleValues$nsim <- input$nsim
      
      
      
      # Generating investment return series
      source("Model/Model_InvReturns.R", local = TRUE, echo = TRUE)
      i.r <- gen_returns(rv$Aggliab$planData_list$inputs_singleValues, returnScn_sim )
      
      # Running simulation model
      source("Model/Model_Sim.R", local = TRUE, echo = TRUE)
      penSim_results <- run_sim(rv$Aggliab,
                                i.r,
                                rv$Aggliab$planData_list$init_amort_unadj,
                                rv$Aggliab$planData_list$init_unrecReturns_unadj,
                                rv$Aggliab$planData_list$inputs_singleValues)
      
      source("Model/Model_Master_sim.R", local = TRUE, echo = TRUE)
      df_riskMeasure
    })

    
    # 3. Creating outputs   
    
    output$text <- renderText(input$planName)
    
    output$plot_FRdist <- renderPlotly({
      
      # Distribution of funded ratio 
      fig.title    <- paste0("Distribution across simulations of funded ratio")
      fig.subtitle <- NULL
      fig_FRdist <- outputs_list()  %>% 
        select(year, FR.q25, FR.q50, FR.q75) %>% 
        gather(type, value, -year) %>% 
        mutate(type = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25") ,
                                   labels = c("75th percentile", "50th percentile", "25th percentile")),
               value = round(value, 2)) %>% 
        ggplot(aes(x = year, y = value,
                   color = type,
                   shape = type)) + 
        theme_bw() + 
        #facet_grid(.~returnScn) + 
        geom_line() + 
        geom_point(size = 2) + 
        geom_hline(yintercept = 100, linetype = 2, size = 1) +
        # coord_cartesian(ylim = c(40,160)) + 
        scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5),2046)) + 
        scale_y_continuous(breaks = seq(0, 500, 20)) + 
        scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red),  name = NULL) + 
        scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL) +
        labs(title = fig.title,
             subtitle = fig.subtitle,
             x = NULL, y = "Market-asset value \nas a percentage of total liability (%)") + 
        theme(axis.text.x = element_text(size = 8),
              legend.position = "bottom") + 
        RIG.theme()
      #fig_FRdist
      
      fig_FRdist <- 
      ggplotly(fig_FRdist, tooltip = c("year", "value")) %>% 
      layout(title = "", 
             legend = list(orientation = 'h', x = 0.1, y = -0.05, xref = "paper", yref = "paper"),
             margin = list(
               t= 90,
               b= 30,
               l= 75,
               r= 10,
               pad= 0)
      ) %>% 
        add_annotations(x = 0.5, y = 1.1, text = fig.title, 
                        xref = "paper", yref = "paper", showarrow= F) %>% 
        config(displayModeBar = "hover")
      
    })
    
      output$plot_ERCdist <- renderPlotly({
      # Distribution of funded ratio 
      fig.title    <- paste0("Distribution across simulations of employer contribution rate")
      fig.subtitle <- NULL
      fig_ERC_PR_dist <- outputs_list() %>% 
        select(year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
        gather(type, value, -year) %>% 
        mutate(type = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25"),
                                   labels = c("75th percentile", "50th percentile", "25th percentile")),
               value = round(value, 2)) %>% 
        ggplot(aes(x = year, y = value,
                   color = type,
                   shape = type)) + 
        theme_bw() + 
        #facet_grid(.~returnScn) + 
        geom_line() + 
        geom_point(size = 1.5) + 
        # coord_cartesian(ylim = c(0,35)) + 
        scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
        scale_y_continuous(breaks = seq(0, 500, 5)) + 
        scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green),  name = NULL) + 
        scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL) +
        labs(title = fig.title,
             subtitle = fig.subtitle,
             x = NULL, y = "Employer contribution \nas a percentage of total payroll (%)") + 
        theme(axis.text.x = element_text(size = 8),
              legend.position = "bottom") + 
        RIG.theme()
      fig_ERC_PR_dist
      
      fig_ERC_PR_dist <- ggplotly(fig_ERC_PR_dist, tooltip = c("year", "value")) %>% 
        layout(title = "", 
               legend = list(orientation = 'h', x = 0.1, y = -0.05, xref = "paper", yref = "paper"),
               margin = list(
                 t= 90,
                 b= 30,
                 l= 75,
                 r= 10,
                 pad= 0)
        ) %>% 
        add_annotations(x = 0.5, y = 1.1, text = fig.title, 
                        xref = "paper", yref = "paper", showarrow= F) %>% 
        config(displayModeBar = "hover")
       # https://jsfiddle.net/ajzeigert/fmha6jmk/1/
    })
    
    
    output$plot_FR40less <- renderPlotly({
    
    # Risk of low funded ratio
    fig.title <- "Probabilities of funded ratio \nbelow 40% in any year up to the given year"
    fig.subtitle <- NULL
    fig_FR40less <- outputs_list() %>%
      select(year, FR40less) %>%
      gather(type, value, -year) %>% 
      mutate(value = round(value, 2)) %>% 
      # mutate(type = factor(type, levels = c("FR75less", "FR60less", "FR40less"), labels = c("75%","60%", "40%" ))) %>% 
      #mutate(FR40less.det = 0) %>% 
      #gather(variable, value, -year) %>% 
      ggplot(aes(x = year, y = value)) + 
      theme_bw() + 
      geom_point(size = 2, color = RIG.blue) + 
      geom_line(color = RIG.blue) + 
      #coord_cartesian(ylim = c(0,25)) + 
      scale_y_continuous(breaks = seq(0,200, 5)) +
      scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
      #scale_color_manual(values = c(RIG.blue, RIG.red, RIG.red),  name = NULL) + 
      #scale_shape_manual(values = c(17,16, 15),  name = NULL) +
      labs(title = fig.title,
           subtitle = fig.subtitle,
           x = NULL, y = "Probability (%)") + 
      guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
      RIG.theme()+
      theme(legend.position="none",
            title =element_text(size=11)) 
    # fig_FR40less
    # fig_FR40less$data %>% filter(year == 2046)
    
    fig_FR40less <- ggplotly(fig_FR40less, tooltip = c("year", "value")) %>% 
    layout(title = "", 
           margin = list(
             t= 90,
             b= 20,
             l= 60,
             r= 10,
             pad= 0)
    ) %>% 
      add_annotations(x = 0.5, y = 1.1, text = fig.title, 
                      xref = "paper", yref = "paper", showarrow= F) %>% 
      config(displayModeBar = "hover")
    
    })

    output$plot_ERChike <- renderPlotly({
    # Risk of sharp increase in ERC/PR
    fig.title <- "Probability of contribution rising by more than 10% \nof payroll in a 5-year period up to the given year"
    fig.subtitle <- NULL
    fig_ERChike <- outputs_list() %>% 
      #mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
      select(year, ERC_hike) %>% 
      gather(type, value, -year) %>% 
      mutate(value = round(value, 2)) %>% 
      #mutate(ERChike.det = 0) %>% 
      #gather(variable, value, - year) %>% 
      ggplot(aes(x = year, y = value)) + theme_bw() + 
      geom_point(size = 2, color = RIG.blue) + 
      geom_line(color = RIG.blue) + 
      #coord_cartesian(ylim = c(0,40)) + 
      #scale_y_continuous(breaks = seq(0,200, 5)) +
      scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
      #scale_color_manual(values = c(RIG.blue, RIG.red, RIG.green, RIG.purple),  name = "") + 
      #scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
      labs(title = fig.title,
           subtitle = fig.subtitle,
           x = NULL, y = "Probability (%)") + 
      guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
      RIG.theme() +
      theme(title =element_text(size=11))
    # fig_ERChike
    # fig_ERChike$data %>% filter(year == 2046)
    
    #fig_2riskMeasures <- grid.arrange(fig_FR40less, fig_ERChike, ncol = 2, widths = c(1, 1))
    #fig_2riskMeasures %>% grid.draw()

    fig_ERChike  <- ggplotly(fig_ERChike, tooltip = c("year", "value")) %>% 
      layout(title = "", 
             margin = list(
               t= 90,
               b= 20,
               l= 40,
               r= 20,
               pad= 0)
      ) %>% 
      add_annotations(x = 0.5, y = 1.1, text = fig.title, 
                      xref = "paper", yref = "paper", showarrow= F) %>% 
      config(displayModeBar = "hover")
    
    
    
    #subplot(fig_FR40less, fig_ERChike)
    
    })
    
    
    
    
    
    
    
})
