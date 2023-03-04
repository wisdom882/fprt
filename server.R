##### Server Function 
#setwd("C:/Users/frank/Documents/R_Projects/Shiny_models")
source("copd_func.R")

server <- function(input, output){
  
  output$Model_structure <- renderImage({
    list(src = "Model_structure.png",
         width = "100%",
         height = 330)
  }, deleteFile = F)
  
  # when action button pressed ...
  observeEvent(input$run_simulation,
               ignoreNULL = F, {
                 # Run model function with Shiny inputs
                 psa_df <- copd_func(n.t   = input$n_cycles, 
                                     n.sim = input$n_sim,
                                     d.r   = 0.03, 
                                     trt_cost = input$trt_cost)
                 
                 #-- CREATE COST EFFECTIVENESS TABLE --#
                 # renderTable continuously updates table
                 output$icer_table <- renderTable({
                   df_res_table <- data.frame( # create dataframe
                     Options = c("DCY Therapy","Standard of care"),
                     QALYs = c(mean(psa_df$QALYs_trt),
                               mean(psa_df$QALYs_notrt)),
                     Costs = c(mean(psa_df$Cost_trt),
                               mean(psa_df$Cost_notrt)),
                     Inc.QALYs = c(mean(psa_df$QALYs_trt) - mean(psa_df$QALYs_notrt), NA),
                     Inc.Costs = c(mean(psa_df$Cost_trt) - mean(psa_df$Cost_notrt), NA),
                     ICER = c((mean(psa_df$Cost_trt) - mean(psa_df$Cost_notrt))/(mean(psa_df$QALYs_trt) - mean(psa_df$QALYs_notrt)), NA)
                   ) # close data-frame
                   # round the data-frame to two digits
                   df_res_table[,2:6] = round(
                     df_res_table[,2:6],digits = 2)
                   # print the results table
                   df_res_table
                   
                 }) # table plot end.
                 
                 #-- CREATE COST EFFECTIVENESS PLANE --#
                 # render plot repeatedly updates.
                 output$SO_CE_plane <- renderPlot({
                   psa_df
                   # create cost effectiveness plane plot
                   plot(
                     # x y are incremental QALYs Costs
                     x = psa_df$Incremental_QALYs,
                     y = psa_df$Incremental_Cost,
                     # label axes
                     xlab = "Incremental QALYs",
                     ylab = "Incremental Costs/(CAD$)",
                     col = "blue",
                     # set x-limits and y-limits for plot.
                     xlim = c( min(psa_df$Incremental_QALYs,
                                   psa_df$Incremental_QALYs*-1),
                               max(psa_df$Incremental_QALYs,
                                   psa_df$Incremental_QALYs*-1)),
                     ylim = c( min(psa_df$Incremental_Cost,
                                   psa_df$Incremental_Cost*-1),
                               max(psa_df$Incremental_Cost,
                                   psa_df$Incremental_Cost*-1)),
                     # include y and y axis lines.
                     abline(h = 0,v = 0),
                     
                   ) # CE plot end
                 }) # renderplot end
                 
                 ################ CEAC ################################
                 output$CEAC <- renderPlot({
                   
                   tst_df <- cbind.data.frame(ic_cost = psa_df$Incremental_Cost, ic_qaly = psa_df$Incremental_QALYs, wtp_20k = rep(0, input$n_sim), wtp_40k = rep(0, input$n_sim), wtp_60k = rep(0, input$n_sim), 
                                              wtp_80k = rep(0, input$n_sim), wtp_100k = rep(0, input$n_sim), wtp_120k = rep(0, input$n_sim),
                                              wtp_140k = rep(0, input$n_sim), wtp_160k = rep(0, input$n_sim), wtp_180k = rep(0, input$n_sim), wtp_200k = rep(0, input$n_sim))
                   
                   tst_df2 <- tst_df %>%
                     mutate(wtp_20k = (20000 * ic_qaly) - ic_cost,
                            wtp_40k = (40000 * ic_qaly) - ic_cost,
                            wtp_60k = (60000 * ic_qaly) - ic_cost,
                            wtp_80k = (80000 * ic_qaly) - ic_cost,
                            wtp_100k = (100000 * ic_qaly) - ic_cost,
                            wtp_120k = (120000 * ic_qaly) - ic_cost,
                            wtp_140k = (140000 * ic_qaly) - ic_cost,
                            wtp_160k = (160000 * ic_qaly) - ic_cost,
                            wtp_180k = (180000 * ic_qaly) - ic_cost,
                            wtp_200k = (200000 * ic_qaly) - ic_cost)
                   
                   ## Calculate proportions with NMB >= 0
                   prop_20k <- mean(tst_df2$wtp_20k >= 0)
                   prop_40k <- mean(tst_df2$wtp_40k >= 0)
                   prop_60k <- mean(tst_df2$wtp_60k >= 0)
                   prop_80k <- mean(tst_df2$wtp_80k >= 0)
                   prop_100k <- mean(tst_df2$wtp_100k >= 0)
                   prop_120k <- mean(tst_df2$wtp_120k >= 0)
                   prop_140k <- mean(tst_df2$wtp_140k >= 0)
                   prop_160k <- mean(tst_df2$wtp_160k >= 0)
                   prop_180k <- mean(tst_df2$wtp_180k >= 0)
                   prop_200k <- mean(tst_df2$wtp_200k >= 0)
                   
                   prop_dt <- c(prop_20k, prop_40k, prop_60k, prop_80k, prop_100k,
                                prop_120k, prop_140k, prop_160k, prop_180k, prop_200k)
                   
                   v.wtp= c(20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000, 180000, 200000)
                   
                   wtp_prop_df <- data.frame(v.wtp, prop_dt)
                   
                   
                   # create CEAC plot
                   plot(
                     # x y are incremental QALYs Costs
                     x = wtp_prop_df$v.wtp,
                     y = wtp_prop_df$prop_dt,
                     sub = "QALY: quality-adjusted life-year; ICER: incremental cost-effectiveness ratio",
                     xlab = "Willingness-to-pay per QALY gained/(CAD$)",
                     ylab = "Probability of cost-effectiveness",
                     
                     
                     col = "blue",
                     # set x-limits and y-limits for plot.
                     #xlim = c(0, 200000),
                     #xaxt = 'n',
                     #axis(side = 20000, 20000:200000),
                     #ylim = c(0, 1),
                     #title(sub = "Cost-effectiveness acceptability curve of the ICER. QALY: quality-adjusted life-year; ICER: incremental cost-effectiveness ratio", 
                     #       xlab = "Willingness-to-pay per QALY Gained",
                     #      ylab = "Probability of cost-effectiveness"),
                     type = "b",
                     pch = 19
                     # include y and y axis lines.
                     #abline(h = 0,v = 0)
                   ) # CE plot end
                 }) # renderplot end
               }) # Observe event end
} # Server end
