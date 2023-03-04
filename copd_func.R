################ Build a Shiny app for your Economic Model #####################
rm(list = ls())
#Import relevant libraries
library(lubridate)
library(dplyr)
library(dbplyr)
library(shiny)
library(shinydashboard)
#install.packages("semantic.dashboard")
#library(semantic.dashboard)
library(DT)
library(ggplot2)
library(plotly)
#library(bslib)
############ Step 1: Make a function that outputs the results of the model #####

copd_func <- function(n.t   = 20,                               # number of cycles
                      n.sim = 10000,                            # number of simulations
                      d.r   = 0.03,                             # discount rate
                      trt_cost = 750                             #treatment cost   
){
  set.seed(1950)
  ### Load Libraries
  
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  #source("C:/Users/frank/Documents/R_Projects/Shiny_models/PSA_functions.R")
  
  
  ##################################### Model input No Treatment Time dependent PSA #########################################
  
  Strategies <- c("No Treatment", "Treatment")  # strategy names 
  v.n   <- c("State1", "State2", "State3", "State4", "Dead")     # state names
  n.s   <- length(v.n)                      # number of states
  v.dw  <- 1 / (1 + d.r) ^ (0:n.t)          # calculate discount weight for each cycle based on discount rate d.r
  
  
  ####### probabilistic analysis method 0.0 ########################################### 
  ####### Generate random values inside a "for" loop ###################################
  p1 <- Sys.time()   # save system time 
  set.seed(1)        # set a seed to be able to reproduce the same results
  TC1 <- TE1 <- TExac <- vector("numeric", length = n.sim) # create a vector with length equal to the number of simulations
  for (i in 1: n.sim) {
    p.S1S2 <- rbeta(n = 1, shape1 = 20,  shape2 = 900) #0.01924  # probability to transition from S1 to S2
    p.S2S3 <- rbeta(n = 1, shape1 = 21,  shape2 = 900) #0.01438  # probability to move from S2 to S3
    p.S3S4 <- rbeta(n = 1, shape1 = 14,  shape2 = 1000) #0.00972  # probability to move from S3 to S4
    
    p.S1S2Trt <- rbeta(n = 1, shape1 = 25,  shape2 = 1200) #0.01237  # probability to transition from S1 to S2 for the treatment group
    p.S2S3Trt <- rbeta(n = 1, shape1 = 12,  shape2 = 960) #0.00923  # probability to move from S2 to S3 for the treatment group
    p.S3S4Trt <- rbeta(n = 1, shape1 = 12,  shape2 = 960)#0.00622  # probability to move from S3 to S4 for the treatment group
    
    p.S1D <-  rbeta(n = 1, shape1 = 21,  shape2 = 900) #0.0123 # probability to die when in S1
    p.S2D <-  rbeta(n = 1, shape1 = 21,  shape2 = 900) #0.0235 # probability to die when in S2
    p.S3D <-  rbeta(n = 1, shape1 = 20,  shape2 = 900) #0.01924 #0.0999 # probability to die when in S3
    p.S4D <-  rbeta(n = 1, shape1 = 210,  shape2 = 980) #0.1992 # probability to die when in S4
    p.DD <-   1      # Probability of remaining dead
    
    
    #v.M_Init  <- rep("S1", n.i)             # initial state for all individuals
    #v.Ts_Init <- rep(0, times = n.i)        # no illness onset at start of model
    
    e.S1 <-   rnorm(1, 0.608, 0.02) #0.608 #Exacerbation rate for S1
    e.S2 <-  rnorm(1, 0.761, 0.02) #0.761 #Exacerbation rate for S2
    e.S3 <-  rnorm(1, 1.769, 0.08) #1.769 #Exacerbation rate for S3
    e.S4 <-  rnorm(1, 1.898, 0.14) #1.898 #Exacerbation rate for S4
    e.D <-   0 #Exacerbation rate for D
    
    
    c.S1  <-  rnorm(1, 1487.81, 200) #1487.81 # cost of remaining in S1 for one cycle 
    c.S2  <-  rnorm(1, 2175.52, 250) #2175.52 # cost of remaining in S2 for one cycle
    c.S3  <-  rnorm(1, 9168.51, 950) #9168.51 # cost of remaining in S3 for one cycle
    c.S4  <-  rnorm(1, 28801.59, 3000) #28801.59 # cost of remaining in S4 for one cycle
    c.D   <-  0 # cost of remaining in D for one cycle
    c.Trt <-  rnorm(1, trt_cost, trt_cost*0.1) #250 #cost of treatment
    c.exac <- rnorm(1, 1793.97, 185) #1793.97 #cost of exercebation
    
    u.S1  <- rnorm(1, 0.82169, 0.08) #0.82169         # utility when in S1 
    u.S2  <- rnorm(1, 0.72645, 0.07) #0.72645         # utility when in S2
    u.S3  <- rnorm(1, 0.66540, 0.06) #0.66540         # utility when in S3
    u.S4  <- rnorm(1, 0.54320, 0.05) #0.54320         # utility when in S4
    u.D  <- 0                # utility when in D
    u.exac <- rnorm(1, -0.06945, 0.006) #-0.06945      # Disutility for exacerbation
    
    
    pob_dying <- c(0.01593,0.01752, 0.0193, 0.02124,0.02329,0.02555, 0.0281,
                   0.03104, 0.03429, 0.03779, 0.04165, 0.04599, 0.05091,
                   0.05631, 0.0621, 0.06846, 0.07555,0.08353, 0.09214, 0.10129)
    
    m.P <- matrix(c(1 - (p.S1S2 + (p.S1D+pob_dying[1])), p.S1S2, 0, 0, p.S1D+pob_dying[1],  # trans. probabilities when in S1
                    0, 1 - (p.S2S3 + (p.S2D+pob_dying[1])), p.S2S3, 0, p.S2D+pob_dying[1],  # trans. probabilities when in S2
                    0, 0, 1 - (p.S3S4 + (p.S3D+pob_dying[1])), p.S3S4, p.S3D+pob_dying[1],  # trans. probabilities when in S3
                    0, 0, 0, 1 - (p.S4D+pob_dying[1]), p.S4D+pob_dying[1],  # trans. probabilities when in S4
                    0, 0, 0, 0, 1),  # trans. probabilities when Dead  
                  byrow = TRUE, nrow = n.s,  ncol = n.s, 
                  dimnames = list(v.n, v.n))
    
    m.TR <- matrix(NA, nrow = n.t + 1, ncol = n.s, 
                   dimnames = list(0:n.t, v.n))   # create Markov trace
    
    m.TR[1, ] <- c(1, 0, 0, 0, 0)                          # initialize Markov trace
    
    ####### PROCESS #########################################
    for (t in 1:n.t) {                            # throughout the number of cycles
      m.TR[t + 1, ] <- m.TR[t, ] %*% m.P          # estimate the Markov trace for cycle t + 1 
    } # close the loop for time 
    
    ####### OUTPUT  ###########################################
    v.c <- c(c.S1 + c.exac, c.S2 + c.exac, c.S3 + c.exac, c.S4 + c.exac, c.D)     # vector of costs  
    v.e <- c(u.S1 + u.exac, u.S2  + u.exac, u.S3  + u.exac, u.S4  + u.exac, u.D)     # vector of utilities
    v.exac <- c(e.S1, e.S2, e.S3, e.S4, e.D)  #Vector of exacerbations
    
    TC1[i] <- t(m.TR %*% v.c) %*% v.dw  # total cost for ith simulation
    TE1[i] <- t(m.TR %*% v.e) %*% v.dw  # total QALYs for ith simulation
    TExac[i] <- t(m.TR %*% v.exac) %*% v.dw  # total QALYs for ith simulation
    
  } # close the loop for the number of simulations 
  
  
  ##################################### Model input Treatment Time dependent PSA #########################################
  TC_trt <- TE_trt <- TExac_trt <- vector("numeric", length = n.sim) # create a vector with length equal to the number of simulations
  
  for (i in 1: n.sim) {
    p.S1S2 <- rbeta(n = 1, shape1 = 20,  shape2 = 900) #0.01924  # probability to transition from S1 to S2
    p.S2S3 <- rbeta(n = 1, shape1 = 21,  shape2 = 900) #0.01438  # probability to move from S2 to S3
    p.S3S4 <- rbeta(n = 1, shape1 = 14,  shape2 = 1000) #0.00972  # probability to move from S3 to S4
    
    p.S1S2Trt <- rbeta(n = 1, shape1 = 25,  shape2 = 1200) #0.01237  # probability to transition from S1 to S2 for the treatment group
    p.S2S3Trt <- rbeta(n = 1, shape1 = 12,  shape2 = 960) #0.00923  # probability to move from S2 to S3 for the treatment group
    p.S3S4Trt <- rbeta(n = 1, shape1 = 12,  shape2 = 960)#0.00622  # probability to move from S3 to S4 for the treatment group
    
    p.S1D <-  rbeta(n = 1, shape1 = 21,  shape2 = 900) #0.0123 # probability to die when in S1
    p.S2D <-  rbeta(n = 1, shape1 = 21,  shape2 = 900) #0.0235 # probability to die when in S2
    p.S3D <-  rbeta(n = 1, shape1 = 20,  shape2 = 900) #0.01924 #0.0999 # probability to die when in S3
    p.S4D <-  rbeta(n = 1, shape1 = 210,  shape2 = 980) #0.1992 # probability to die when in S4
    p.DD <-   1      # Probability of remaining dead
    
    
    #v.M_Init  <- rep("S1", n.i)             # initial state for all individuals
    #v.Ts_Init <- rep(0, times = n.i)        # no illness onset at start of model
    
    e.S1 <-   rnorm(1, 0.608, 0.02) #0.608 #Exacerbation rate for S1
    e.S2 <-  rnorm(1, 0.761, 0.02) #0.761 #Exacerbation rate for S2
    e.S3 <-  rnorm(1, 1.769, 0.08) #1.769 #Exacerbation rate for S3
    e.S4 <-  rnorm(1, 1.898, 0.14) #1.898 #Exacerbation rate for S4
    e.D <-   0 #Exacerbation rate for D
    
    
    c.S1  <-  rnorm(1, 1487.81, 200) #1487.81 # cost of remaining in S1 for one cycle 
    c.S2  <-  rnorm(1, 2175.52, 250) #2175.52 # cost of remaining in S2 for one cycle
    c.S3  <-  rnorm(1, 9168.51, 950) #9168.51 # cost of remaining in S3 for one cycle
    c.S4  <-  rnorm(1, 28801.59, 3000) #28801.59 # cost of remaining in S4 for one cycle
    c.D   <-  0 # cost of remaining in D for one cycle
    c.Trt <-  rnorm(1, trt_cost, trt_cost*0.1) #250 #cost of treatment
    c.exac <- rnorm(1, 1793.97, 185) #1793.97 #cost of exercebation
    
    u.S1trt  <- rnorm(1, 0.8756, 0.02) #0.82169         # utility when in S1 
    u.S2trt  <- rnorm(1, 0.8445, 0.02) #0.72645         # utility when in S2
    u.S3trt  <- rnorm(1, 0.7440, 0.03) #0.66540         # utility when in S3
    u.S4trt  <- rnorm(1, 0.6320, 0.03) #0.54320         # utility when in S4
    u.D  <- 0                # utility when in D
    u.exac <- rnorm(1, -0.06945, 0.006) #-0.06945      # Disutility for exacerbation
    
    
    pob_dying <- c(0.01593,0.01752, 0.0193, 0.02124,0.02329,0.02555, 0.0281,
                   0.03104, 0.03429, 0.03779, 0.04165, 0.04599, 0.05091,
                   0.05631, 0.0621, 0.06846, 0.07555,0.08353, 0.09214, 0.10129)
    
    m.P_trt <- matrix(c(1 - (p.S1S2Trt + (p.S1D+pob_dying[1])), p.S1S2Trt, 0, 0, p.S1D+pob_dying[1],  # trans. probabilities when in S1
                        0, 1 - (p.S2S3Trt + (p.S2D+pob_dying[1])), p.S2S3Trt, 0, p.S2D+pob_dying[1],  # trans. probabilities when in S2
                        0, 0, 1 - (p.S3S4Trt + (p.S3D+pob_dying[1])), p.S3S4Trt, p.S3D+pob_dying[1],  # trans. probabilities when in S3
                        0, 0, 0, 1 - (p.S4D+pob_dying[1]), p.S4D+pob_dying[1],  # trans. probabilities when in S4
                        0, 0, 0, 0, 1),  # trans. probabilities when Dead  
                      byrow = TRUE, nrow = n.s,  ncol = n.s, 
                      dimnames = list(v.n, v.n))
    
    m.TR_trt <- matrix(NA, nrow = n.t + 1, ncol = n.s, 
                       dimnames = list(0:n.t, v.n))   # create Markov trace
    
    m.TR_trt[1, ] <- c(1, 0, 0, 0, 0)                          # initialize Markov trace
    
    ####### PROCESS #########################################
    for (t in 1:n.t) {                            # throughout the number of cycles
      m.TR_trt[t + 1, ] <- m.TR_trt[t, ] %*% m.P_trt          # estimate the Markov trace for cycle t + 1 
    } # close the loop for time 
    
    ####### OUTPUT  ###########################################
    v.c_trt <- c(c.S1 + c.exac + c.Trt, c.S2 + c.exac + c.Trt, c.S3 + c.exac + c.Trt, c.S4 + c.exac + c.Trt, c.D)     # vector of costs  
    v.e_trt <- c(u.S1trt + u.exac, u.S2trt  + u.exac, u.S3trt  + u.exac, u.S4trt  + u.exac, u.D)     # vector of utilities
    v.exac_trt <- c(e.S1, e.S2, e.S3, e.S4, e.D)  #Vector of exacerbations
    
    TC_trt[i] <- t(m.TR_trt %*% v.c_trt) %*% v.dw  # total cost for ith simulation
    TE_trt[i] <- t(m.TR_trt %*% v.e_trt) %*% v.dw  # total QALYs for ith simulation
    TExac_trt[i] <- t(m.TR_trt %*% v.exac_trt) %*% v.dw  # total QALYs for ith simulation
    
  } # close the loop for the number of simulations 
  
  inc_cost <- mean(TC_trt) - mean(TC1)
  inc_QALYs <- mean(TE_trt) - mean(TE1)
  Exa_avoided <- mean(TExac) - mean(TExac_trt)
  
  ICER <- inc_cost/inc_QALYs #$133,742.2
  ICER_Exac_avoided <- inc_cost/Exa_avoided #$19,278.48
  
  cost_df <- data.frame(TC1, TC_trt)
  QALY_df <- data.frame(TE1, TE_trt)
  Exac_df <- data.frame(TExac_trt, TExac)
  
  ic_cost <- TC_trt - TC1
  
  ic_qaly <- TE_trt - TE1
  
  ic_exac <- TExac_trt - TExac
  
  ce_plane_df <- cbind.data.frame(ic_cost, ic_qaly, ic_exac)
  
  display_df <- cbind.data.frame(TC1, TC_trt, TE1, TE_trt, Incremental_cost = TC_trt - TC1, 
                                 Incremental_QALYs = TE_trt - TE1)
  
  colnames(display_df) <- c("Cost_notrt", "Cost_trt", "QALYs_notrt", "QALYs_trt", "Incremental_Cost", "Incremental_QALYs")
  
  return(display_df)
  
  
}

#psa_df <- copd_func(n.t   = 20, 
#n.sim = 10000,
#d.r   = 0.03, 
#trt_cost = 750)
