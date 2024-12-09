################################################################################
# ROCKFISH HARVEST AND RELEASE ESTIMATION WITH REIMER / BAYESIAN METHODS
#
# This code provides a Bayesian version of the Howard et al. (2020) methods
# (see BRF_Howard.R, YE_Howard.R, etc.). The code also provides diagnostics
# and posterior examination of the data and saves it to a format for dissemination
# to ADF&G biologists and managers
#
# Developed by Adam Reimer in 2022
# 
# Current Author: Phil Joy (philip.joy@alaska.gov)
#
# Lats updated: November 2024
#
################################################################################
library(readxl)
library(janitor)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(wesanderson)

#functions, including data and parameter specifications
source(".\\scripts//bayes_data_param_load.R")

#year to run model through
start_yr <- 1977
end_yr <- 2023

#most recent Howard estimates: 
REP_YR <- 2023 #for bringing in Howard estimats

#load the data:
list2env(readinData(spl_knts = 7,
                    start_yr = start_yr,
                    end_yr = end_yr),
         .GlobalEnv)

#load parameters
params <- jags_params()

#area codes
area_codes <- comp %>% select(area,area_n) %>% unique() %>%
  add_row(area = "BSAI", area_n = 5) %>%
  add_row(area = "SOKO2SAP", area_n = 6) %>%
  add_row(area = "WKMA", area_n = 7) %>%
  mutate(area_n = as.character(area_n)) %>% arrange(as.numeric(area_n))

#-------------------------------------------------------------------------------
# Run models!

#iterations, burnin, chains and trimming rate:
ni <- 16E5; nb <- ni*.75; nc <- 3; nt <- (ni - nb) / 1000

#model to run; see /models folder
mod <- "HR_censLBR"

#-------------------------------------------------------------------------------
#Are we using starting values from a prior model?
use_inits = "no"

use_this_model <- "HR_censLBR_thru2023_1600000_7kn_2024-12-07"

initspost <- readRDS(paste0(".\\output\\bayes_posts\\",use_this_model,".rds"))

#If you're using the last run:
last_inits <- lapply(1:nc, function(chain) {
  chain_data <- as.matrix(postH$samples[[chain]])
  as.list(chain_data[nrow(chain_data), ])
})

other_inits <- lapply(1:nc, function(chain) {
  chain_data <- as.matrix(initspost$samples[[chain]])
  as.list(chain_data[nrow(chain_data), ])
})

for (i in 1:3) { #i <- 1
  last_inits[[i]]$'beta0_pH[1]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[2]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[3]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[4]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[5]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[6]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[7]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[8]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[9]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[10]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[11]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[12]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[13]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[14]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[15]' <- runif(1,-0.4,0)
  last_inits[[i]]$'beta0_pH[16]' <- runif(1,-0.4,0)
  
  last_inits[[i]]$'beta1_pH[1]' <- 0
  last_inits[[i]]$'beta1_pH[2]' <- 0
  last_inits[[i]]$'beta1_pH[3]' <- 0
  last_inits[[i]]$'beta1_pH[4]' <- 0
  last_inits[[i]]$'beta1_pH[5]' <- 0
  last_inits[[i]]$'beta1_pH[6]' <- 0
  last_inits[[i]]$'beta1_pH[7]' <- 0
  last_inits[[i]]$'beta1_pH[8]' <- 0
  last_inits[[i]]$'beta1_pH[9]' <- 0
  last_inits[[i]]$'beta1_pH[10]' <- 0
  last_inits[[i]]$'beta1_pH[11]' <- 0
  last_inits[[i]]$'beta1_pH[12]' <- 0
  last_inits[[i]]$'beta1_pH[13]' <- 0
  last_inits[[i]]$'beta1_pH[14]' <- 0
  last_inits[[i]]$'beta1_pH[15]' <- 0
  last_inits[[i]]$'beta1_pH[16]' <- 0
  
  last_inits[[i]]$'beta2_pH[1]' <- 0
  last_inits[[i]]$'beta2_pH[2]' <- 0
  last_inits[[i]]$'beta2_pH[3]' <- 0
  last_inits[[i]]$'beta2_pH[4]' <- 0
  last_inits[[i]]$'beta2_pH[5]' <- 0
  last_inits[[i]]$'beta2_pH[6]' <- 0
  last_inits[[i]]$'beta2_pH[7]' <- 0
  last_inits[[i]]$'beta2_pH[8]' <- 0
  last_inits[[i]]$'beta2_pH[9]' <- 0
  last_inits[[i]]$'beta2_pH[10]' <- 0
  last_inits[[i]]$'beta2_pH[11]' <- 0
  last_inits[[i]]$'beta2_pH[12]' <- 0
  last_inits[[i]]$'beta2_pH[13]' <- 0
  last_inits[[i]]$'beta2_pH[14]' <- 0
  last_inits[[i]]$'beta2_pH[15]' <- 0
  last_inits[[i]]$'beta2_pH[16]' <- 0
  
  last_inits[[i]]$'beta3_pH[1]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[2]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[3]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[4]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[5]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[6]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[7]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[8]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[9]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[10]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[11]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[12]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[13]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[14]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[15]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pH[16]' <- runif(1,28,38)
  
  last_inits[[i]]$'beta0_yellow[1]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[2]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[3]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[4]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[5]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[6]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[7]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[8]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[9]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[10]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[11]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[12]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[13]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[14]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[15]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow[16]' <- runif(1,-0.6,-0.4)
  
  last_inits[[i]]$'beta1_yellow[1]' <- 0
  last_inits[[i]]$'beta1_yellow[2]' <- 0
  last_inits[[i]]$'beta1_yellow[3]' <-0
  last_inits[[i]]$'beta1_yellow[4]' <- 0
  last_inits[[i]]$'beta1_yellow[5]' <- 0
  last_inits[[i]]$'beta1_yellow[6]' <- 0
  last_inits[[i]]$'beta1_yellow[7]' <- 0
  last_inits[[i]]$'beta1_yellow[8]' <- 0
  last_inits[[i]]$'beta1_yellow[9]' <-0
  last_inits[[i]]$'beta1_yellow[10]' <- 0
  last_inits[[i]]$'beta1_yellow[11]' <- 0
  last_inits[[i]]$'beta1_yellow[12]' <-0
  last_inits[[i]]$'beta1_yellow[13]' <- 0
  last_inits[[i]]$'beta1_yellow[14]' <- 0
  last_inits[[i]]$'beta1_yellow[15]' <- 0
  last_inits[[i]]$'beta1_yellow[16]' <- 0
  
  last_inits[[i]]$'beta2_yellow[1]' <- 0
  last_inits[[i]]$'beta2_yellow[2]' <- 0
  last_inits[[i]]$'beta2_yellow[3]' <- 0
  last_inits[[i]]$'beta2_yellow[4]' <- 0
  last_inits[[i]]$'beta2_yellow[5]' <- 0
  last_inits[[i]]$'beta2_yellow[6]' <- 0
  last_inits[[i]]$'beta2_yellow[7]' <- 0
  last_inits[[i]]$'beta2_yellow[8]' <- 0
  last_inits[[i]]$'beta2_yellow[9]' <- 0
  last_inits[[i]]$'beta2_yellow[10]' <- 0
  last_inits[[i]]$'beta2_yellow[11]' <- 0
  last_inits[[i]]$'beta2_yellow[12]' <- 0
  last_inits[[i]]$'beta2_yellow[13]' <- 0
  last_inits[[i]]$'beta2_yellow[14]' <- 0
  last_inits[[i]]$'beta2_yellow[15]' <-0
  last_inits[[i]]$'beta2_yellow[16]' <- 0
  
  last_inits[[i]]$'beta3_yellow[1]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[2]' <-runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[3]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[4]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[5]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[6]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[7]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[8]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[9]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[10]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[11]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[12]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[13]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[14]' <-runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[15]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow[16]' <- runif(1,28,38)
  
  last_inits[[i]]$'beta0_yellow_x[1]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[2]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[3]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[4]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[5]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[6]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[7]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[8]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[9]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[10]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[11]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[12]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[13]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[14]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[15]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_yellow_x[16]' <- runif(1,-0.6,-0.4)
  
  last_inits[[i]]$'beta1_yellow_x[1]' <- 0
  last_inits[[i]]$'beta1_yellow_x[2]' <- 0
  last_inits[[i]]$'beta1_yellow_x[3]' <- 0
  last_inits[[i]]$'beta1_yellow_x[4]' <- 0
  last_inits[[i]]$'beta1_yellow_x[5]' <- 0
  last_inits[[i]]$'beta1_yellow_x[6]' <- 0
  last_inits[[i]]$'beta1_yellow_x[7]' <- 0
  last_inits[[i]]$'beta1_yellow_x[8]' <- 0
  last_inits[[i]]$'beta1_yellow_x[9]' <- 0
  last_inits[[i]]$'beta1_yellow_x[10]' <- 0
  last_inits[[i]]$'beta1_yellow_x[11]' <- 0
  last_inits[[i]]$'beta1_yellow_x[12]' <- 0
  last_inits[[i]]$'beta1_yellow_x[13]' <- 0
  last_inits[[i]]$'beta1_yellow_x[14]' <- 0
  last_inits[[i]]$'beta1_yellow_x[15]' <- 0
  last_inits[[i]]$'beta1_yellow_x[16]' <- 0
  
  last_inits[[i]]$'beta2_yellow_x[1]' <- 0
  last_inits[[i]]$'beta2_yellow_x[2]' <- 0
  last_inits[[i]]$'beta2_yellow_x[3]' <- 0
  last_inits[[i]]$'beta2_yellow_x[4]' <- 0
  last_inits[[i]]$'beta2_yellow_x[5]' <- 0
  last_inits[[i]]$'beta2_yellow_x[6]' <- 0
  last_inits[[i]]$'beta2_yellow_x[7]' <- 0
  last_inits[[i]]$'beta2_yellow_x[8]' <- 0
  last_inits[[i]]$'beta2_yellow_x[9]' <- 0
  last_inits[[i]]$'beta2_yellow_x[10]' <- 0
  last_inits[[i]]$'beta2_yellow_x[11]' <- 0
  last_inits[[i]]$'beta2_yellow_x[12]' <- 0
  last_inits[[i]]$'beta2_yellow_x[13]' <- 0
  last_inits[[i]]$'beta2_yellow_x[14]' <- 0
  last_inits[[i]]$'beta2_yellow_x[15]' <- 0
  last_inits[[i]]$'beta2_yellow_x[16]' <- 0
  
  last_inits[[i]]$'beta3_yellow_x[1]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[2]' <-runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[3]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[4]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[5]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[6]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[7]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[8]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[9]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[10]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[11]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[12]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[13]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[14]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[15]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_yellow_x[16]' <- runif(1,28,38)
  
  last_inits[[i]]$'beta0_pelagic[1]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[2]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[3]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[4]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[5]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[6]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[7]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[8]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[9]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[10]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[11]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[12]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[13]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[14]' <-runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[15]' <- runif(1,-0.6,-0.4)
  last_inits[[i]]$'beta0_pelagic[16]' <- runif(1,-0.6,-0.4)
  
  last_inits[[i]]$'beta1_pelagic[1]' <- 0
  last_inits[[i]]$'beta1_pelagic[2]' <- 0
  last_inits[[i]]$'beta1_pelagic[3]' <- 0
  last_inits[[i]]$'beta1_pelagic[4]' <- 0
  last_inits[[i]]$'beta1_pelagic[5]' <- 0
  last_inits[[i]]$'beta1_pelagic[6]' <- 0
  last_inits[[i]]$'beta1_pelagic[7]' <- 0
  last_inits[[i]]$'beta1_pelagic[8]' <- 0
  last_inits[[i]]$'beta1_pelagic[9]' <- 0
  last_inits[[i]]$'beta1_pelagic[10]' <- 0
  last_inits[[i]]$'beta1_pelagic[11]' <- 0
  last_inits[[i]]$'beta1_pelagic[12]' <- 0
  last_inits[[i]]$'beta1_pelagic[13]' <- 0
  last_inits[[i]]$'beta1_pelagic[14]' <- 0
  last_inits[[i]]$'beta1_pelagic[15]' <- 0
  last_inits[[i]]$'beta1_pelagic[16]' <- 0
  
  last_inits[[i]]$'beta2_pelagic[1]' <- 0
  last_inits[[i]]$'beta2_pelagic[2]' <- 0
  last_inits[[i]]$'beta2_pelagic[3]' <- 0
  last_inits[[i]]$'beta2_pelagic[4]' <- 0
  last_inits[[i]]$'beta2_pelagic[5]' <- 0
  last_inits[[i]]$'beta2_pelagic[6]' <- 0
  last_inits[[i]]$'beta2_pelagic[7]' <- 0
  last_inits[[i]]$'beta2_pelagic[8]' <- 0
  last_inits[[i]]$'beta2_pelagic[9]' <- 0
  last_inits[[i]]$'beta2_pelagic[10]' <- 0
  last_inits[[i]]$'beta2_pelagic[11]' <- 0
  last_inits[[i]]$'beta2_pelagic[12]' <- 0
  last_inits[[i]]$'beta2_pelagic[13]' <- 0
  last_inits[[i]]$'beta2_pelagic[14]' <- 0
  last_inits[[i]]$'beta2_pelagic[15]' <- 0
  last_inits[[i]]$'beta2_pelagic[16]' <- 0
  
  last_inits[[i]]$'beta3_pelagic[1]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[2]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[3]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[4]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[5]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[6]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[7]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[8]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[9]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[10]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[11]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[12]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[13]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[14]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[15]' <- runif(1,28,38)
  last_inits[[i]]$'beta3_pelagic[16]' <- runif(1,28,38)
}

# Name the initial values you want to use:
inits_to_use <- last_inits

#-------------------------------------------------------------------------------
#Run the model
if (use_inits == "no") {
  tstart <- Sys.time()
  postH <- 
    jagsUI::jags(
      parameters.to.save = params,
      model.file = paste0(".\\models\\",mod,".txt"),
      data = jags_dat, 
      parallel = TRUE, verbose = TRUE,
      #inits = list(list(muHhat_ay = log(jags_dat$H_ayg * 1.2)), list(muHhat_ay = log(jags_dat$H_ayg * 1.2)), list(muHhat_ay = log(jags_dat$H_ayg * 1.2))),
      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
      store.data = TRUE)
  runtime <- Sys.time() - tstart; runtime
} 
if (use_inits == "yes") {
  tstart <- Sys.time()
  postH <- jagsUI::jags(
    parameters.to.save = params,
    model.file = paste0(".\\models\\",mod,".txt"),
    data = jags_dat, 
    inits = inits_to_use,
    parallel = TRUE, 
    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,  # no burn-in for the second run
    store.data = TRUE, verbose = TRUE
  )
  runtime <- Sys.time() - tstart; runtime
}

#-------------------------------------------------------------------------------
# Save these results?
other_label <- paste0(jags_dat$C,"kn")

saveRDS(postH, paste0(".\\output\\bayes_posts\\",mod,"_thru",end_yr,"_",ni,"_",other_label,"_",Sys.Date(),".rds"))
saveRDS(postH, paste0("H:\\Documents\\Rockfish_SF_mortality\\RockfishSportMort\\output\\bayes_posts\\",mod,"_thru",end_yr,"_",ni,"_",Sys.Date(),"_v2.rds"))
#-------------------------------------------------------------------------------
# Or are we just re-examinng a past run? See /output/bayes_posts/ folder
results <- "HR_censLBR_thru2023_1600000_7kn_2024-12-07"

#model_HCR_censLBR_xspline_thru2019_6e+06_2024-11-24; 98% converged
#model_HCR_censLBR_1bc_xspline_thru2019_6e+06_2024-11-24; 99% converged
#model_HCR_yeLBR_xspline_thru2019_6e+06_2024-11-24; ~98.5% converged
#model_HCR_allLBR_xspline_thru2019_6e+06_2024-11-24; yuck <96% converged.

postH <- readRDS(paste0(".\\output\\bayes_posts\\",results,".rds"))

#-------------------------------------------------------------------------------
# Examine results!

rhat <- get_Rhat(postH, cutoff = 1.11)
names(rhat)[1] <- "Rhat_values"

all_rhat <- get_Rhat(postH,cutoff = 0.01)
names(all_rhat)[1] <- "Rhat_values"
as.vector(all_rhat$Rhat_values) %>% data.frame()-> rhat_vals
prop_conv <- round(nrow(rhat_vals %>% filter(Rhat <= 1.115))/nrow(rhat_vals),2); prop_conv

rhat

head(rhat$Rhat_values %>% arrange(-Rhat))
#jagsUI::traceplot(postH, Rhat_min = 1.1)

rhat_exam <- rhat$Rhat_values %>%
  rownames_to_column(var = "Parameter") %>%
  separate(Parameter, into = c("variable", "index"), sep = "\\[", extra = "merge") %>%
  mutate(index = str_replace(index, "\\]", "")) %>%
  separate(index, into = c("area_n", "year", "user"), sep = ",", fill = "right") %>%
  mutate(across(starts_with("index"), as.numeric)) %>%
  left_join(comp %>% select(area,area_n) %>% unique() %>%
              add_row(area = "BSAI", area_n = 5) %>%
              add_row(area = "SOKO2SAP", area_n = 6) %>%
              add_row(area = "WKMA", area_n = 7) %>%
              mutate(area_n = as.character(area_n)),
            by = "area_n")

rhat_exam %>% group_by(variable) %>%
  filter(variable %in% c("logbc_H", "mu_bc_H", "sd_bc_H",
                         #"logbc_C", "mu_bc_C", "sd_bc_C",
                         "logbc_R","bc_R_offset","mu_bc_R_offset","sd_bcRoff",
                         #User proportions (proportion guided); different for H and R
                         "pG", "b1_pG", "b2_pG",
                         #"pG_R", "b1_pG_R", "b2_pG_R",
                         #proportion harvested: 
                         "pH", #"pHu",
                         "mu_beta0_pH","tau_beta0_pH","beta0_pH","beta1_pH","beta2_pH","beta3_pH","beta4_pH",
                         #random effects on pH
                         "re_pH", #"re_pHu",
                         "sd_pH",
                         #"re_pH","sd_pH",
                         #proportions same for catch and harvest? thinking on it?
                         "p_pelagic", "beta0_pelagic", "beta1_pelagic", "beta2_pelagic", "beta3_pelagic", "beta4_pelagic", 
                         "mu_beta0_pelagic", "tau_beta0_pelagic",
                         "p_yellow", "beta0_yellow", "beta1_yellow", "beta2_yellow", "beta3_yellow", "beta4_yellow",
                         "mu_beta0_yellow", "tau_beta0_yellow",
                         "p_yellow_x", "beta0_yellow_x", "beta1_yellow_x", "beta2_yellow_x", "beta3_yellow_x", "beta4_yellow_x",
                         "mu_beta0_yellow_x", "tau_beta0_yellow_x",
                         "p_black", "beta0_black", "beta1_black", "beta2_black",  "beta3_black", "beta4_black",
                         "mu_beta0_black", "tau_beta0_black",
                         #random effects on species
                         "re_pelagic", "re_black","re_yellow",
                         "sd_comp", 
                         #harvest estimates and spline parts
                         "Htrend_ay", "H_ay", "sigma_H", "lambda_H", "H_ayg", "H_ayu", 
                         "Hb_ayg", "Hb_ayu", "Hb_ay",
                         "Hy_ayg", "Hy_ayu", "Hy_ay",
                         "logHhat_ay",
                         #with hierarchichal pline lambda
                         "mu_lambda_H","sigma_lambda_H","beta_H","beta0_H")) -> rhat_exam_params
rhat_exam_params %>%  summarise(n = n(),
            badRhat_avg = mean(Rhat)) %>%
  arrange(-badRhat_avg,-n) %>% print(n = 100)

rhat_exam_params %>% group_by(area) %>%
  summarise(n = n(),
            badRhat_avg = mean(Rhat)) %>%
  arrange(-n)  #print(n = 100)

rhat_exam_params %>% group_by(variable,area) %>%
  summarise(n = n(),
            badRhat_avg = mean(Rhat)) %>%
  arrange(-badRhat_avg,-n) -> par_x_area

with(par_x_area, table(variable,area))

rhat_exam %>% group_by(variable,area) %>%
  summarise(n = n(),
            badRhat_avg = mean(Rhat)) %>%
  arrange(-n,-badRhat_avg) %>% print(n = 100)



#--- Traceplots ----------------------------------------------------------------
area_codes

rhat_exam %>% group_by(variable,area) %>%
  summarise(n = n(),
            badRhat_avg = mean(Rhat)) %>%
  arrange(-badRhat_avg,-n) %>% 
  filter(str_detect(variable, "lambda"))

jagsUI::traceplot(postH, parameters = "lambda_H")

jagsUI::traceplot(postH, parameters = "beta_H")
jagsUI::traceplot(postH, parameters = "beta0_H")

jagsUI::traceplot(postH, parameters = "sigma_H")

jagsUI::traceplot(postH, parameters = c("mu_lambda_H","sigma_lambda_H"))

jagsUI::traceplot(postH, parameters = c("sd_pH","mu_beta0_pH","tau_beta0_pH",
                                        "beta0_pH","beta1_pH",
                                        "beta2_pH","beta3_pH","beta4_pH"))

rhat_exam %>% group_by(variable,area) %>%
  summarise(n = n(),
            badRhat_avg = mean(Rhat)) %>%
  arrange(-badRhat_avg,-n) %>% 
  filter(str_detect(variable, "pH"))

jagsUI::traceplot(postH, parameters = c("sd_comp","mu_beta0_yellow","tau_beta0_yellow",
                                        "beta0_yellow","beta1_yellow",
                                        "beta2_yellow","beta3_yellow","beta4_yellow"))

jagsUI::traceplot(postH, parameters = c("mu_beta0_yellow_x","tau_beta0_yellow_x",
                                        "beta0_yellow_x","beta1_yellow_x",
                                        "beta2_yellow_x","beta3_yellow_x","beta4_yellow_x"))

rhat_exam %>% group_by(variable,area) %>%
  summarise(n = n(),
            badRhat_avg = mean(Rhat)) %>%
  arrange(-badRhat_avg,-n) %>% 
  filter(str_detect(variable, "_yellow")) %>% print(n=50)

rhat_exam %>% group_by(variable,area) %>%
  summarise(n = n(),
            badRhat_avg = mean(Rhat)) %>%
  arrange(-badRhat_avg,-n) %>% 
  filter(str_detect(variable, "p_yellow")) %>% print(n=50)

jagsUI::traceplot(postH, parameters = c("mu_beta0_pelagic","tau_beta0_pelagic",
                                        "beta0_pelagic","beta1_pelagic",
                                        "beta2_pelagic","beta3_pelagic","beta4_pelagic"))

jagsUI::traceplot(postH, parameters = c("mu_beta0_black","tau_beta0_black",
                                        "beta0_black","beta1_black",
                                        "beta2_black","beta3_black","beta4_black"))

jagsUI::traceplot(postH, parameters = c("mu_bc_H","tau_bc_H","sd_bc_H"))
jagsUI::traceplot(postH, parameters = "logbc_H")

#jagsUI::traceplot(postH, parameters = c("mu_bc_C","tau_bc_C","sd_bc_C"))
postH$mean$bc_C_offset; exp(postH$mean$bc_C_offset)

jagsUI::traceplot(postH, parameters = c("bc_R_offset"))
jagsUI::traceplot(postH, parameters = "logbc_R")

jagsUI::traceplot(postH, parameters = c("b1_pG", "b2_pG","pG"))

library(coda)
library(shinystan)
launch_shinystan(as.shinystan(postH$samples))

# Inspect posterior --------------------------------------------------------
str(postH)

postH$mean$lambda_H
postH$mean$lambda_C
postH$mean$sigma_H
postH$mean$sigma_C
postH$mean$H_ay
postH$q2.5$pH_slo
postH$mean$pH_slo
postH$q97.5$pH_slo
postH$mean$sd_comp
paste0(round(postH$mean$beta1_pelagic, 3), "(", round(postH$q2.5$beta1_pelagic, 3), ", ", round(postH$q97.5$beta1_pelagic, 3), ")")
paste0(round(postH$mean$beta2_pelagic, 3), "(", round(postH$q2.5$beta2_pelagic, 3), ", ", round(postH$q97.5$beta2_pelagic, 3), ")")

# * Harvest --------------------------------------------------------
# ** SWHS total harvest vrs. model total harvest --------------------------------------------------------
jagsUI::traceplot(postH, parameters = c("H_ay","C_ay"))

as.data.frame(
  rbind(t(jags_dat$Hhat_ay),
        t(apply(exp(postH$sims.list$Htrend_ay), c(2,3), mean)),
        t(postH$mean$H_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 3),
         source = rep(c("SWHS", "trend", "H"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = H, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

as.data.frame(
  rbind(t(jags_dat$Rhat_ay),
        #t(apply(exp(postH$sims.list$Ctrend_ay), c(2,3), mean)),
        t(postH$mean$R_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 2),
         source = rep(c("SWHS", "R"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "R") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = R, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

rhat_exam %>% group_by(variable,area) %>%
  summarise(n = n(),
            badRhat_avg = mean(Rhat)) %>%
  arrange(-badRhat_avg,-n) %>% 
  filter(str_detect(variable, "R_ay")) %>% print(n=50)

as.data.frame(
  rbind(t(jags_dat$Rhat_ayg),
        t(jags_dat$Rlb_ayg), 
        #t(apply(exp(postH$sims.list$Ctrend_ay), c(2,3), mean)),
        t(postH$mean$R_ayg))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 3),
         source = rep(c("SWHS","LB" ,"R"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "R") %>%
  left_join(as.data.frame(
    (t(jags_dat$cvRhat_ayg))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 1),
             source = rep(c("SWHS"), each = Y)) %>%
      pivot_longer(!c(year, source), names_to = "area", values_to = "cv"),
    by = c("year","source","area")) %>%
  
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
         swhs_hi = R*(1+cv*1.96),
         swhs_lo = R*(1-cv*1.96)) %>%
  ggplot(aes(x = year, y = R, color = source)) +
  geom_ribbon(aes(x = year, ymin = swhs_lo, ymax = swhs_hi, fill = source), color = NA, alpha = 0.2) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

as.data.frame(
  rbind(t(jags_dat$Rlb_ayg),
        #t(apply(exp(postH$sims.list$Ctrend_ay), c(2,3), mean)),
        #t((postH$mean$C_ayg - postH$mean$H_ayg)))) %>%
        t((postH$mean$R_ayg)))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 2),
         source = rep(c("LB", "R"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "R") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(t(postH$q97.5$R_ayg) %>% data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(start_yr:end_yr, times = 1),
                     source = "R") %>%
              pivot_longer(!c(year, source), names_to = "area", values_to = "R_hi") %>%
              left_join(t(postH$q2.5$R_ayg) %>% data.frame() %>%
                          setNames(nm = unique(H_ayg$area)) %>%
                          mutate(year = rep(start_yr:end_yr, times = 1),
                                 source = "R") %>%
                          pivot_longer(!c(year, source), names_to = "area", values_to = "R_lo"),
                        by = c("year","source","area")),
            by = c("year","source","area")) %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = R, color = source)) +
  geom_ribbon(aes(ymin = R_lo, ymax = R_hi, fill = source),
              color = NA, alpha = 0.3) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

as.data.frame(
  rbind(t(jags_dat$Rlby_ayg),
        #t(apply(exp(postH$sims.list$Ctrend_ay), c(2,3), mean)),
        #t(postH$mean$Cy_ayg - postH$mean$Hy_ayg))) %>%
        t(postH$mean$Ry_ayg))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 2),
         source = rep(c("LB", "R"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "R") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = R, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

# * P(Harvested) --------------------------------------------------------
# ** annual estimates  --------------------------------------------------------
#pH_mod <- 
#  postH$mean$pH %>%
#  t() %>%
#  as.data.frame() %>%
#  setNames(nm = unique(H_ayg$area)) %>%
#  mutate(year = unique(Hhat_ay$year[1:Y]),
#         source = "model") %>%
#  pivot_longer(-c(year, source), names_to = "area", values_to = "pH") %>%
#  left_join(postH$q2.5$pH %>%
#              t() %>%
#              as.data.frame() %>%
#              setNames(nm = unique(H_ayg$area)) %>%
#              mutate(year = unique(Hhat_ay$year[1:Y]),
#                     source = "model") %>%
#              pivot_longer(-c(year, source), names_to = "area", values_to = "pH_lo95"),
#            by = c("year","source","area")) %>%
#  left_join(postH$q97.5$pH %>%
#              t() %>%
#              as.data.frame() %>%
#              setNames(nm = unique(H_ayg$area)) %>%
#              mutate(year = unique(Hhat_ay$year[1:Y]),
#                     source = "model") %>%
#              pivot_longer(-c(year, source), names_to = "area", values_to = "pH_hi95"),
#            by = c("year","source","area"))

pH_mod <- 
  rbind(postH$mean$pH[,,1] %>% t(),
        postH$mean$pH[,,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(rbind(postH$q2.5$pH[,,1] %>% t(),
                  postH$q2.5$pH[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
                     source = "model") %>%
              pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_lo95")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user","source")) %>%
  left_join(rbind(postH$q97.5$pH[,,1] %>% t(),
                  postH$q97.5$pH[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
                     source = "model") %>%
              pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_hi95")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user","source"))

#need to get Hhat_ayu and Rhat_ayu that are not part of model data

pH_obs <- 
  as.data.frame(
    rbind(t(jags_dat$Hhat_ayg/(jags_dat$Rhat_ayg + jags_dat$Hhat_ayg)),
          t(jags_dat$Hhat_ayu/(jags_dat$Rhat_ayu + jags_dat$Hhat_ayu)),
          t(jags_dat$Hhat_ay/(jags_dat$Rhat_ay + jags_dat$Hhat_ay)))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 3),
         user = rep(c("charter", "private","all"), each = length(unique(Hhat_ay$year))),
         source = "SWHS") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
         p_lo95 = NA, p_hi95 = NA) #%>%

rbind(beta0_pH <- postH$q50$beta0_pH,
      beta1_pH <- postH$q50$beta1_pH,
      beta2_pH <- postH$q50$beta2_pH,
      beta3_pH <- postH$q50$beta3_pH,
      beta4_pH <- postH$q50$beta4_pH) %>% t() %>%
  data.frame() %>%
  mutate(area = unique(H_ayg$area)) %>%
  rename(beta0 = X1, beta1=X2, beta2=X3, beta3=X4, beta4 = X5) %>%
  right_join(expand.grid(y = seq(1,end_yr-start_yr+1,1), u = c(1, 2), 
                         area = factor(comp$area, unique(H_ayg$area), ordered = TRUE)),
             by = "area") %>%
  mutate(logit_pH = beta0 +
           beta1 / (1 + exp(-beta2 * (y - beta3))) +
           beta4 * (u - 1),
         pH = logit_to_prob(logit_pH),
         year = y + 1976,
         user = ifelse(u == 1, "charter","private"))-> pH_trend

pH_obs %>% filter(user != "all") %>%
  ggplot(aes(x = year, y = pH, color = user)) +
  geom_ribbon(data = pH_mod, aes(ymin = p_lo95, ymax = p_hi95, fill = user), 
              alpha = 0.2, color = NA) +
  geom_point(aes(shape = source)) +
  geom_point(data  = pH_obs %>% filter(user == "all"), color = "darkgrey") +
  geom_line(data = pH_mod, aes(color = user)) +
  geom_line(data = pH_trend, aes(color = user)) +
  #geom_hline(data = p_pelagic_trend, aes(yintercept = p_pelagic), linetype = 2) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(. ~ area)


# ** observation error --------------------------------------------------------
as.data.frame(
  rbind(t(jags_dat$Hhat_ay),
        t(apply(exp(postH$sims.list$logHhat_ay), c(2,3), mean)))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 2),
         source = rep(c("SWHS", "mean"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = H, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area)
#  facet_wrap(. ~ area, scales = "free")

as.data.frame(
  rbind(t(jags_dat$Rhat_ay),
        t(apply(exp(postH$sims.list$logRhat_ay), c(2,3), mean)))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 2),
         source = rep(c("SWHS", "mean"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "R") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = R, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area)
#  facet_wrap(. ~ area, scales = "free")

#as.data.frame(
#  rbind(t(jags_dat$Rlb_ayg),
#        t(apply(exp(postH$sims.list$logChat_ayg), c(2,3), mean) -
#            apply(exp(postH$sims.list$logHhat_ayg), c(2,3), mean)))) %>%
#  setNames(nm = unique(H_ayg$area)) %>%
#  mutate(year = rep(start_yr:end_yr, times = 2),
#         source = rep(c("SWHS", "mean"), each = Y)) %>%
#  pivot_longer(!c(year, source), names_to = "area", values_to = "C") %>%
#  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
#  ggplot(aes(x = year, y = C, color = source)) +
#  geom_line() + 
#  facet_wrap(. ~ area, scales = "free")

# ** logbook harvest vrs. model total harvest -------------------------------------------------------- 
as.data.frame(
  rbind(t(jags_dat$Hlb_ayg),
        t(postH$mean$H_ay),
        t(apply(exp(postH$sims.list$Htrend_ay), c(2,3), mean)),
        t(jags_dat$Hhat_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 4),
         source = rep(c("logbook", "Harvest", "trend", "SWHS"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "H") %>%
  mutate(yr_group = ifelse(year <= 1997, "no logbook", ifelse(year <= 2010, "No user", "full data")),
         area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = H, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

# ** logbook harvest vrs. model charter harvest --------------------------------------------------------
as.data.frame(
  rbind(t(postH$mean$H_ayg),
        t(jags_dat$Hlb_ayg),
        t(postH$mean$H_ayu),
        t(postH$mean$H_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 4),
         source = rep(c("model", "logbook", "model", "model"), each = Y),
         user = rep(c("charter", "charter", "private", "total"), each = Y)) %>%
  pivot_longer(!c(year, source, user), names_to = "area", values_to = "H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = H, color = user, linetype = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

# ** logbook residuals --------------------------------------------------------
as.data.frame(
  t(postH$mean$H_ayg) - t(jags_dat$Hlb_ayg)) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = start_yr:end_yr) %>%
  pivot_longer(!year, names_to = "area", values_to = "res") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = res)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(. ~ area, scales = "free_y")
  
  as.data.frame(
    t(postH$mean$R_ayg) - t(jags_dat$Rlb_ayg)) %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = start_yr:end_yr) %>%
    pivot_longer(!year, names_to = "area", values_to = "res") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    ggplot(aes(x = year, y = res)) +
    geom_point() +
    geom_hline(aes(yintercept = 0)) +
    facet_wrap(. ~ area, scales = "free_y")

# ** SWHS residuals --------------------------------------------------------
#names(wes_palettes)
pal <- wes_palette(name = "Zissou1Continuous", type = "continuous")

as.data.frame(
  t(log(postH$mean$H_ay) + postH$mean$logbc_H) - t(log(jags_dat$Hhat_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = start_yr:end_yr) %>%
  pivot_longer(!year, names_to = "area", values_to = "H_res") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(as.data.frame(t(jags_dat$cvHhat_ay)) %>% setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = start_yr:end_yr) %>%
              pivot_longer(!year, names_to = "area", values_to = "cv") %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area")) -> swhs_Hres
swhs_Hres %>% ggplot(aes(x = year, y = H_res)) +
  scale_color_gradientn(colours = pal) + 
  geom_point(aes(color = cv)) +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(. ~ area, scales = "free_y")


as.data.frame(
  #t(log(postH$mean$C_ay) + postH$mean$logbc_C) - t(log(jags_dat$Chat_ay_pH))) %>%
  t(log(postH$mean$R_ay) + postH$mean$logbc_R) - t(log(jags_dat$Rhat_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = start_yr:end_yr) %>%
  pivot_longer(!year, names_to = "area", values_to = "R_res") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(as.data.frame(t(jags_dat$cvRhat_ay)) %>% setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = start_yr:end_yr) %>%
              pivot_longer(!year, names_to = "area", values_to = "cv") %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area"))-> swhs_Rres

swhs_Rres %>% filter(year > 1992) %>%
  ggplot(aes(cv)) + geom_histogram() + facet_wrap(. ~ area, scales = "free_y")

swhs_Rres %>%  
  filter(cv < 25) %>%
  ggplot(aes(x = year, y = R_res)) +
  scale_color_gradientn(colours = pal) + 
  geom_point(aes(color = cv)) +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(. ~ area, scales = "free_y")

area_codes
jagsUI::traceplot(postH, parameters = c("bc_R_offset"))
jagsUI::traceplot(postH, parameters = "logbc_R")
# ** yelloweye logbook harvest vrs. model charter harvest --------------------------------------------------------
as.data.frame(
  rbind(t(postH$mean$Hy_ayg),
        t(jags_dat$Hlby_ayg))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 2),
         source = rep(c("model", "logbook"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = H, linetype = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

as.data.frame(
  rbind(t(postH$mean$Ry_ayg),
        t(jags_dat$Rlby_ayg))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 2),
         source = rep(c("model", "logbook"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "R") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = R, linetype = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

# * User comp --------------------------------------------------------
# ** mean by area --------------------------------------------------------
pG <- postH$sims.list$b1_pG / (postH$sims.list$b1_pG + postH$sims.list$b2_pG) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) 
pG %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "pG") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = pG)) +
  geom_histogram(binwidth = 0.02) +
  facet_wrap(.~area)

# ** annual estimates  --------------------------------------------------------
pG_mod <- 
  postH$mean$pG %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ay$year),
         source = "model") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "pG") %>%
  left_join(postH$q2.5$pG %>%
              t() %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = unique(Hhat_ay$year),
                     source = "model") %>%
              pivot_longer(-c(year, source), names_to = "area", values_to = "pG_lo95"),
            by = c("year","source","area")) %>%
  left_join(postH$q97.5$pG %>%
              t() %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = unique(Hhat_ay$year),
                     source = "model") %>%
              pivot_longer(-c(year, source), names_to = "area", values_to = "pG_hi95"),
            by = c("year","source","area"))
pG_obs <- 
  (jags_dat$Hhat_ayg/jags_dat$Hhat_ay)[,35:Y] %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ayu$year[Hhat_ayu$year <= end_yr]),
         source = "observed") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "pG")

rbind(pG_mod, pG_obs%>% mutate(pG_lo95 = NA, pG_hi95 = NA)) %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = pG, color = source)) +
  geom_ribbon(aes(ymin = pG_lo95, ymax = pG_hi95, borders = NA, fill = source), 
              alpha = 0.2, color = NA) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(. ~ area)

# * SWHS bias --------------------------------------------------------
# ** mean by area --------------------------------------------------------
exp(postH$sims.list$mu_bc_H) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "bc") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
         data = "H") %>% #-> grr#%>%
  rbind(exp(postH$sims.list$mu_bc_H * postH$sims.list$bc_R_offset) %>%
          as.data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "bc") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
                 data = "R")) %>%
  rbind((postH$sims.list$bc_R_offset) %>%
          as.data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "bc") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
                 data = "R_offset")) -> bias

range(bias$bc) #YIKES!!!! 

bias %>% filter(bc < 6 & data != "R_offset") %>%
  ggplot(aes(x = bc, col = data, fill = data)) +
  geom_histogram(binwidth = .05, alpha = 0.2, position = "identity") +
  coord_cartesian(xlim = c(0, 3)) +
  geom_vline(aes(xintercept = 1)) +
  facet_wrap(.~area)

bias %>% filter(data == "R_offset") %>%
  ggplot(aes(x = bc, col = data, fill = data)) +
  geom_histogram(binwidth = .05, alpha = 0.2, position = "identity") +
  coord_cartesian(xlim = c(-2, 3)) +
  geom_vline(aes(xintercept = 1)) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  facet_wrap(.~area)

jagsUI::traceplot(postH, parameters = c("mu_bc_H","tau_bc_H","sd_bc_H"))
jagsUI::traceplot(postH, parameters = c("bc_R_offset"))

# ** sd by area --------------------------------------------------------
postH$sims.list$sd_bc_H %>%
  as.data.frame() %>%
  setNames(nm = unique(Hhat_ay$area)) %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "se_bc") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
         data = "H") %>%
  #rbind(postH$sims.list$sd_bc_C %>%
  #        as.data.frame() %>%
  #        setNames(nm = unique(Hhat_ay$area)) %>%
  #        pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "se_bc") %>%
  #        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
  #               data = "C")) %>%
  
  ggplot(aes(x = se_bc, col = data, fill = data)) +
  geom_histogram(binwidth = 0.1, alpha = 0.2, position = "identity") +
  facet_wrap(.~area) + coord_cartesian(xlim = c(0, 4))

# ** more bias exam --------------------------------------------------------

mu_bc_H <- data.frame(area = unique(H_ayg$area), mu_bc = apply(exp(postH$sims.list$mu_bc_H), 2, mean))
bc_mod <- 
  apply(exp(postH$sims.list$logbc_H), c(2, 3), mean) %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ay$year),
         source = "model") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
  mutate(data = "H") %>%
  rbind(apply(exp(postH$sims.list$logbc_H * 
                    array(postH$sims.list$bc_R_offset, 
                          dim = c(dim(postH$sims.list$logbc_H)[1],jags_dat$A,Y))), c(2, 3), mean) %>%
          t() %>%
          as.data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = unique(Hhat_ay$year),
                 source = "model") %>%
          pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
          mutate(data = "R"))

dim(postH$q50$logbc_H)
dim(postH$q50$bc_R_offset)

bc_mod2 <- 
  #apply(exp(postH$sims.list$logbc_H), c(2, 3), mean) %>%
  exp(postH$q50$logbc_H) %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ay$year),
         source = "model") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
  mutate(data = "H") %>%
  rbind(exp(postH$q50$logbc_H * array(postH$q50$bc_R_offset,
                                      dim = c(jags_dat$A,Y))) %>%
          t() %>%
          as.data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = unique(Hhat_ay$year),
                 source = "model") %>%
          pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
          mutate(data = "R"))

bc_obs <- 
  (jags_dat$Hhat_ayg/jags_dat$Hlb_ayg)[,35:Y] %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ayu$year[Hhat_ayu$year <= end_yr]),
         source = "observed") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
  mutate(data = "H") %>% 
  rbind((jags_dat$Rhat_ayg/jags_dat$Rlb_ayg)[,35:Y] %>%
  #rbind((jags_dat$Chat_ayg/(jags_dat$Rlb_ayg + jags_dat$Hlb_ayg))[,35:Y] %>%
          t() %>%
          as.data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = unique(Hhat_ayu$year[Hhat_ayu$year <= end_yr]),
                 source = "observed") %>%
          pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
          mutate(data = "R"))

rbind(bc_mod2, bc_obs) %>%
  filter(data == "H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = bc, color = source)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0, 5)) +
  geom_hline(aes(yintercept = mu_bc), data = mu_bc_H) +
  facet_wrap(. ~ area)

rbind(bc_mod2, bc_obs) %>%
  filter(data == "R") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = bc, color = source)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0, 5)) +
  geom_hline(aes(yintercept = mu_bc), data = mu_bc_H) +
  facet_wrap(. ~ area)

rbind(bc_mod2, bc_obs) %>%
  #filter(data == "C") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = bc, color = source)) +
  geom_point(aes(shape = data, fill = data),alpha = 0.4) +
  geom_line(aes(linetype = data)) +
  coord_cartesian(ylim = c(0, 5)) +
  geom_hline(aes(yintercept = mu_bc), data = mu_bc_H) +
  facet_wrap(. ~ area)

# * Composition --------------------------------------------------------
# ** Pelagic annual  --------------------------------------------------------
p_pelagic_mod <- 
  rbind(postH$mean$p_pelagic[,,1] %>% t(),
        postH$mean$p_pelagic[,,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
  pivot_longer(-c(year, user), names_to = "area", values_to = "p_pelagic")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(rbind(postH$q2.5$p_pelagic[,,1] %>% t(),
                  postH$q2.5$p_pelagic[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo95")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user")) %>%
  left_join(rbind(postH$q97.5$p_pelagic[,,1] %>% t(),
                  postH$q97.5$p_pelagic[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi95")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user"))

p_pelagic_obs <-
  comp %>%
  mutate(year = year_n + 1976, #1995,
         area = unique(H_ayg$area)[area_n],
         user = ifelse(user_n == 0, "charter", "private"),
         source = ifelse(source == 1, "sample", "logbook"),
         p_pelagic = pelagic / N,
         area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  select(year, area, user, source, p_pelagic) %>%
  rbind(H_ayg %>% 
          mutate(p_pelagic = Hp / H, 
                 user = "charter", 
                 source = "logbook") %>% 
          select(year, area, user, source, p_pelagic)) %>%
  mutate(p_lo95 = NA, p_hi95 = NA)

p_pelagic_trend <-
  data.frame(
    p_pelagic = boot::inv.logit(
      unlist(mapply(function(x, y) rep(x, each = y), postH$mean$mu_beta0_pelagic, c(4, 6, 6)))),
    area = unique(H_ayg$area))

rbind(beta0_pelagic <- postH$q50$beta0_pelagic,
      beta1_pelagic <- postH$q50$beta1_pelagic,
      beta2_pelagic <- postH$q50$beta2_pelagic,
      beta3_pelagic <- postH$q50$beta3_pelagic,
      beta4_pelagic <- postH$q50$beta4_pelagic) %>% t() %>%
  data.frame() %>%
  mutate(area = unique(H_ayg$area)) %>%
  rename(beta0 = X1, beta1=X2, beta2=X3, beta3=X4, beta4 = X5) %>%
  right_join(expand.grid(y = seq(1,end_yr-start_yr+1,1), u = c(1, 2), 
                         area = factor(comp$area, unique(H_ayg$area), ordered = TRUE)),
             by = "area") %>%
  mutate(logit_pel = beta0 +
           beta1 / (1 + exp(-beta2 * (y - beta3))) +
           beta4 * (u - 1),
         p_pelagic = logit_to_prob(logit_pel),
         year = y + 1976,
         user = ifelse(u == 1, "charter","private"))-> pel_trend

p_pelagic_obs %>%
  ggplot(aes(x = year, y = p_pelagic, color = user)) +
  geom_ribbon(data = p_pelagic_mod, aes(ymin = p_lo95, ymax = p_hi95, fill = user), 
              alpha = 0.2, color = NA) +
  geom_point(aes(shape = source)) +
  geom_line(data = p_pelagic_mod, linetype = 2) +
  geom_hline(data = p_pelagic_trend, aes(yintercept = p_pelagic), linetype = 2) +
  geom_line(data=pel_trend) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(. ~ area)


# ** black/pelagic annual  --------------------------------------------------------
p_black_mod <- 
  rbind(postH$mean$p_black[,,1] %>% t(),
        postH$mean$p_black[,,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user, source), names_to = "area", values_to = "p_black")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(rbind(postH$q2.5$p_black[,,1] %>% t(),
                  postH$q2.5$p_black[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo95")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user")) %>%
  left_join(rbind(postH$q97.5$p_black[,,1] %>% t(),
                  postH$q97.5$p_black[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi95")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user"))

p_black_obs <-
  #jags_dat[grep("comp_", names(jags_dat), value = TRUE)] %>%
  jags_dat[grep("comp_(?!.*_x)", names(jags_dat), value = TRUE, perl = TRUE)] %>%
  as.data.frame() %>%
  mutate(year = comp_year + 1976,
         area = (unique(H_ayg$area)[comp_area]),
         user = ifelse(comp_user == 0, "charter", "private"),
         p_black = comp_black / comp_pelagic,
         area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  mutate(p_lo95 = NA, p_hi95 = NA,
         area = factor(area, unique(H_ayg$area), ordered = TRUE))

p_black_trend <- data.frame(
  p_black = boot::inv.logit(
    unlist(mapply(function(x, y) rep(x, each = y), postH$mean$mu_beta0_black, c(4, 6, 6)))),
  area = unique(H_ayg$area))

p_black_mod %>%
  left_join(postH$mean$beta0_black %>% t() %>% data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              pivot_longer(cols = unique(H_ayg$area),
                           names_to = "area", values_to = "beta0") %>%
              left_join(postH$mean$beta1_black %>% t() %>% data.frame() %>%
                          setNames(nm = unique(H_ayg$area)) %>%
                          pivot_longer(cols = unique(H_ayg$area),
                                       names_to = "area", values_to = "beta1"), 
                        by = "area") %>%
              left_join(postH$mean$beta2_black %>% t() %>% data.frame() %>%
                          setNames(nm = unique(H_ayg$area)) %>%
                          pivot_longer(cols = unique(H_ayg$area),
                                       names_to = "area", values_to = "beta2"), 
                        by = "area"), 
            by = c("area")) %>%
  mutate(trend = ifelse(user == "charter",
                        beta0+beta1*(year-1976)+beta2,
                        beta0+beta1*(year-1976))) -> p_black_mod

rbind(beta0_black <- postH$q50$beta0_black,
      beta1_black <- postH$q50$beta1_black,
      beta2_black <- postH$q50$beta2_black,
      beta3_black <- postH$q50$beta3_black,
      beta4_black <- postH$q50$beta4_black) %>% t() %>%
  data.frame() %>%
  mutate(area = unique(H_ayg$area)) %>%
  rename(beta0 = X1, beta1=X2, beta2=X3, beta3=X4, beta4 = X5) %>%
  right_join(expand.grid(y = seq(1,end_yr-start_yr+1,1), u = c(1, 2), 
                         area = factor(comp$area, unique(H_ayg$area), ordered = TRUE)),
             by = "area") %>%
  mutate(logit_b = beta0 +
           beta1 / (1 + exp(-beta2 * (y - beta3))) +
           beta4 * (u - 1),
         p_black = logit_to_prob(logit_b),
         year = y + 1976,
         user = ifelse(u == 1, "charter","private"))-> black_trend

rbind(p_black_obs) %>%
  ggplot(aes(x = year, y = p_black, color = user)) +
  geom_ribbon(data = p_black_mod, aes(ymin = p_lo95, ymax = p_hi95, fill = user), 
              alpha = 0.2, color = NA) +
  geom_point() +
  geom_line(data = p_black_mod, linetype = 2) +
  #geom_line(data = p_black_mod, aes(x=year, y=trend,color=user)) +
  geom_hline(data = p_black_trend, aes(yintercept = p_black), linetype = 2) +
  geom_line(data=black_trend) +
  scale_alpha_manual(values = c(0.2, 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(. ~ area)

# ** yellow/non-pelagic annual  --------------------------------------------------------
p_yellow_mod <- 
  rbind(postH$mean$p_yellow[,,1] %>% t(),
        postH$mean$p_yellow[,,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user, source), names_to = "area", values_to = "p_yellow")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(rbind(postH$q2.5$p_yellow[,,1] %>% t(),
                  postH$q2.5$p_yellow[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo95")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user")) %>%
  left_join(rbind(postH$q97.5$p_yellow[,,1] %>% t(),
                  postH$q97.5$p_yellow[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi95")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user")) %>%
  left_join(rbind(postH$q25$p_yellow[,,1] %>% t(),
                  postH$q25$p_yellow[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo50")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user")) %>%
  left_join(rbind(postH$q75$p_yellow[,,1] %>% t(),
                  postH$q75$p_yellow[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi50")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user")) %>%
  left_join(rbind(postH$q50$p_yellow[,,1] %>% t(),
                  postH$q50$p_yellow[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "med_p")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user"))

p_yellowX_mod <- 
  rbind(postH$mean$p_yellow_x[,,1] %>% t(),
        postH$mean$p_yellow_x[,,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user, source), names_to = "area", values_to = "p_yellow")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(rbind(postH$q2.5$p_yellow_x[,,1] %>% t(),
                  postH$q2.5$p_yellow_x[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo95")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user")) %>%
  left_join(rbind(postH$q97.5$p_yellow_x[,,1] %>% t(),
                  postH$q97.5$p_yellow_x[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi95")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user")) %>%
  left_join(rbind(postH$q25$p_yellow_x[,,1] %>% t(),
                  postH$q25$p_yellow_x[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo50")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user")) %>%
  left_join(rbind(postH$q75$p_yellow_x[,,1] %>% t(),
                  postH$q75$p_yellow_x[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi50")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user")) %>%
  left_join(rbind(postH$q50$p_yellow_x[,,1] %>% t(),
                  postH$q50$p_yellow_x[,,2] %>% t()) %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 2),
                     user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
              pivot_longer(-c(year, user), names_to = "area", values_to = "med_p")  %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area","user"))

p_yellow_obs <-
#  jags_dat[grep("comp_", names(jags_dat), value = TRUE)] %>%
  jags_dat[grep("comp_(?!.*_x)", names(jags_dat), value = TRUE, perl = TRUE)] %>%
  as.data.frame() %>%
  mutate(year = comp_year + 1976,
         area = unique(H_ayg$area)[comp_area],
         user = ifelse(comp_user == 0, "charter", "private"),
         p_yellow = comp_yellow / (comp_N - comp_pelagic),
         area = factor(area, unique(H_ayg$area), ordered = TRUE))

p_yellowX_obs <- compX %>%
  mutate(p_yellow = yellow_x / (N - pelagic),
         user = ifelse(user_n == 0, "charter", "private"))
  
p_yellow_trend <-
  data.frame(
    p_yellow = boot::inv.logit(
      unlist(mapply(function(x, y) rep(x, each = y), postH$mean$mu_beta0_yellow, c(4, 6, 6)))),
    area = unique(H_ayg$area))

rbind(beta0_ye <- postH$q50$beta0_yellow,
      beta1_ye <- postH$q50$beta1_yellow,
      beta2_ye <- postH$q50$beta2_yellow,
      beta3_ye <- postH$q50$beta3_yellow,
      beta4_ye <- postH$q50$beta4_yellow) %>% t() %>%
  data.frame() %>%
  mutate(area = unique(H_ayg$area)) %>%
  rename(beta0 = X1, beta1=X2, beta2=X3, beta3=X4, beta4 = X5) %>%
  right_join(expand.grid(y = seq(1,end_yr-start_yr+1,1), u = c(1, 2), 
                         area = factor(comp$area, unique(H_ayg$area), ordered = TRUE)),
             by = "area") %>%
  mutate(logit_y = beta0 +
           beta1 / (1 + exp(-beta2 * (y - beta3))) +
           beta4 * (u - 1),
         p_yellow = logit_to_prob(logit_y),
         year = y + 1976,
         user = ifelse(u == 1, "charter","private"))-> yellow_trend

rbind(beta0_yeX <- postH$q50$beta0_yellow_x,
      beta1_yeX <- postH$q50$beta1_yellow_x,
      beta2_yeX <- postH$q50$beta2_yellow_x,
      beta3_yeX <- postH$q50$beta3_yellow_x,
      beta4_yeX <- postH$q50$beta4_yellow_x) %>% t() %>%
  data.frame() %>%
  mutate(area = unique(H_ayg$area)) %>%
  rename(beta0 = X1, beta1=X2, beta2=X3, beta3=X4, beta4 = X5) %>%
  right_join(expand.grid(y = seq(1,end_yr-start_yr+1,1), u = c(1, 2), 
                         area = factor(comp$area, unique(H_ayg$area), ordered = TRUE)),
             by = "area") %>%
  mutate(logit_y = beta0 +
           beta1 / (1 + exp(-beta2 * (y - beta3))) +
           beta4 * (u - 1),
         p_yellow = logit_to_prob(logit_y),
         year = y + 1976,
         user = ifelse(u == 1, "charter","private"))-> yellow_x_trend

southeast <- c("CSEO","EWYKT","NSEI","NSEO","SSEI","SSEO")
central <- c("CI","NG","PWSI","PWSO","BSAI","SOKO2SAP","WKMA","afognak","eastside","northeast")

rbind(p_yellow_obs) %>%
  ggplot(aes(x = year, y = p_yellow, color = user)) +
  geom_ribbon(data = p_yellowX_mod, 
              aes(ymin = p_lo95, ymax = p_hi95, fill = user), 
              alpha = 0.15, color = NA) +
  geom_ribbon(data = p_yellow_mod %>% filter(area %in% central),
              aes(ymin = p_lo95, ymax = p_hi95, fill = user), 
              alpha = 0.15, color = NA) +
  geom_point() +
  #geom_point(data = p_yellowX_obs, aes(x = year, y = p_yellow), color = "yellow", size = 0.5) +
  geom_line(data = p_yellow_mod %>% filter(area %in% central), linetype = 2) +
  geom_line(data = p_yellowX_mod, linetype=2) +
  #geom_line(data = p_yellow_mod, aes(x = year, y = trend, color = user)) +
  #geom_line(data = p_yellow_mod, aes(, y = med_p), linetype = 2, size = 0.5) +
  geom_line(data = yellow_x_trend) +
  geom_line(data = yellow_trend %>% filter(area %in% central)) +
  geom_hline(data = p_yellow_trend, aes(yintercept = p_yellow), linetype = 2) +
  scale_alpha_manual(values = c(0.2, 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(. ~ area)


#-------------------------------------------------------------------------------
#center year for logistic regression of p_black 
#Prior on random random effect se or heir model. SD_comp fixed atm.

rbind(postH$mean$re_pelagic[,,1] %>% t(),
      postH$mean$re_pelagic[,,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
  pivot_longer(-c(year, user
  ), names_to = "area", values_to = "p_pelagic")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = p_pelagic, color = user)) +
  geom_line() +
  facet_wrap(. ~ area)

# ------------------------------------------------------------------------------
# COMPARE TO HOWARD ETAL METHODS: 
# * Black Rockfish Harvest -----------------------------------------------------

brf_how <- read.csv(paste0("output\\BRF_harv_Howard_thru",REP_YR,".csv")) %>%
  clean_names() %>%
  select(region,year,area = rpt_area, gui_brf, var_gui_brf, priv_brf, var_priv_brf,
         tot_brf = total_br_fharv, var_tot_brf = var_total_br_fharv) 

brf_how %>% select(-c("var_gui_brf","var_priv_brf","var_tot_brf")) %>%
  pivot_longer(cols = c("gui_brf","priv_brf","tot_brf"),
               names_to = "user",
               values_to = "H_how") %>%
  mutate(user = ifelse(user == "gui_brf","guided",
                       ifelse(user == "priv_brf","unguided","All"))) %>%
  left_join(brf_how %>% select(-c("gui_brf","priv_brf","tot_brf")) %>%
              pivot_longer(cols = c("var_gui_brf","var_priv_brf","var_tot_brf"),
                           names_to = "user",
                           values_to = "var_H_how") %>%
              mutate(user = ifelse(user == "var_gui_brf","guided",
                                   ifelse(user == "var_priv_brf","unguided","All"))),
            by = c("region","year","area","user")) %>%
  mutate(se_H_how = sqrt(var_H_how),
         lo95_H_how = ifelse(H_how - 1.96 * se_H_how < 0,0,
                             H_how - 1.96 * se_H_how),
         hi95_H_how = H_how + 1.96 * se_H_how) -> brf_how


as.data.frame(
  rbind(t(postH$mean$Hb_ayg),
        t(postH$mean$Hb_ayu),
        t(postH$mean$Hb_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 3),
         user = rep(c("guided", "unguided", "All"), each = Y)) %>%
  pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(as.data.frame(
    rbind(t(postH$q2.5$Hb_ayg),
          t(postH$q2.5$Hb_ayu),
          t(postH$q2.5$Hb_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "H_lo95") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
    by = c("year","user","area")) %>%
  left_join(as.data.frame(
    rbind(t(postH$q97.5$Hb_ayg),
          t(postH$q97.5$Hb_ayu),
          t(postH$q97.5$Hb_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "H_hi95") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
    by = c("year","user","area")) %>%
  mutate(area = toupper(area)) %>%
  left_join(brf_how, by = c("year","user","area")) %>%
  
  ggplot(aes(x = year, y = H, color = user)) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95, fill = user), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(aes(x = year, y = H_how, color = user, shape = user)) +
  geom_errorbar(aes(x = year, ymin=lo95_H_how, ymax=hi95_H_how, color = user), width=.2,
                position=position_dodge(0.05))

# * YELLOWEYE Rockfish Harvest --------------------------------------------------------
# ** SWHS total harvest vrs. model total harvest --------------------------------------------------------
ye_how <- read.csv(paste0("output\\YE_harv_Howard_thru",REP_YR,".csv")) %>%
  clean_names() %>%
  select(region,year,area = rpt_area, gui_ye, var_gui_ye, priv_ye, var_priv_ye,
         tot_ye = total_y_eharv, var_tot_ye = var_total_y_eharv) 

ye_how %>% select(-c("var_gui_ye","var_priv_ye","var_tot_ye")) %>%
  pivot_longer(cols = c("gui_ye","priv_ye","tot_ye"),
               names_to = "user",
               values_to = "H_how") %>%
  mutate(user = ifelse(user == "gui_ye","guided",
                       ifelse(user == "priv_ye","unguided","All"))) %>%
  left_join(ye_how %>% select(-c("gui_ye","priv_ye","tot_ye")) %>%
              pivot_longer(cols = c("var_gui_ye","var_priv_ye","var_tot_ye"),
                           names_to = "user",
                           values_to = "var_H_how") %>%
              mutate(user = ifelse(user == "var_gui_ye","guided",
                                   ifelse(user == "var_priv_ye","unguided","All"))),
            by = c("region","year","area","user")) %>%
  mutate(se_H_how = sqrt(var_H_how),
         lo95_H_how = ifelse(H_how - 1.96 * se_H_how < 0,0,
                             H_how - 1.96 * se_H_how),
         hi95_H_how = H_how + 1.96 * se_H_how) -> ye_how

as.data.frame(
  rbind(t(postH$mean$Hy_ayg),
        t(postH$mean$Hy_ayu),
        t(postH$mean$Hy_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 3),
         user = rep(c("guided", "unguided", "All"), each = Y)) %>%
  pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(as.data.frame(
    rbind(t(postH$q2.5$Hy_ayg),
          t(postH$q2.5$Hy_ayu),
          t(postH$q2.5$Hy_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "H_lo95") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
    by = c("year","user","area")) %>%
  left_join(as.data.frame(
    rbind(t(postH$q97.5$Hy_ayg),
          t(postH$q97.5$Hy_ayu),
          t(postH$q97.5$Hy_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "H_hi95") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
    by = c("year","user","area")) %>%
  mutate(area = toupper(area)) %>%
  left_join(ye_how, by = c("year","user","area")) %>%
  
  ggplot(aes(x = year, y = H, color = user)) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95, fill = user), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(aes(x = year, y = H_how, color = user, shape = user)) +
  geom_errorbar(aes(x = year, ymin=lo95_H_how, ymax=hi95_H_how, color = user), width=.2,
                position=position_dodge(0.05))

# !! NOTE !! Eastside private are being estimated near 0. I don't think there is 
# anything wrong; better use of data in model and howard methods are small and
# likely the result of borrowing species comp data from other region. 

#------------------------------------------------------------------------------
# Check releases:
as.data.frame(
  rbind(t(postH$mean$R_ay),
        t(postH$mean$Rb_ay),
        t(postH$mean$Ry_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 3),
         group = rep(c("tot_r", "black_r", "yellow_r"), each = Y)) %>%
  pivot_longer(!c(year,group), names_to = "area", values_to = "R") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  
  bind_rows(as.data.frame(
    rbind(t(jags_dat$Chat_ay_pH - jags_dat$Hhat_ay),
          #t(apply(exp(postH$sims.list$Ctrend_ay), c(2,3), mean)),
          t(postH$mean$R_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 2),
             group = rep(c("SWHS", "R_est"), each = Y)) %>%
      pivot_longer(!c(year, group), names_to = "area", values_to = "R") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) ) %>%
  
  ggplot(aes(x = year, y = R, color = group)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free") 

as.data.frame(
  rbind(t(postH$mean$R_ayg),
        t(postH$mean$Rb_ayg),
        t(postH$mean$Ry_ayg))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 3),
         group = rep(c("tot_r", "black_r", "yellow_r"), each = Y)) %>%
  pivot_longer(!c(year,group), names_to = "area", values_to = "R") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  bind_rows(as.data.frame(
    rbind(t(jags_dat$Chat_ayg - jags_dat$Hhat_ayg),
          #t(apply(exp(postH$sims.list$Ctrend_ay), c(2,3), mean)),
          t(jags_dat$Rlb_ayg))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 2),
             group = rep(c("SWHS", "lb_R"), each = Y)) %>%
      pivot_longer(!c(year, group), names_to = "area", values_to = "R") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) ) %>%
  
  ggplot(aes(x = year, y = R, color = group)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free") 

jags_dat$Rlb_ayg
# * Black Rockfish RELEASE --------------------------------------------------------
# ** SWHS total harvest vrs. model total harvest --------------------------------------------------------

brf_howR <- read.csv(paste0("output\\BRF_rel_Howard_thru",REP_YR,".csv")) %>%
  clean_names() %>%
  select(region,year,area = rpt_area, gui_brf, var_gui_brf, priv_brf, var_priv_brf,
         tot_brf = total_br_frel, var_tot_brf = var_total_br_frel) %>% unique() #note some duplicate BSAI and SOKO2SAPs

brf_howR %>% select(-c("var_gui_brf","var_priv_brf","var_tot_brf")) %>%
  pivot_longer(cols = c("gui_brf","priv_brf","tot_brf"),
               names_to = "user",
               values_to = "R_how") %>%
  mutate(user = ifelse(user == "gui_brf","guided",
                       ifelse(user == "priv_brf","unguided","All"))) %>%
  left_join(brf_howR %>% select(-c("gui_brf","priv_brf","tot_brf")) %>%
              pivot_longer(cols = c("var_gui_brf","var_priv_brf","var_tot_brf"),
                           names_to = "user",
                           values_to = "var_R_how") %>%
              mutate(user = ifelse(user == "var_gui_brf","guided",
                                   ifelse(user == "var_priv_brf","unguided","All"))),
            by = c("region","year","area","user")) %>%
  mutate(se_R_how = sqrt(var_R_how),
         lo95_R_how = ifelse(R_how - 1.96 * se_R_how < 0,0,
                             R_how - 1.96 * se_R_how),
         hi95_R_how = R_how + 1.96 * se_R_how) -> brf_howR

as.data.frame(
  rbind(t(postH$mean$Rb_ayg),
        t(postH$mean$Rb_ayu),
        t(postH$mean$Rb_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 3),
         user = rep(c("guided", "unguided", "All"), each = Y)) %>%
  pivot_longer(!c(year,user), names_to = "area", values_to = "R") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(as.data.frame(
    rbind(t(postH$q2.5$Rb_ayg),
          t(postH$q2.5$Rb_ayu),
          t(postH$q2.5$Rb_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "R_lo95") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
    by = c("year","user","area")) %>%
  left_join(as.data.frame(
    rbind(t(postH$q97.5$Rb_ayg),
          t(postH$q97.5$Rb_ayu),
          t(postH$q97.5$Rb_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "R_hi95") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
    by = c("year","user","area")) %>%
  mutate(area = toupper(area)) %>%
  left_join(brf_howR, by = c("year","user","area")) %>%
  filter(user %in% c("guided","unguided")) %>%
  ggplot(aes(x = year, y = R, color = user)) +
  geom_line() + 
  #geom_ribbon(aes(x=year,ymin = R_lo95, ymax = R_hi95, fill = user), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(aes(x = year, y = R_how, color = user, shape = user)) +
  geom_errorbar(aes(x = year, ymin=lo95_R_how, ymax=hi95_R_how, color = user), width=.2,
                position=position_dodge(0.05)) +
  geom_ribbon(aes(x=year,ymin = R_lo95, ymax = R_hi95, fill = user), alpha = 0.25, color = NA) +
  geom_line()


# * YELLOWEYE Rockfish Harvest --------------------------------------------------------
# ** SWHS total harvest vrs. model total harvest --------------------------------------------------------
ye_howR <- read.csv(paste0("output\\YE_rel_Howard_thru",REP_YR,".csv")) %>%
  clean_names() %>%
  select(region,year,area = rpt_area, gui_ye, var_gui_ye, priv_ye, var_priv_ye,
         tot_ye = total_y_erel, var_tot_ye = var_total_y_erel) %>% unique()

ye_howR %>% select(-c("var_gui_ye","var_priv_ye","var_tot_ye")) %>%
  pivot_longer(cols = c("gui_ye","priv_ye","tot_ye"),
               names_to = "user",
               values_to = "R_how") %>%
  mutate(user = ifelse(user == "gui_ye","guided",
                       ifelse(user == "priv_ye","unguided","All"))) %>%
  left_join(ye_howR %>% select(-c("gui_ye","priv_ye","tot_ye")) %>%
              pivot_longer(cols = c("var_gui_ye","var_priv_ye","var_tot_ye"),
                           names_to = "user",
                           values_to = "var_R_how") %>%
              mutate(user = ifelse(user == "var_gui_ye","guided",
                                   ifelse(user == "var_priv_ye","unguided","All"))),
            by = c("region","year","area","user")) %>%
  mutate(se_R_how = sqrt(var_R_how),
         lo95_R_how = ifelse(R_how - 1.96 * se_R_how < 0,0,
                             R_how - 1.96 * se_R_how),
         hi95_R_how = R_how + 1.96 * se_R_how) -> ye_howR

#ye_howR %>% filter(area == "PWSI") %>% arrange(user,year) %>% print(n = 80)

as.data.frame(
  rbind(t(postH$mean$Ry_ayg),
        t(postH$mean$Ry_ayu),
        t(postH$mean$Ry_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 3),
         user = rep(c("guided", "unguided", "All"), each = Y)) %>%
  pivot_longer(!c(year,user), names_to = "area", values_to = "R") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(as.data.frame(
    rbind(t(postH$q2.5$Ry_ayg),
          t(postH$q2.5$Ry_ayu),
          t(postH$q2.5$Ry_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "R_lo95") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
    by = c("year","user","area")) %>%
  left_join(as.data.frame(
    rbind(t(postH$q97.5$Ry_ayg),
          t(postH$q97.5$Ry_ayu),
          t(postH$q97.5$Ry_ay))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(start_yr:end_yr, times = 3),
             user = rep(c("guided", "unguided", "All"), each = Y)) %>%
      pivot_longer(!c(year,user), names_to = "area", values_to = "R_hi95") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
    by = c("year","user","area")) %>%
  mutate(area = toupper(area)) %>%
  left_join(ye_howR, by = c("year","user","area")) %>%
  
  ggplot(aes(x = year, y = R, color = user)) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = R_lo95, ymax = R_hi95, fill = user), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(aes(x = year, y = R_how, color = user, shape = user)) +
  geom_errorbar(aes(x = year, ymin=lo95_R_how, ymax=hi95_R_how, color = user), width=.2,
                position=position_dodge(0.05))


#

#-------------------------------------------------------------------------------






