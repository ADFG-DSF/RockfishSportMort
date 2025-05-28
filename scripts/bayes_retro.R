################################################################################
# ROCKFISH HARVEST AND RELEASE ESTIMATION RETROSPECTIVE EXAM
#
# This code examines retrospective patterns in the Bayesian model for estimating
# rockfish removals and harvests
#
Current Author: Phil Joy (philip.joy@alaska.gov)
#
# Lats updated: June 2025
#
################################################################################
library(readxl)
library(janitor)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(wesanderson)
library(jagsUI)
library(rjags)

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
ni <- 1E5; nb <- ni*.25; nc <- 3; nt <- (ni - nb) / 1000
# 15e5 = 1.6 - 1.7 days
# 25e5 = 2.9 days

#model to run; see /models folder
mod <- "rf_harvest_est_kha_rm_wt" #at 15e5, second half of trace plots look converged, pH_1 may need tightening; p_black may need rethinking on hyper priors to align with inside/outside rather than regions?

#-------------------------------------------------------------------------------
#Are we using starting values from a prior model?
use_inits = "yes"

use_this_model <- "rf_harvest_est_kha_rm_wt_thru2023_1500000_7kn_2025-05-25" #for yelloweye betas:
#use_this_model <- "HR_fitLBR_2bias_hierPcomp_5pH_infPr_thru2023_2e+06_7kn_2025-01-26"

initspost <- readRDS(paste0(".\\output\\bayes_posts\\",use_this_model,".rds"))

halfway <- floor(nrow(as.matrix(initspost$samples[[1]])) / 4)

other_inits <- lapply(1:nc, function(chain) {
  chain_data <- as.matrix(initspost$samples[[chain]])
  #as.list(chain_data[nrow(chain_data), ])
  second_half <- chain_data[(halfway + 1):nrow(chain_data), , drop = FALSE]  # Extract second half
  as.list(colMeans(second_half))
})

last_inits <- other_inits

# Name the initial values you want to use:
inits_to_use <- last_inits

inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_comp"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_pH"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "sd_pH"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta0_pelagic"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta0_yellow"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta0_black"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta0_dsr"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta0_slope"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta1_pelagic"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta1_yellow"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta1_black"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta1_dsr"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta1_slope"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta2_pelagic"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta2_yellow"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta2_black"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta2_dsr"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta2_slope"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta3_pelagic"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta3_yellow"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta3_black"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta3_dsr"]
})
inits_to_use <- lapply(inits_to_use, function(chain_list) {
  chain_list[names(chain_list) != "tau_beta3_slope"]
})

for (i in 1:3){
  inits_to_use[[i]]$`mu_beta4_pH[2,1]` <- 0
  inits_to_use[[i]]$`mu_beta4_pH[2,2]` <- 0
  inits_to_use[[i]]$`beta4_pH[10,1]` <- -0.2
  inits_to_use[[i]]$`beta4_pH[10,1]` <- 0
}

###############################################################################
retro <- 10

for (i in 1:retro){ #i <- 1
  start_yr <- 1977
  retro_yr <- end_yr - i
  
  #load the data:
  list2env(readinData(spl_knts = 7,
                      start_yr = start_yr,
                      end_yr = retro_yr),
           .GlobalEnv)
  
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
  
  other_label <- paste0("retro-",i)
  
  saveRDS(postH, paste0(".\\output\\bayes_posts\\retros\\",mod,"_thru",retro_yr,"_",ni,"_",other_label,".rds"))
  saveRDS(postH, paste0("H:\\Documents\\Rockfish_SF_mortality\\RockfishSportMort\\output\\bayes_posts\\retros\\",mod,"_thru",retro_yr,"_",ni,".rds"))
}





























