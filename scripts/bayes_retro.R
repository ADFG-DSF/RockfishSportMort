################################################################################
# ROCKFISH HARVEST AND RELEASE ESTIMATION RETROSPECTIVE EXAM
#
# This code examines retrospective patterns in the Bayesian model for estimating
# rockfish removals and harvests
#
# Current Author: Phil Joy (philip.joy@alaska.gov)
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
library(scales)
library(dplyr)

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
ni <- 15E5; nb <- ni*.25; nc <- 3; nt <- (ni - nb) / 1000
# 15e5 = 1.6 - 1.7 days
# 25e5 = 2.9 days

#model to run; see /models folder
mod <- "rf_harvest_est_nm_wt" #at 15e5, second half of trace plots look converged, pH_1 may need tightening; p_black may need rethinking on hyper priors to align with inside/outside rather than regions?

#-------------------------------------------------------------------------------
#Are we using starting values from a prior model?
use_inits = "yes"

use_this_model <- "rf_harvest_est_nm_wt_thru2023_3e+06__2025-06-08"

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

for (i in 1:3){
  inits_to_use[[i]]$`re_slope[14,44,2]` <- 0
}

inits_to_use[[i]]$`re_slope[14,44,2]`
###############################################################################
retro <- 10

for (i in 6:retro){ #i <- 5
  start_yr <- 1977
  retro_yr <- end_yr - i
  
  #load the data:
  list2env(readinData(spl_knts = 7,
                      start_yr = start_yr,
                      end_yr = retro_yr),
           .GlobalEnv)
  
  if (i > 5){
    inits_to_use <- lapply(inits_to_use, function(chain_list) {
      chain_list[names(chain_list) != "mu_beta3_slope"]
    })
  }
  
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
  #saveRDS(postH, paste0("H:\\Documents\\Rockfish_SF_mortality\\RockfishSportMort\\output\\bayes_posts\\retros\\",mod,"_thru",retro_yr,"_",ni,"_",other_label,".rds"))
}

################################################################################
# Examine results
#retro <- 4
for (i in 0:retro){ #i <- 1
  retro_yr <- end_yr - i
  
  list2env(readinData(spl_knts = 7,
                      start_yr = start_yr,
                      end_yr = retro_yr),
           .GlobalEnv)
 
  if (i == 0){
    postH <- initspost
  } else {
    postH <- readRDS(paste0(".\\output\\bayes_posts\\retros\\",mod,"_thru",retro_yr,"_",ni,".rds"))
  }
  
  all_rhat <- get_Rhat(postH,cutoff = 0.01)
  names(all_rhat)[1] <- "Rhat_values"
  as.vector(all_rhat$Rhat_values) %>% data.frame()-> rhat_vals
  prop_conv <- round(nrow(rhat_vals %>% filter(Rhat <= 1.115))/nrow(rhat_vals),4)
  
  as.data.frame(
    rbind(t(postH$q50$Bb_ayg),
          t(postH$q50$Bb_ayu),
          t(postH$q50$Bb_ay))) %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(start_yr:retro_yr, times = 3),
           user = rep(c("guided", "unguided", "All"), each = Y)) %>%
    pivot_longer(!c(year,user), names_to = "area", values_to = "B") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(as.data.frame(
      rbind(t(postH$q2.5$Bb_ayg),
            t(postH$q2.5$Bb_ayu),
            t(postH$q2.5$Bb_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "B_lo95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$q97.5$Bb_ayg),
            t(postH$q97.5$Bb_ayu),
            t(postH$q97.5$Bb_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "B_hi95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$sd$Bb_ayg),
            t(postH$sd$Bb_ayu),
            t(postH$sd$Bb_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "sd_B") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    mutate(area = toupper(area)) %>%
    mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE),
           category = "Total mortality",
           retro = i,
           retro_name = paste0("retro-",i),
           prop_conv = prop_conv) ->brf_removals
  
  as.data.frame(
    rbind(t(postH$q50$Rb_ayg),
          t(postH$q50$Rb_ayu),
          t(postH$q50$Rb_ay))) %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(start_yr:retro_yr, times = 3),
           user = rep(c("guided", "unguided", "All"), each = Y)) %>%
    pivot_longer(!c(year,user), names_to = "area", values_to = "R") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(as.data.frame(
      rbind(t(postH$q2.5$Rb_ayg),
            t(postH$q2.5$Rb_ayu),
            t(postH$q2.5$Rb_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "R_lo95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$q97.5$Rb_ayg),
            t(postH$q97.5$Rb_ayu),
            t(postH$q97.5$Rb_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "R_hi95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$sd$Rb_ayg),
            t(postH$sd$Rb_ayu),
            t(postH$sd$Rb_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "sd_R") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    mutate(area = toupper(area)) %>%
    mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE),
           category = "Total released",
           retro = i,
           retro_name = paste0("retro-",i),
           prop_conv = prop_conv) ->brf_releases
  
  as.data.frame(
    rbind(t(postH$q50$Hb_ayg),
          t(postH$q50$Hb_ayu),
          t(postH$q50$Hb_ay))) %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(start_yr:retro_yr, times = 3),
           user = rep(c("guided", "unguided", "All"), each = Y)) %>%
    pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(as.data.frame(
      rbind(t(postH$q2.5$Hb_ayg),
            t(postH$q2.5$Hb_ayu),
            t(postH$q2.5$Hb_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "H_lo95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$q97.5$Hb_ayg),
            t(postH$q97.5$Hb_ayu),
            t(postH$q97.5$Hb_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "H_hi95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$sd$Hb_ayg),
            t(postH$sd$Hb_ayu),
            t(postH$sd$Hb_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "sd_H") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    mutate(area = toupper(area)) %>%
    mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE),
           category = "Total harvested",
           retro = i,
           retro_name = paste0("retro-",i),
           prop_conv = prop_conv) ->brf_harvests
  
  as.data.frame(
    rbind(t(postH$q50$By_ayg),
          t(postH$q50$By_ayu),
          t(postH$q50$By_ay))) %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(start_yr:retro_yr, times = 3),
           user = rep(c("guided", "unguided", "All"), each = Y)) %>%
    pivot_longer(!c(year,user), names_to = "area", values_to = "B") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(as.data.frame(
      rbind(t(postH$q2.5$By_ayg),
            t(postH$q2.5$By_ayu),
            t(postH$q2.5$By_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "B_lo95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$q97.5$By_ayg),
            t(postH$q97.5$By_ayu),
            t(postH$q97.5$By_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "B_hi95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$sd$By_ayg),
            t(postH$sd$By_ayu),
            t(postH$sd$By_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "sd_B") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    mutate(area = toupper(area)) %>%
    mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE),
           category = "Total mortality",
           retro = i,
           retro_name = paste0("retro-",i),
           prop_conv = prop_conv) ->ye_removals
  
  as.data.frame(
    rbind(t(postH$q50$Ry_ayg),
          t(postH$q50$Ry_ayu),
          t(postH$q50$Ry_ay))) %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(start_yr:retro_yr, times = 3),
           user = rep(c("guided", "unguided", "All"), each = Y)) %>%
    pivot_longer(!c(year,user), names_to = "area", values_to = "R") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(as.data.frame(
      rbind(t(postH$q2.5$Ry_ayg),
            t(postH$q2.5$Ry_ayu),
            t(postH$q2.5$Ry_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "R_lo95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$q97.5$Ry_ayg),
            t(postH$q97.5$Ry_ayu),
            t(postH$q97.5$Ry_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "R_hi95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$sd$Ry_ayg),
            t(postH$sd$Ry_ayu),
            t(postH$sd$Ry_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "sd_R") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    mutate(area = toupper(area)) %>%
    mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE),
           category = "Total released",
           retro = i,
           retro_name = paste0("retro-",i),
           prop_conv = prop_conv) ->ye_releases
  
  as.data.frame(
    rbind(t(postH$q50$Hy_ayg),
          t(postH$q50$Hy_ayu),
          t(postH$q50$Hy_ay))) %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(start_yr:retro_yr, times = 3),
           user = rep(c("guided", "unguided", "All"), each = Y)) %>%
    pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(as.data.frame(
      rbind(t(postH$q2.5$Hy_ayg),
            t(postH$q2.5$Hy_ayu),
            t(postH$q2.5$Hy_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "H_lo95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$q97.5$Hy_ayg),
            t(postH$q97.5$Hy_ayu),
            t(postH$q97.5$Hy_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "H_hi95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$sd$Hy_ayg),
            t(postH$sd$Hy_ayu),
            t(postH$sd$Hy_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:retro_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "sd_H") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    mutate(area = toupper(area)) %>%
    mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE),
           category = "Total harvested",
           retro = i,
           retro_name = paste0("retro-",i),
           prop_conv = prop_conv) ->ye_harvests
  
  if (i == 0){
    retro_brf_removals <- brf_removals
    retro_brf_releases <- brf_releases
    retro_brf_harvests <- brf_harvests
    
    retro_ye_removals <- ye_removals
    retro_ye_releases <- ye_releases
    retro_ye_harvests <- ye_harvests
  } else {
    retro_brf_removals <- rbind(retro_brf_removals,brf_removals)
    retro_brf_releases <- rbind(retro_brf_releases,brf_releases)
    retro_brf_harvests <- rbind(retro_brf_harvests,brf_harvests)
    
    retro_ye_removals <- rbind(retro_ye_removals,ye_removals)
    retro_ye_releases <- rbind(retro_ye_releases,ye_releases)
    retro_ye_harvests <- rbind(retro_ye_harvests,ye_harvests)
  }
}

pal = wes_palette("Zissou1", 11, type = "continuous")[11:1]

retro_brf_removals %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user == "guided") %>%
  ggplot(aes(x = year, y = B, color = retro_name, type = user, fill = retro_name)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = B_lo95, ymax = B_hi95), alpha = 0.05, color = NA) +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Black Rockfish Removals (lbs)", x = "Year")

retro_brf_removals %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user == "unguided") %>%
  ggplot(aes(x = year, y = B, color = retro_name, type = user, fill = retro_name)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = B_lo95, ymax = B_hi95), alpha = 0.05, color = NA) +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Black Rockfish Removals (lbs)", x = "Year")

retro_brf_releases %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user == "guided") %>%
  ggplot(aes(x = year, y = R, color = retro_name, type = user, fill = retro_name)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = R_lo95, ymax = R_hi95), alpha = 0.05, color = NA) +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Black Rockfish Releases (n)", x = "Year")

retro_brf_releases %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user == "unguided") %>%
  ggplot(aes(x = year, y = R, color = retro_name, type = user, fill = retro_name)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = R_lo95, ymax = R_hi95), alpha = 0.05, color = NA) +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Black Rockfish Releases (n)", x = "Year")

retro_brf_harvests %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user == "guided") %>%
  ggplot(aes(x = year, y = H, color = retro_name, type = user, fill = retro_name)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.05, color = NA) +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Black Rockfish Harvests (n)", x = "Year")

retro_brf_harvests %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user == "unguided") %>%
  ggplot(aes(x = year, y = H, color = retro_name, type = user, fill = retro_name)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.05, color = NA) +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Black Rockfish Harvests (n)", x = "Year")

retro_ye_removals %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user == "guided") %>%
  ggplot(aes(x = year, y = B, color = retro_name, type = user, fill = retro_name)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = B_lo95, ymax = B_hi95), alpha = 0.05, color = NA) +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Yelloweye Rockfish Removals (lbs)", x = "Year")

retro_ye_removals %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user == "unguided") %>%
  ggplot(aes(x = year, y = B, color = retro_name, type = user, fill = retro_name)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = B_lo95, ymax = B_hi95), alpha = 0.05, color = NA) +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Yelloweye Rockfish Removals (lbs)", x = "Year")

retro_ye_releases %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user == "guided") %>%
  ggplot(aes(x = year, y = R, color = retro_name, type = user, fill = retro_name)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = R_lo95, ymax = R_hi95), alpha = 0.05, color = NA) +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Yelloweye Rockfish Releases (n)", x = "Year")

retro_ye_releases %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user == "unguided") %>%
  ggplot(aes(x = year, y = R, color = retro_name, type = user, fill = retro_name)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = R_lo95, ymax = R_hi95), alpha = 0.05, color = NA) +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Yelloweye Rockfish Releases (n)", x = "Year")

retro_ye_harvests %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user == "guided") %>%
  ggplot(aes(x = year, y = H, color = retro_name, type = user, fill = retro_name)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.05, color = NA) +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Yelloweye Rockfish Harvests (n)", x = "Year")

retro_ye_harvests %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user == "unguided") %>%
  ggplot(aes(x = year, y = H, color = retro_name, type = user, fill = retro_name)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.05, color = NA) +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Yelloweye Rockfish Harvests (n)", x = "Year")

# summarization of retro patterns
retro_exams <- list(retro_brf_harvests,
                 retro_brf_releases,
                 retro_brf_removals,
                 retro_ye_harvests,
                 retro_ye_releases,
                 retro_ye_removals)

cat <- c("harvests","releases","removals",
         "harvests","releases","removals")
varlist <- c("H","R","B","H","R","B")

sp <- c("black","black","black",
        "yelloweye","yelloweye","yelloweye")

for (i in 1:6){ #i <- 2
  re <- retro_exams[[i]]
  #var <- varlist[i]
  if (i %in% c(1,4)) {
    re <- re %>% mutate(var = H)
  }
  if (i %in% c(2,5)) {
    re <- re %>% mutate(var = R)
  }
  if (i %in% c(3,6)) {
    re <- re %>% mutate(var = B)
  }
  re %>% 
    select(year,user,area,var,retro_name) %>%
    pivot_wider(names_from = retro_name,
                values_from = var) %>%
    clean_names() %>%
    mutate(species = sp[i],
           cat = cat[i],
           retro1_pdif = (retro_0 - retro_1) / retro_0,
           retro2_pdif = (retro_0 - retro_2) / retro_0,
           retro3_pdif = (retro_0 - retro_3) / retro_0,
           retro4_pdif = (retro_0 - retro_4) / retro_0,
           retro1_dif = (retro_0 - retro_1) ,
           retro2_dif = (retro_0 - retro_2) ,
           retro3_dif = (retro_0 - retro_3) ,
           retro4_dif = (retro_0 - retro_4) ) %>%
    #dplyr::rowwise() %>%
    mutate(mean_dif = (retro1_dif+retro2_dif+retro3_dif+retro4_dif)/4) -> re
  if (i == 1){
    retro_res <- re
  } else {
    retro_res <- rbind(retro_res,re)
  }
}

pal <- c("blue","darkcyan")

retro_res %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user != "All", species == "black") %>%
  ggplot(aes(x = year, y = mean_dif, color = user, linetype = cat)) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  facet_wrap(. ~ area, scales = "free") +
  geom_line() + 
  geom_hline(yintercept = 0, color = "black") +
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Black Rockfish Deviations from Terminal Estimate (%)", x = "Year")

options(scipen = 999)
View(retro_res %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>% 
  filter(user != "All", species == "black", area == "AFOGNAK"))



















