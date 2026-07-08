################################################################################
# ROCKFISH HARVEST AND RELEASE ESTIMATION WITH REIMER / BAYESIAN METHODS
#
# This code allows for comparisons between contemporary models (> 2019) and 
# historical reconstruction models
#
# Developed by  Phil Joy (philip.joy@alaska.gov)
#
# Lats updated: July 2026
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

#functions, including data and parameter specifications
source(".\\scripts//bayes_data_param_load.R")

#year to run model through

end_yr <- 2024
start_yr <- 1977

#most recent Howard estimates: 
REP_YR <- 2024 #for bringing in Howard estimats

#load the data:
list2env(readinData_alt(spl_knts = 7,
                        start_yr = 1977,
                        end_yr = end_yr,
                        SE06 = "exclude"), #SE06 = "exclude"
         .GlobalEnv)

#load parameters
params <- jags_params()

#area codes
area_codes <- comp %>% select(area,area_n) %>% unique() %>%
  add_row(area = "BSAI", area_n = 5) %>%
  add_row(area = "SOKO2SAP", area_n = 6) %>%
  add_row(area = "WKMA", area_n = 7) %>%
  mutate(area_n = as.character(area_n)) %>% arrange(as.numeric(area_n))

set.seed(8645)

histresults <- "Gen4int_indcomp_swhsR_FULL_pHB4pars_re0full_altwt_2xcvSEo_finaltuning7_thru2024_2e+06_SE06ex_2026-06-29"

contresults1 <- "annual_est_take5_thru2024_1e+05__2026-07-07"
contresults2 <- "annual_est_take5.1_thru2024_40000_tst_2026-07-07"
contresults3 <- "annual_est_take5.1.1.a_thru2024_10000_tst_2026-07-07"
contresults4 <- "annual_est_take5.1.1.a2_thru2024_10000_tst_2026-07-07"

Historical <- readRDS(paste0(".\\output\\bayes_posts\\",histresults,".rds"))

Contemporary_a1 <- readRDS(paste0(".\\output\\bayes_posts\\",contresults1,".rds"))
Contemporary_a2 <- readRDS(paste0(".\\output\\bayes_posts\\",contresults2,".rds"))
Contemporary_a3 <- readRDS(paste0(".\\output\\bayes_posts\\",contresults3,".rds"))
Contemporary_a4 <- readRDS(paste0(".\\output\\bayes_posts\\",contresults4,".rds"))

modlist <- list(Historical=Historical,
                Contemporary_a1 = Contemporary_a1,
                Contemporary_a2 = Contemporary_a2,
                Contemporary_a3 = Contemporary_a3,
                Contemporary_a4 = Contemporary_a4)

#-------------------------------------------------------------------------------
# Compare all harvests

for (i in 1:length(modlist)){ #i <- 1
  postH <- modlist[[i]]
  
  #start_yr <- ifelse(names(modlist[i])== "Historical",1977,2020)
  
#  if (names(modlist[i])== "Historical"){
#    list2env(readinData_alt(spl_knts = 7,
#                                     start_yr = start_yr,
#                                     end_yr = end_yr,
#                                     SE06 = "exclude"), #SE06 = "exclude"
#             .GlobalEnv)
#  } 
  
#  if (names(modlist[i])== "Contemporary_a1"){
#    list2env(readinData_alt(spl_knts = 7,
#                            start_yr = start_yr,
#                            end_yr = end_yr,
#                            SE06 = "exclude"), #SE06 = "exclude"
#             .GlobalEnv)
#  }
  
#  if (names(modlist[i])== "Contemporary_a2"){
#    list2env(readinData_alt(spl_knts = 4,
#                            start_yr = start_yr,
#                            end_yr = end_yr,
#                            SE06 = "exclude"), #SE06 = "exclude"
#             .GlobalEnv)
#  }
  
  as.data.frame(
    rbind(t(postH$q50$H_ayg),
          t(postH$q50$H_ayu),
          t(postH$q50$H_ay))) %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(start_yr:end_yr, times = 3),
           user = rep(c("guided", "unguided", "All"), each = Y)) %>%
    pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(as.data.frame(
      rbind(t(postH$q2.5$H_ayg),
            t(postH$q2.5$H_ayu),
            t(postH$q2.5$H_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:end_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "H_lo95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$q97.5$H_ayg),
            t(postH$q97.5$H_ayu),
            t(postH$q97.5$H_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:end_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "H_hi95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$sd$H_ayg),
            t(postH$sd$H_ayu),
            t(postH$sd$H_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:end_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "sd_H") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    mutate(area = toupper(as.character(area))) %>% #filter(!is.na(area)) %>%
    mutate(model = names(modlist[i])) -> temp
  
  unique(temp$area)
  
  if (i == 1){
    rf_plotdat = temp
  } else {
    rf_plotdat = rbind(rf_plotdat,temp)
  }
  
  as.data.frame(
    rbind(t(postH$q50$Hb_ayg),
          t(postH$q50$Hb_ayu),
          t(postH$q50$Hb_ay))) %>%
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
    left_join(as.data.frame(
      rbind(t(postH$sd$Hb_ayg),
            t(postH$sd$Hb_ayu),
            t(postH$sd$Hb_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:end_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "sd_H") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    mutate(area = toupper(as.character(area))) %>% #filter(!is.na(area)) %>%
    mutate(model = names(modlist[i])) -> brftemp
  
  unique(brftemp$area)
  
 # brftemp %>% filter(model == "Contemporary_a2",
#                         year == "2020",
#                         area == "CI")
  
  if (i == 1){
    brf_plotdat = brftemp
  } else {
    brf_plotdat = rbind(brf_plotdat,brftemp)
  }
  
  as.data.frame(
    rbind(t(postH$q50$Hy_ayg),
          t(postH$q50$Hy_ayu),
          t(postH$q50$Hy_ay))) %>%
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
    left_join(as.data.frame(
      rbind(t(postH$sd$Hy_ayg),
            t(postH$sd$Hy_ayu),
            t(postH$sd$Hy_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:end_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "sd_H") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    mutate(area = toupper(as.character(area))) %>% #filter(!is.na(area)) %>%
    mutate(model = names(modlist[i])) -> yetemp
  
  unique(yetemp$area)
  
  if (i == 1){
    ye_plotdat = yetemp
  } else {
    ye_plotdat = rbind(ye_plotdat,yetemp)
  }
  
  as.data.frame(
    rbind(t(postH$q50$Hd_ayg),
          t(postH$q50$Hd_ayu),
          t(postH$q50$Hd_ay))) %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(start_yr:end_yr, times = 3),
           user = rep(c("guided", "unguided", "All"), each = Y)) %>%
    pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(as.data.frame(
      rbind(t(postH$q2.5$Hd_ayg),
            t(postH$q2.5$Hd_ayu),
            t(postH$q2.5$Hd_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:end_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "H_lo95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$q97.5$Hd_ayg),
            t(postH$q97.5$Hd_ayu),
            t(postH$q97.5$Hd_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:end_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "H_hi95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$sd$Hd_ayg),
            t(postH$sd$Hd_ayu),
            t(postH$sd$Hd_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:end_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "sd_H") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    mutate(area = toupper(as.character(area))) %>% #filter(!is.na(area)) %>%
    mutate(model = names(modlist[i])) -> dsrtemp
  
  if (i == 1){
    dsr_plotdat = dsrtemp
  } else {
    dsr_plotdat = rbind(dsr_plotdat,dsrtemp)
  }
  
  as.data.frame(
    rbind(t(postH$q50$Hs_ayg),
          t(postH$q50$Hs_ayu),
          t(postH$q50$Hs_ay))) %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(start_yr:end_yr, times = 3),
           user = rep(c("guided", "unguided", "All"), each = Y)) %>%
    pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(as.data.frame(
      rbind(t(postH$q2.5$Hs_ayg),
            t(postH$q2.5$Hs_ayu),
            t(postH$q2.5$Hs_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:end_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "H_lo95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$q97.5$Hs_ayg),
            t(postH$q97.5$Hs_ayu),
            t(postH$q97.5$Hs_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:end_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "H_hi95") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    left_join(as.data.frame(
      rbind(t(postH$sd$Hs_ayg),
            t(postH$sd$Hs_ayu),
            t(postH$sd$Hs_ay))) %>%
        setNames(nm = unique(H_ayg$area)) %>%
        mutate(year = rep(start_yr:end_yr, times = 3),
               user = rep(c("guided", "unguided", "All"), each = Y)) %>%
        pivot_longer(!c(year,user), names_to = "area", values_to = "sd_H") %>%
        mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
      by = c("year","user","area")) %>%
    mutate(area = toupper(as.character(area))) %>% #filter(!is.na(area)) %>%
    mutate(model = names(modlist[i])) -> sltemp
  
  if (i == 1){
    sl_plotdat = sltemp
  } else {
    sl_plotdat = rbind(sl_plotdat,sltemp)
  }
  
  pHpel_mod_tmp <- 
    rbind(postH$mean$pH[,,1,1] %>% t(),
          postH$mean$pH[,,2,1] %>% t()) %>%
    as.data.frame() %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(unique(Hhat_ay$year), times = 2),
           user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
           source = "model") %>%
    pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH")  %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(rbind(postH$q2.5$pH[,,1,1] %>% t(),
                    postH$q2.5$pH[,,2,1] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
                       source = "model") %>%
                pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_lo95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user","source")) %>%
    left_join(rbind(postH$q97.5$pH[,,1,1] %>% t(),
                    postH$q97.5$pH[,,2,1] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
                       source = "model") %>%
                pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_hi95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user","source")) %>%
    mutate(model = names(modlist[i]))
  
  unique(pHpel_mod_tmp$area)
  
  if (i == 1){
    pHpel_mod = pHpel_mod_tmp
  } else {
    pHpel_mod = rbind(pHpel_mod,pHpel_mod_tmp)
  }
  
  pHye_mod_temp <- 
    rbind(postH$mean$pH[,,1,2] %>% t(),
          postH$mean$pH[,,2,2] %>% t()) %>%
    as.data.frame() %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(unique(Hhat_ay$year), times = 2),
           user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
           source = "model") %>%
    pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH")  %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(rbind(postH$q2.5$pH[,,1,2] %>% t(),
                    postH$q2.5$pH[,,2,2] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
                       source = "model") %>%
                pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_lo95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user","source")) %>%
    left_join(rbind(postH$q97.5$pH[,,1,2] %>% t(),
                    postH$q97.5$pH[,,2,2] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
                       source = "model") %>%
                pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_hi95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user","source")) %>%
    mutate(model = names(modlist[i]))
  
  unique(pHye_mod_temp$area)
  
  if (i == 1){
    pHye_mod = pHye_mod_temp
  } else {
    pHye_mod = rbind(pHye_mod,pHye_mod_temp)
  }
  
  pHnpny_mod_tmp <- 
    rbind(postH$mean$pH[,,1,3] %>% t(),
          postH$mean$pH[,,2,3] %>% t()) %>%
    as.data.frame() %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(unique(Hhat_ay$year), times = 2),
           user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
           source = "model") %>%
    pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH")  %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(rbind(postH$q2.5$pH[,,1,3] %>% t(),
                    postH$q2.5$pH[,,2,3] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
                       source = "model") %>%
                pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_lo95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user","source")) %>%
    left_join(rbind(postH$q97.5$pH[,,1,3] %>% t(),
                    postH$q97.5$pH[,,2,3] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
                       source = "model") %>%
                pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_hi95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user","source")) %>%
    mutate(model = names(modlist[i]))
  
  unique(pHye_mod_temp$area)
  
  if (i == 1){
    pHnpny_mod = pHnpny_mod_tmp
  } else {
    pHnpny_mod = rbind(pHnpny_mod,pHnpny_mod_tmp)
  }
  
  pHdsr_mod_tmp <- 
    rbind(postH$mean$pH[,,1,4] %>% t(),
          postH$mean$pH[,,2,4] %>% t()) %>%
    as.data.frame() %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(unique(Hhat_ay$year), times = 2),
           user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
           source = "model") %>%
    pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH")  %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(rbind(postH$q2.5$pH[,,1,4] %>% t(),
                    postH$q2.5$pH[,,2,4] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
                       source = "model") %>%
                pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_lo95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user","source")) %>%
    left_join(rbind(postH$q97.5$pH[,,1,4] %>% t(),
                    postH$q97.5$pH[,,2,4] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
                       source = "model") %>%
                pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_hi95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user","source")) %>%
    filter(area %in% c("CSEO","EWYKT","NSEI","NSEO","SSEI","SSEO")) %>%
    mutate(model = names(modlist[i]))
  
  unique(pHye_mod_temp$area)
  
  if (i == 1){
    pHdsr_mod = pHdsr_mod_tmp
  } else {
    pHdsr_mod = rbind(pHdsr_mod,pHdsr_mod_tmp)
  }
  
  pHslope_mod_tmp <- 
    rbind(postH$mean$pH[,,1,5] %>% t(),
          postH$mean$pH[,,2,5] %>% t()) %>%
    as.data.frame() %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(unique(Hhat_ay$year), times = 2),
           user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
           source = "model") %>%
    pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH")  %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(rbind(postH$q2.5$pH[,,1,5] %>% t(),
                    postH$q2.5$pH[,,2,5] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
                       source = "model") %>%
                pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_lo95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user","source")) %>%
    left_join(rbind(postH$q97.5$pH[,,1,5] %>% t(),
                    postH$q97.5$pH[,,2,5] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
                       source = "model") %>%
                pivot_longer(-c(year, user,source), names_to = "area", values_to = "p_hi95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user","source")) %>%
    filter(area %in% c("CSEO","EWYKT","NSEI","NSEO","SSEI","SSEO")) %>%
    mutate(model = names(modlist[i]))
  
  unique(pHye_mod_temp$area)
  
  if (i == 1){
    pHslope_mod = pHslope_mod_tmp
  } else {
    pHslope_mod = rbind(pHslope_mod,pHslope_mod_tmp)
  }
  
  p_pelagic_tmp <- 
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
              by = c("year","area","user")) %>%
    mutate(model = names(modlist[i]))
  
  unique(p_pelagic_tmp$area)
  
  if (i == 1){
    p_pelagic = p_pelagic_tmp
  } else {
    p_pelagic = rbind(p_pelagic,p_pelagic_tmp)
  }
  
  p_black_tmp <- 
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
              by = c("year","area","user")) %>%
    mutate(model = names(modlist[i]))
  
  unique(p_pelagic_tmp$area)
  
  if (i == 1){
    p_black = p_black_tmp
  } else {
    p_black = rbind(p_black,p_black_tmp)
  }
  
  p_yellow_tmp <- 
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
              by = c("year","area","user")) %>%
    mutate(model = names(modlist[i]))
  
  unique(p_pelagic_tmp$area)
  
  if (i == 1){
    p_yellow = p_yellow_tmp
  } else {
    p_yellow = rbind(p_yellow,p_yellow_tmp)
  }
  
  p_dsr_tmp <- 
    rbind(postH$mean$p_dsr[,,1] %>% t(),
          postH$mean$p_dsr[,,2] %>% t()) %>%
    as.data.frame() %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(unique(Hhat_ay$year), times = 2),
           user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
           source = "model") %>%
    pivot_longer(-c(year, user, source), names_to = "area", values_to = "p_dsr")  %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(rbind(postH$q2.5$p_dsr[,,1] %>% t(),
                    postH$q2.5$p_dsr[,,2] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
                pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user")) %>%
    left_join(rbind(postH$q97.5$p_dsr[,,1] %>% t(),
                    postH$q97.5$p_dsr[,,2] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
                pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user")) %>%
    left_join(rbind(postH$q50$p_dsr[,,1] %>% t(),
                    postH$q50$p_dsr[,,2] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
                pivot_longer(-c(year, user), names_to = "area", values_to = "med_p")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user")) %>%
    mutate(model = names(modlist[i]))
  
  if (i == 1){
    p_dsr = p_dsr_tmp
  } else {
    p_dsr = rbind(p_dsr,p_dsr_tmp)
  }
  
  p_slope_tmp <- 
    rbind(postH$mean$p_slope[,,1] %>% t(),
          postH$mean$p_slope[,,2] %>% t()) %>%
    as.data.frame() %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = rep(unique(Hhat_ay$year), times = 2),
           user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
           source = "model") %>%
    pivot_longer(-c(year, user, source), names_to = "area", values_to = "p_slope")  %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
    left_join(rbind(postH$q2.5$p_slope[,,1] %>% t(),
                    postH$q2.5$p_slope[,,2] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
                pivot_longer(-c(year, user), names_to = "area", values_to = "p_lo95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user")) %>%
    left_join(rbind(postH$q97.5$p_slope[,,1] %>% t(),
                    postH$q97.5$p_slope[,,2] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
                pivot_longer(-c(year, user), names_to = "area", values_to = "p_hi95")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user")) %>%
    left_join(rbind(postH$q50$p_slope[,,1] %>% t(),
                    postH$q50$p_slope[,,2] %>% t()) %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = rep(unique(Hhat_ay$year), times = 2),
                       user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
                pivot_longer(-c(year, user), names_to = "area", values_to = "med_p")  %>%
                mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
              by = c("year","area","user")) %>%
    mutate(model = names(modlist[i]))
  
  if (i == 1){
    p_slope = p_slope_tmp
  } else {
    p_slope = rbind(p_slope,p_slope_tmp)
  }
  
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
              by = c("year","source","area")) %>%
    mutate(model = names(modlist[i]))
  
  if (i == 1){
    pG = pG_mod
  } else {
    pG = rbind(pG,pG_mod)
  }
  
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
    rbind(exp(postH$q50$logbc_R) %>%
            t() %>%
            as.data.frame() %>%
            setNames(nm = unique(H_ayg$area)) %>%
            mutate(year = unique(Hhat_ay$year),
                   source = "model") %>%
            pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
            mutate(data = "R")) %>%
    left_join(exp(postH$q2.5$logbc_H) %>%
                t() %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = unique(Hhat_ay$year),
                       source = "model") %>%
                pivot_longer(-c(year, source), names_to = "area", values_to = "bc_lo95") %>%
                mutate(data = "H") %>%
                rbind(exp(postH$q2.5$logbc_R) %>%
                        t() %>%
                        as.data.frame() %>%
                        setNames(nm = unique(H_ayg$area)) %>%
                        mutate(year = unique(Hhat_ay$year),
                               source = "model") %>%
                        pivot_longer(-c(year, source), names_to = "area", values_to = "bc_lo95") %>%
                        mutate(data = "R")),
              by = c("year","area","data","source")) %>%
    left_join(exp(postH$q97.5$logbc_H) %>%
                t() %>%
                as.data.frame() %>%
                setNames(nm = unique(H_ayg$area)) %>%
                mutate(year = unique(Hhat_ay$year),
                       source = "model") %>%
                pivot_longer(-c(year, source), names_to = "area", values_to = "bc_hi95") %>%
                mutate(data = "H") %>%
                rbind(exp(postH$q97.5$logbc_R) %>%
                        t() %>%
                        as.data.frame() %>%
                        setNames(nm = unique(H_ayg$area)) %>%
                        mutate(year = unique(Hhat_ay$year),
                               source = "model") %>%
                        pivot_longer(-c(year, source), names_to = "area", values_to = "bc_hi95") %>%
                        mutate(data = "R")),
              by = c("year","area","data","source")) %>%
    mutate(model = names(modlist[i]))
  
  if (i == 1){
    bias_ests = bc_mod2
  } else {
    bias_ests = rbind(bias_ests,bc_mod2)
  }
}
i

unique(rf_plotdat$model)

rf_plotdat %>% filter(model == "Contemporary_a2")

rf_plotdat %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "guided",
         #model != "Contemporary_a1",
         year > 2010) %>%
  ggplot(aes(x = year, y = H, color = model, fill = model)) +
#  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(shape = 13) + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Rockfish Harvests (numbers)", x = "Year")

rf_plotdat %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "unguided",
         year > 2010) %>%
  ggplot(aes(x = year, y = H, color = model, fill = model)) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(shape = 13) + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Rockfish Harvests (numbers)", x = "Year")

brf_plotdat %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "unguided",
         year > 2010) %>%
  ggplot(aes(x = year, y = H, color = model, fill = model)) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(shape = 13) + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Rockfish Harvests (numbers)", x = "Year")

ye_plotdat %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "unguided",
         year > 2010) %>%
  ggplot(aes(x = year, y = H, color = model, fill = model)) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(shape = 13) + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Rockfish Harvests (numbers)", x = "Year")

dsr_plotdat %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "unguided",
         year > 2010) %>%
  ggplot(aes(x = year, y = H, color = model, fill = model)) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(shape = 13) + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Rockfish Harvests (numbers)", x = "Year")

sl_plotdat %>% mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "unguided",
         year > 2010) %>%
  ggplot(aes(x = year, y = H, color = model, fill = model)) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(shape = 13) + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Rockfish Harvests (numbers)", x = "Year")

brf_plotdat %>% filter(model == "Contemporary_a2",
                      year == "2020",
                      area == "CI")


pHpel_mod %>% filter(is.na(area))
unique(pHpel_mod$area)

pHpel_mod %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  ggplot(aes(x = year, y = pH, color = model, fill = model)) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(shape = 13) + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

pHye_mod %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  ggplot(aes(x = year, y = pH, color = model, fill = model, shape = model)) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

pHye_mod %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "charter",
         year > 2010) %>%
  ggplot(aes(x = year, y = pH, color = model, fill = model, shape = model)) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

pHye_mod %>% filter(model == "Contemporary_a2",
                       year == "2020",
                       area == "CI")


pHnpny_mod %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  ggplot(aes(x = year, y = pH, color = model, fill = model, shape = model)) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

pHdsr_mod %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
#  filter(user == "private",
#  year > 2010) %>%
  ggplot(aes(x = year, y = pH, color = model, fill = model, shape = model)) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area+user, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

pHslope_mod %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
#  filter(user == "private",
#  year > 2010) %>%
  ggplot(aes(x = year, y = pH, color = model, fill = model, shape = model)) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area + user, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

p_pelagic
p_yellow
p_black
p_dsr
p_slope

p_pelagic %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  ggplot(aes(x = year, y = p_pelagic, color = model, fill = model, shape = model)) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

p_black %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  ggplot(aes(x = year, y = p_black, color = model, fill = model, shape = model)) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

p_yellow %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  ggplot(aes(x = year, y = p_yellow, color = model, fill = model, shape = model)) +
  #  facet_wrap(~user) + 
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

p_dsr %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  ggplot(aes(x = year, y = p_dsr, color = model, fill = model, shape = model)) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

p_slope %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(user == "private",
         year > 2010) %>%
  ggplot(aes(x = year, y = p_slope, color = model, fill = model, shape = model)) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = p_lo95, ymax = p_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

pG %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
#  filter(user == "private") %>%
  ggplot(aes(x = year, y = pG, color = model, fill = model, shape = model)) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = pG_lo95, ymax = pG_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

bias_ests %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(data == "H",
         year > 2010) %>%
  ggplot(aes(x = year, y = bc, color = model, fill = model, shape = model)) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = bc_lo95, ymax = bc_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")

bias_ests %>% #mutate(area = factor(area, toupper(unique(H_ayg$area)), ordered = TRUE)) %>%
  filter(data == "R",
         year > 2010) %>%
  ggplot(aes(x = year, y = bc, color = model, fill = model, shape = model)) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = bc_lo95, ymax = bc_hi95), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point() + 
  theme_bw(base_size = 14) +
  theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(y = "Proportion harvested", x = "Year")



