###############################################################################
# Random Effects Check
#
# Code to check if the random effects in the Bayes model are working correctly
#
###############################################################################
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
end_yr <- 2024

#most recent Howard estimates: 
REP_YR <- 2024 #for bringing in Howard estimats

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

results <- "Gen4int_indcomp_swhsR_FULL_pHB4pars_thru2024_3e+05_2025-10-15"

postH <- readRDS(paste0(".\\output\\bayes_posts\\",results,".rds"))

#-------------------------------------------------------------------------------
# Examine results!

rhat <- get_Rhat(postH, cutoff = 1.01)
names(rhat)[1] <- "Rhat_values"

all_rhat <- get_Rhat(postH,cutoff = 0.01)
names(all_rhat)[1] <- "Rhat_values"
as.vector(all_rhat$Rhat_values) %>% data.frame()-> rhat_vals
prop_conv <- round(nrow(rhat_vals %>% filter(Rhat <= 1.0115))/nrow(rhat_vals),4); prop_conv

#------------------------------------------------------------------------------
#re_pH[a,y,u,s]
pel_re_pH <- 
  rbind(postH$mean$re_pH[,,1,1] %>% t(),
        postH$mean$re_pH[,,2,1] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "re_pH")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))

ggplot(pel_re_pH,
       aes(x = year,y = re_pH, colour = user)) +
  geom_line() +
  facet_wrap(~area) +
  geom_hline(yintercept = 0, col = "black") +
  theme_bw()

print(pel_re_pH %>% group_by(user,area) %>%
  summarise(re_sum = sum(re_pH)), n = 50)

pHye_mod <- 
  rbind(postH$mean$re_pH[,,1,2] %>% t(),
        postH$mean$re_pH[,,2,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "re_pH")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))

ggplot(pHye_mod,
       aes(x = year,y = re_pH, colour = user)) +
  geom_line() +
  facet_wrap(~area) +
  geom_hline(yintercept = 0, col = "black") +
  theme_bw()

print(pHye_mod %>% group_by(user,area) %>%
        summarise(re_sum = sum(re_pH)), n = 50)


pHother_mod <- 
  rbind(postH$mean$re_pH[,,1,3] %>% t(),
        postH$mean$re_pH[,,2,3] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "re_pH")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))

ggplot(pHother_mod,
       aes(x = year,y = re_pH, colour = user)) +
  geom_line() +
  facet_wrap(~area) +
  geom_hline(yintercept = 0, col = "black") +
  theme_bw()

print(pHother_mod %>% group_by(user,area) %>%
        summarise(re_sum = sum(re_pH)), n = 50)


pHdsr_mod <- 
  rbind(postH$mean$re_pH[,,1,4] %>% t(),
        postH$mean$re_pH[,,2,4] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "re_pH")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))

ggplot(pHdsr_mod,
       aes(x = year,y = re_pH, colour = user)) +
  geom_line() +
  facet_wrap(~area) +
  geom_hline(yintercept = 0, col = "black") +
  theme_bw()

print(pHdsr_mod %>% group_by(user,area) %>%
        summarise(re_sum = sum(re_pH)), n = 50)


pHslope_mod <- 
  rbind(postH$mean$re_pH[,,1,5] %>% t(),
        postH$mean$re_pH[,,2,5] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "re_pH")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))

ggplot(pHslope_mod,
       aes(x = year,y = re_pH, colour = user)) +
  geom_line() +
  facet_wrap(~area) +
  geom_hline(yintercept = 0, col = "black") +
  theme_bw()

print(pHdsr_mod %>% group_by(user,area) %>%
        summarise(re_sum = sum(re_pH)), n = 50)

print(pHdsr_mod %>% group_by(area) %>%
        summarise(re_sum = sum(re_pH)), n = 50)

#-------------------------------------------------------------------------------
#re_pelagic [a,y,u]
p_pel <- 
  rbind(postH$mean$re_pelagic[,,1] %>% t(),
        postH$mean$re_pelagic[,,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "re_p_pel")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))

ggplot(p_pel,
       aes(x = year,y = re_p_pel, colour = user)) +
  geom_line() +
  facet_wrap(~area) +
  geom_hline(yintercept = 0, col = "black") +
  theme_bw()

print(p_pel %>% group_by(user,area) %>%
        summarise(re_sum = sum(re_p_pel)), n = 50)

#-------------------------------------------------------------------------------
#re_pelagic [a,y,u]
p_ye <- 
  rbind(postH$mean$re_yellow[,,1] %>% t(),
        postH$mean$re_yellow[,,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "re_p_ye")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))

ggplot(p_ye,
       aes(x = year,y = re_p_ye, colour = user)) +
  geom_line() +
  facet_wrap(~area) +
  geom_hline(yintercept = 0, col = "black") +
  theme_bw()

print(p_ye %>% group_by(user,area) %>%
        summarise(re_sum = sum(re_p_ye)), n = 50)

#-------------------------------------------------------------------------------
#re_black [a,y,u]
p_black <- 
  rbind(postH$mean$re_black[,,1] %>% t(),
        postH$mean$re_black[,,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "re_p_black")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))

ggplot(p_black,
       aes(x = year,y = re_p_black, colour = user)) +
  geom_line() +
  facet_wrap(~area) +
  geom_hline(yintercept = 0, col = "black") +
  theme_bw()

print(p_black %>% group_by(user,area) %>%
        summarise(re_sum = sum(re_p_black)), n = 50)


#-------------------------------------------------------------------------------
#re_black [a,y,u]
p_dsr <- 
  rbind(postH$mean$re_dsr[,,1] %>% t(),
        postH$mean$re_dsr[,,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "re_p_dsr")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))

ggplot(p_dsr,
       aes(x = year,y = re_p_dsr, colour = user)) +
  geom_line() +
  facet_wrap(~area) +
  geom_hline(yintercept = 0, col = "black") +
  theme_bw()

print(p_dsr %>% group_by(user,area) %>%
        summarise(re_sum = sum(re_p_dsr)), n = 50)


#-------------------------------------------------------------------------------
#re_black [a,y,u]
p_slope <- 
  rbind(postH$mean$re_slope[,,1] %>% t(),
        postH$mean$re_slope[,,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "re_p_slope")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))

ggplot(p_slope,
       aes(x = year,y = re_p_slope, colour = user)) +
  geom_line() +
  facet_wrap(~area) +
  geom_hline(yintercept = 0, col = "black") +
  theme_bw()

print(p_slope %>% group_by(user,area) %>%
        summarise(re_sum = sum(re_p_slope)), n = 50)








































