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

#Cehck data 1: 
basedat <- readinData_alt(spl_knts = 7,
                          start_yr = start_yr,
                          end_yr = end_yr,
                          SE06 = "exclude")
str(basedat)

contdat <- readinData_contemporary(spl_knts = 7,
                                 start_comp_yr = 2020,
                                 start_yr = start_yr,
                                 end_yr = end_yr,
                                 SE06 = "exclude")

basedat$
contdat$comp

rbind(basedat$comp %>% mutate(data = "base"),
      contdat$comp %>% mutate(data = "new")) %>%
  ggplot(aes(x = year, y = N, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_point()

basedat$jags_dat$comp_area
basedat$jags_dat$comp_black / basedat$jags_dat$comp_N
basedat$jags_dat$comp_pelagic
basedat$jags_dat$comp_yellow
basedat$jags_dat$comp_dsr
basedat$jags_dat$comp_slope
basedat$jags_dat$comp_N
basedat$jags_dat$comp_year; contdat$jags_dat$comp_year

basedat$jags_dat$Hhat_ay; contdat$jags_dat$Hhat_ay

basedat <- basedat$jags_dat
contdat <- contdat$jags_dat

list2env(readinData_alt(spl_knts = 7,
                        start_yr = start_yr,
                        end_yr = end_yr,
                        SE06 = "exclude"), #SE06 = "exclude"
         .GlobalEnv)


pG_obs <- 
  (basedat$Hhat_ayg/basedat$Hhat_ay)[,1:basedat$Y] %>% t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = seq(1977,2024,1),
         source = "observed",
         data = "hist") %>%
  rbind((contdat$Hhat_ayg/contdat$Hhat_ay)[,1:contdat$Y] %>% t() %>%
          as.data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = seq(1977,2024,1),
                 source = "observed",
                 data = "cont")) %>%
  pivot_longer(-c(year, source, data), names_to = "area", values_to = "pG") %>%
  ggplot(aes(x = year, y = pG, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_point(); pG_obs


pHpel_obs <- 
  as.data.frame(
    rbind(t(basedat$Hhat_ayg/(basedat$Rhat_ayg + basedat$Hhat_ayg)),
          t(basedat$Hhat_ayu/(basedat$Rhat_ayu + basedat$Hhat_ayu)),
          t(basedat$Hhat_ay/(basedat$Rhat_ay + basedat$Hhat_ay)))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 3),
         user = rep(c("charter", "private","all"), each = length(unique(Hhat_ay$year))),
         source = "SWHS (all RF)") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
         p_lo95 = NA, p_hi95 = NA,
         data = "hist") %>% 
  rbind(t(basedat$Hlbp_ayg/(basedat$Rlbp_ayg + basedat$Hlbp_ayg)) %>%
          data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(unique(Hhat_ay$year), times = 1),
                 user = rep(c("charter"), each = length(unique(Hhat_ay$year))),
                 source = "Logbook") %>%
          pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
                 p_lo95 = NA, p_hi95 = NA,
                 data = "hist")) %>%
  rbind(as.data.frame(
    rbind(t(contdat$Hhat_ayg/(contdat$Rhat_ayg + contdat$Hhat_ayg)),
          t(contdat$Hhat_ayu/(contdat$Rhat_ayu + contdat$Hhat_ayu)),
          t(contdat$Hhat_ay/(contdat$Rhat_ay + contdat$Hhat_ay)))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(unique(Hhat_ay$year), times = 3),
             user = rep(c("charter", "private","all"), each = length(unique(Hhat_ay$year))),
             source = "SWHS (all RF)") %>%
      pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
             p_lo95 = NA, p_hi95 = NA,
             data = "cont")  %>% 
      
      rbind(t(contdat$Hlbp_ayg/(contdat$Rlbp_ayg + contdat$Hlbp_ayg)) %>%
              data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 1),
                     user = rep(c("charter"), each = length(unique(Hhat_ay$year))),
                     source = "Logbook") %>%
              pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
                     p_lo95 = NA, p_hi95 = NA,
                     data = "cont"))) %>%
  filter(user != "all") %>%
  ggplot(aes(x = year, y = pH, col = source, shape = data, fill = user)) +
  facet_wrap(~area + user) +
  geom_point(alpha = 0.4, size = 2) + geom_line(); pHpel_obs

pHye_obs <- 
  as.data.frame(
    rbind(t(basedat$Hhat_ayg/(basedat$Rhat_ayg + basedat$Hhat_ayg)),
          t(basedat$Hhat_ayu/(basedat$Rhat_ayu + basedat$Hhat_ayu)),
          t(basedat$Hhat_ay/(basedat$Rhat_ay + basedat$Hhat_ay)))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 3),
         user = rep(c("charter", "private","all"), each = length(unique(Hhat_ay$year))),
         source = "SWHS (all RF)") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
         p_lo95 = NA, p_hi95 = NA,
         data = "hist") %>% 
  rbind(t(basedat$Hlby_ayg/(basedat$Rlby_ayg + basedat$Hlby_ayg)) %>%
          data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(unique(Hhat_ay$year), times = 1),
                 user = rep(c("charter"), each = length(unique(Hhat_ay$year))),
                 source = "Logbook") %>%
          pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
                 p_lo95 = NA, p_hi95 = NA,
                 data = "hist")) %>%
  rbind(as.data.frame(
    rbind(t(contdat$Hhat_ayg/(contdat$Rhat_ayg + contdat$Hhat_ayg)),
          t(contdat$Hhat_ayu/(contdat$Rhat_ayu + contdat$Hhat_ayu)),
          t(contdat$Hhat_ay/(contdat$Rhat_ay + contdat$Hhat_ay)))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(unique(Hhat_ay$year), times = 3),
             user = rep(c("charter", "private","all"), each = length(unique(Hhat_ay$year))),
             source = "SWHS (all RF)") %>%
      pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
             p_lo95 = NA, p_hi95 = NA,
             data = "cont")  %>% 
      
      rbind(t(contdat$Hlby_ayg/(contdat$Rlby_ayg + contdat$Hlby_ayg)) %>%
              data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 1),
                     user = rep(c("charter"), each = length(unique(Hhat_ay$year))),
                     source = "Logbook") %>%
              pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
                     p_lo95 = NA, p_hi95 = NA,
                     data = "cont"))) %>%
  filter(user != "all") %>%
  ggplot(aes(x = year, y = pH, col = source, shape = data, fill = user)) +
  facet_wrap(~area + user) +
  geom_point(alpha = 0.4, size = 2) + geom_line(); pHye_obs


pHnpnye_obs <- 
  as.data.frame(
    rbind(t(basedat$Hhat_ayg/(basedat$Rhat_ayg + basedat$Hhat_ayg)),
          t(basedat$Hhat_ayu/(basedat$Rhat_ayu + basedat$Hhat_ayu)),
          t(basedat$Hhat_ay/(basedat$Rhat_ay + basedat$Hhat_ay)))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 3),
         user = rep(c("charter", "private","all"), each = length(unique(Hhat_ay$year))),
         source = "SWHS (all RF)") %>%
  pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
         p_lo95 = NA, p_hi95 = NA,
         data = "hist") %>% 
  rbind(t(basedat$Hlbo_ayg/(basedat$Rlbo_ayg + basedat$Hlbo_ayg)) %>%
          data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = rep(unique(Hhat_ay$year), times = 1),
                 user = rep(c("charter"), each = length(unique(Hhat_ay$year))),
                 source = "Logbook") %>%
          pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
                 p_lo95 = NA, p_hi95 = NA,
                 data = "hist")) %>%
  rbind(as.data.frame(
    rbind(t(contdat$Hhat_ayg/(contdat$Rhat_ayg + contdat$Hhat_ayg)),
          t(contdat$Hhat_ayu/(contdat$Rhat_ayu + contdat$Hhat_ayu)),
          t(contdat$Hhat_ay/(contdat$Rhat_ay + contdat$Hhat_ay)))) %>%
      setNames(nm = unique(H_ayg$area)) %>%
      mutate(year = rep(unique(Hhat_ay$year), times = 3),
             user = rep(c("charter", "private","all"), each = length(unique(Hhat_ay$year))),
             source = "SWHS (all RF)") %>%
      pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
      mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
             p_lo95 = NA, p_hi95 = NA,
             data = "cont")  %>% 
      
      rbind(t(contdat$Hlbo_ayg/(contdat$Rlbo_ayg + contdat$Hlbo_ayg)) %>%
              data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = rep(unique(Hhat_ay$year), times = 1),
                     user = rep(c("charter"), each = length(unique(Hhat_ay$year))),
                     source = "Logbook") %>%
              pivot_longer(-c(year, user,source), names_to = "area", values_to = "pH") %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
                     p_lo95 = NA, p_hi95 = NA,
                     data = "cont"))) %>%
  filter(user != "all") %>%
  ggplot(aes(x = year, y = pH, col = source, shape = data, fill = user)) +
  facet_wrap(~area + user) +
  geom_point(alpha = 0.4, size = 2) + geom_line(); pHnpnye_obs



























