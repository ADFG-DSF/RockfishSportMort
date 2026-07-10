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

contdat2 <- list2env(readinData_contemporary2(spl_knts = 4,
                                              start_comp_yr = 2020,
                                              start_yr = 2020,
                                              end_yr = end_yr,
                                              b4_start = 2011,
                                              SE06 = "exclude"), #SE06 = "exclude"
                     .GlobalEnv)


rbind(basedat$comp %>% mutate(data = "base"),
      contdat$comp %>% mutate(data = "new"),
      contdat$comp %>% mutate(data = "new2")) %>%
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

datsets <- list(basedat = basedat,
                contdat = contdat,
                condat2 = contdat2)

extract_jags_array <- function(datsets, var, area_names){
  out <- vector("list", length(datsets))
  for(i in seq_along(datsets)){
    tmp <- datsets[[i]]$jags_dat
    out[[i]] <-
      tmp[[var]][, 1:tmp$Y] %>%
      t() %>%
      as.data.frame() %>%
      setNames(area_names) %>%
      mutate(
        year = seq(1977, 2024),
        source = "observed",
        data = names(datsets)[i]
      )
  }
  bind_rows(out)
}

Hhat_ayg <- extract_jags_array(
  datsets,
  "Hhat_ayg",
  unique(H_ayg$area)
)

Hhat_ayg  %>%
  pivot_longer(-c(year, source, data), names_to = "area", values_to = "Hhat_ayg") %>%
  ggplot(aes(x = year, y = Hhat_ayg, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_point()

Hhat_ayg <- extract_jags_array(
  datsets,
  "Hhat_ayg",
  unique(H_ayg$area)
)

Hhat_ayg  %>%
  pivot_longer(-c(year, source, data), names_to = "area", values_to = "Hhat_ayg") %>%
  ggplot(aes(x = year, y = Hhat_ayg, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_point()

prigui_ay <- extract_jags_array(
  datsets,
  "prigui_ay",
  unique(H_ayg$area)
)

prigui_ay  %>%
  pivot_longer(-c(year, source, data), names_to = "area", values_to = "prigui_ay") %>%
  ggplot(aes(x = year, y = prigui_ay, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_point()

cvHhat_ay <- extract_jags_array(
  datsets,
  "cvHhat_ay",
  unique(H_ayg$area)
)

cvHhat_ay  %>%
  pivot_longer(-c(year, source, data), names_to = "area", values_to = "cvHhat_ay") %>%
  ggplot(aes(x = year, y = cvHhat_ay, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_point()

cvHhat_ayg <- extract_jags_array(
  datsets,
  "cvHhat_ayg",
  unique(H_ayg$area)
)

cvHhat_ayg  %>%
  pivot_longer(-c(year, source, data), names_to = "area", values_to = "cvHhat_ayg") %>%
  ggplot(aes(x = year, y = cvHhat_ayg, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_point()


for (i in 1:length(datsets)){ #i <- 1
  tmp <- datsets[[i]]$jags_dat
  yrs <- unique(datsets[[i]]$Hhat_ayg$year)
  
  tmp <- (tmp$Hhat_ayg/tmp$Hhat_ay)[,1:tmp$Y] %>% t() %>%
    as.data.frame() %>%
    setNames(nm = unique(H_ayg$area)) %>%
    mutate(year = seq(1977,2024,1),
           source = "observed",
           data = names(datsets[i]))
  
  if (i == 1){
    pG_obs <- tmp
  } else {
    pG_obs <- rbind(pG_obs,tmp)
  }
}

pG_obs  %>%
  pivot_longer(-c(year, source, data), names_to = "area", values_to = "pG") %>%
  ggplot(aes(x = year, y = pG, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_point()


for (i in 1:length(datsets)){ #i <- 1
  tmp <- datsets[[i]]$jags_dat
  yrs <- unique(datsets[[i]]$Hhat_ayg$year)
  
  cbind(tmp$intp_year,
        tmp$intp_area,
        tmp$intp_user,
        tmp$inth_pel,
        tmp$intc_pel) %>% data.frame() %>%
    setNames(nm = c("year","area","user","H","C")) %>%
    mutate(year = 1976 + year,
           pH = H/C,
           data = names(datsets[i])) -> tmp
  
  if (i == 1){
    intp <- tmp
  } else {
    intp <- rbind(intp,tmp)
  }
}

ggplot(intp %>% filter(user == 1),
       aes(x = year, y = pH, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_line() + geom_point()

ggplot(intp %>% filter(user == 0),
       aes(x = year, y = pH, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_line() + geom_point()

#-------------------------------------------------------------------------------

for (i in 1:length(datsets)){ #i <- 1
  tmp <- datsets[[i]]$jags_dat
  yrs <- unique(datsets[[i]]$Hhat_ayg$year)
  
  cbind(tmp$inty_year,
        tmp$inty_area,
        tmp$inty_user,
        tmp$inth_ye,
        tmp$intc_ye) %>% data.frame() %>%
    setNames(nm = c("year","area","user","H","C")) %>%
    mutate(year = 1976 + year,
           pH = H/C,
           data = names(datsets[i])) -> tmp
  
  if (i == 1){
    inty <- tmp
  } else {
    inty <- rbind(inty,tmp)
  }
}

ggplot(inty %>% filter(user == 1),
       aes(x = year, y = pH, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_line() + geom_point()

ggplot(inty %>% filter(user == 0),
       aes(x = year, y = pH, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_line() + geom_point()

#-------------------------------------------------------------------------------
for (i in 1:length(datsets)){ #i <- 1
  tmp <- datsets[[i]]$jags_dat
  yrs <- unique(datsets[[i]]$Hhat_ayg$year)
  
  cbind(tmp$into_year,
        tmp$into_area,
        tmp$into_user,
        tmp$inth_other,
        tmp$intc_other) %>% data.frame() %>%
    setNames(nm = c("year","area","user","H","C")) %>%
    mutate(year = 1976 + year,
           pH = H/C,
           data = names(datsets[i])) -> tmp
  
  if (i == 1){
    into <- tmp
  } else {
    into <- rbind(into,tmp)
  }
}

ggplot(into %>% filter(user == 1),
       aes(x = year, y = pH, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_line() + geom_point()

ggplot(into %>% filter(user == 0),
       aes(x = year, y = pH, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_line() + geom_point()

into %>% filter(data == "condat2" & area == 1)

#-------------------------------------------------------------------------------
for (i in 1:length(datsets)){ #i <- 1
  tmp <- datsets[[i]]$jags_dat
  yrs <- unique(datsets[[i]]$Hhat_ayg$year)
  
  cbind(tmp$intd_year,
        tmp$intd_area,
        tmp$intd_user,
        tmp$inth_dsr,
        tmp$intc_dsr) %>% data.frame() %>%
    setNames(nm = c("year","area","user","H","C")) %>%
    mutate(year = 1976 + year,
           pH = H/C,
           data = names(datsets[i])) -> tmp
  
  if (i == 1){
    intd <- tmp
  } else {
    intd <- rbind(intd,tmp)
  }
}

ggplot(intd %>% filter(user == 1),
       aes(x = year, y = pH, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_line() + geom_point()

ggplot(intd %>% filter(user == 0),
       aes(x = year, y = pH, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_line() + geom_point()

into %>% filter(data == "condat2" & area == 1)

#-------------------------------------------------------------------------------
for (i in 1:length(datsets)){ #i <- 1
  tmp <- datsets[[i]]$jags_dat
  yrs <- unique(datsets[[i]]$Hhat_ayg$year)
  
  cbind(tmp$ints_year,
        tmp$ints_area,
        tmp$ints_user,
        tmp$inth_slope,
        tmp$intc_slope) %>% data.frame() %>%
    setNames(nm = c("year","area","user","H","C")) %>%
    mutate(year = 1976 + year,
           pH = H/C,
           data = names(datsets[i])) -> tmp
  
  if (i == 1){
    ints <- tmp
  } else {
    ints <- rbind(ints,tmp)
  }
}

ggplot(ints %>% filter(user == 1),
       aes(x = year, y = pH, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_line() + geom_point()

ggplot(ints %>% filter(user == 0),
       aes(x = year, y = pH, col = data, shape = data)) +
  facet_wrap(~area) +
  geom_line() + geom_point()

into %>% filter(data == "condat2" & area == 1)

################################################################################

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



























