################################################################################
# Bayesian data load for JAGS run
#
# This code pulls and loads the data and parameters for running models in 
# run_bayes_mod.R files.
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

#source(".\\scripts//functions.R")

#Penalized spline regression taken from https://bragqut.github.io/2016/05/24/samclifford-splines/
bspline <- function(x, K, bdeg=3, cyclic=FALSE, xl=min(x), xr=max(x)){
  x <- as.matrix(x,ncol=1)
  
  ndx <- K - bdeg
  
  # as outlined in Eilers and Marx (1996)
  dx <- (xr - xl) / ndx
  t <- xl + dx * (-bdeg:(ndx+bdeg))
  T <- (0 * x + 1) %*% t
  X <- x %*% (0 * t + 1)
  P <- (X - T) / dx
  B <- (T <= X) & (X < (T + dx))
  r <- c(2:length(t), 1)
  
  for (k in 1:bdeg){
    B <- (P * B + (k + 1 - P) * B[ ,r]) / k; 
  }
  
  B <- B[,1:(ndx+bdeg)]
  
  if (cyclic == 1){
    for (i in 1:bdeg){
      B[ ,i] <- B[ ,i] + B[ ,K-bdeg+i]    
    }
    B <- B[ , 1:(K-bdeg)]
  }
  
  return(B)
}

makeQ = function(degree, K, epsilon=1e-3){
  x <- diag(K)
  E <- diff(x, differences=degree)
  return( t(E) %*% E + x*epsilon)
} 

#-------------------------------------------------------------------------------
# Function for getting Rhat from posterior
get_Rhat <- function(post, cutoff = 1.1){
  list(
    data.frame("Rhat" = post$summary[, "Rhat"][post$summary[, "Rhat"] > cutoff & !is.na(post$summary[, "Rhat"])]),
    "R^ quantiles" = quantile(post$summary[, "Rhat"], probs = seq(0.9, 1, by = .01), na.rm = TRUE))
}

# Read data --------------------------------------------------------
# Logbook harvests by area, year for guided trips
readinData <- function(spl_knts = 7,
                       start_yr = 1977,
                       end_yr = 2019){
  H_ayg <- readRDS(".//data//bayes_dat//H_ayg.rds") %>% 
    mutate(H_lb = ifelse(H == 0, 1, H))
  
  # Logbook releases by area, year for guided trips
  R_ayg <- readRDS(".//data//bayes_dat//R_ayg.rds") %>% 
    mutate(R_lb = ifelse(R == 0, 1, R),
           Rye = ifelse(year < 2006, NA,Rye))

  # SWHS harvests by area, year and user 
  Hhat_ayu <- 
    readRDS(".//data//bayes_dat//Hhat_ayu.rds")  %>% 
    mutate(Hhat = ifelse(H == 0, 1, H), 
           seH = ifelse(seH == 0, 1, seH)) %>%
    arrange(area, user, year)
  
  Chat_ayu <- 
    readRDS(".//data//bayes_dat//Chat_ayu.rds")  %>% 
    mutate(Chat = ifelse(C == 0, 1, C), 
           seC = ifelse(seC == 0, 1, seC)) %>%
    arrange(area, user, year)
  
  # SWHS harvests by area, year
  Hhat_ay <- 
    readRDS(".//data//bayes_dat//Hhat_ay.rds") %>% 
    rename(Hhat = H) %>%
    mutate(area = as.character(area)) %>%
    bind_rows(Hhat_ayu %>% 
                group_by(region, area, year) %>% 
                summarise(Hhat = sum(H), seH = sqrt(sum(seH^2))) %>%
                mutate(area = as.character(area))) %>%
    arrange(region, area, year) %>%
    mutate(Hhat = ifelse(Hhat == 0, 1, Hhat), 
           seH = ifelse(seH == 0, 1, seH)) %>%
    select(-cv)
  
  Hhat_ay %>% filter(is.na(Hhat))
  #DEV Code; delete once we figure out how to deal with the blanks
  Hhat_ay %>% mutate(Hhat = ifelse(is.na(Hhat),1,Hhat),
                     seH = ifelse(is.na(seH),1,seH)) -> Hhat_ay
  
  
  # SWHS Catch by area, year
  Chat_ay <- 
    readRDS(".//data//bayes_dat//Chat_ay.rds") %>% 
    rename(Chat = C) %>%
    mutate(area = as.character(area)) %>%
    bind_rows(readRDS(".//data//bayes_dat//Chat_ayu.rds") %>% 
                group_by(region, area, year) %>% 
                summarise(Chat = sum(C), seC = sqrt(sum(seC^2))) %>%
                mutate(area = as.character(area))) %>%
    arrange(region, area, year) %>%
    mutate(Chat = ifelse(Chat == 0, 1, Chat), 
           seC = ifelse(seC == 0, 1, seC)) %>%
    select(-cv)
  
  Chat_ay %>% filter(is.na(Chat))
  
  
  # Survey data on catch composition
  S_ayu0 <- 
    readRDS(".//data//bayes_dat//S_ayu.rds") 
  S_ayu <- 
    S_ayu0 %>% mutate(year = as.integer(year)) %>%
    bind_rows(data.frame(area = rep(unique(S_ayu0$area[S_ayu0$region %in% "Southeast"]), each = 4), 
                         year = rep(rep(1996:1997, each = 2), times = 6),
                         user = rep(c("charter", "private"), times = 12),
                         totalrf_n = 0, ye_n = NA, black_n = NA, pelagic_n = NA, nonpel_n = NA, notye_nonpel_n = NA)) %>%
    filter(year >= 1996) %>%
    arrange(user, area, year) 
  
  #When retention of YE is prohibited as n SE AK between 2020 through 2024 we need
  # to censor the survey/creel data
  S_ayu <- S_ayu %>%
    mutate(ye_n = ifelse(region == "Southeast" &
                           year > 2019 & year < 2025,
                         NA,ye_n))
  
  # prep data for model
  H_ayg <- H_ayg %>% filter(year >= start_yr & year <= end_yr)
  
  R_ayg <- R_ayg %>% filter(year >= start_yr & year <= end_yr)
  
  Hhat_ayg <- Hhat_ayu %>% filter(user == "guided" & year >= start_yr & year <= end_yr); unique(Hhat_ayg$area)
  Hhat_ayp <- Hhat_ayu %>% filter(user == "private" & year >= start_yr & year <= end_yr)
  
  Chat_ayg <- Chat_ayu %>% filter(user == "guided" & year >= start_yr & year <= end_yr)
  Chat_ayp <- Chat_ayu %>% filter(user == "private" & year >= start_yr & year <= end_yr)
  
  # Separate out the unknowns in the pre-1996 data
  Hhat_Uy <- Hhat_ay %>% filter(area == "UNKNOWN" & year >= start_yr & year <= end_yr)
  Chat_Uy <- Chat_ay %>% filter(area == "UNKNOWN" & year >= start_yr & year <= end_yr)
  
  Hhat_ay <- Hhat_ay %>% filter(area != "UNKNOWN" & year >= start_yr & year <= end_yr)
  Chat_ay <- Chat_ay %>% filter(area != "UNKNOWN" & year >= start_yr & year <= end_yr)
  
  A = length(unique(Hhat_ay$area))
  Y = length(unique(Hhat_ay$year))
  
  #SPLINE COMPONENTS: 
  C <- spl_knts
  Z <- bspline(1:Y, K = C) #bspline(1:24, K = C)
  
  #COMP DATA
  comp <- S_ayu %>% filter(year >= start_yr & year <= end_yr) %>%
    mutate(area_n = as.numeric(area), 
           user_n = ifelse(user == "charter", 0, 1), 
           year_n = year - (start_yr - 1),  #year - 1995, changed with the addition of the old data...
           #region_n = ifelse()
           source = 1) %>% 
    select(year, year_n, area_n, user_n, source, N = totalrf_n, pelagic = pelagic_n, black = black_n, yellow = ye_n,
           region,area) %>%
    filter(N != 0) %>%
    mutate(yellow = ifelse(N - pelagic == 0, NA, yellow)) #,
  ##         N = ifelse(is.na(yellow),NA,N))
  
  compX <- comp %>% filter(area_n %in% c(11,12,13,14,15,16)) %>%
    mutate(yellow_x = ifelse(region == "Southeast" & year > 2019 & year < 2025,
                             NA,yellow),
           pelagic_x = pelagic,
           N_x = ifelse(region == "Southeast" & year > 2019 & year < 2025,
                        NA,N)) %>%
    filter(!is.na(N_x))
  #comp %>% filter(region == "Southeast") %>% print(n =200)
  #comp %>% print(n =50)
  #range(comp$year_n)
  
  #with(comp, table(area_n, area))
  #with(comp, table(user_n,area))
  #with(S_ayu, table(user,area))
  
  
  matrix_Hhat_ay <- matrix(Hhat_ay$Hhat, nrow = A, ncol = Y, byrow = TRUE)
  #matrix_Hhat_ay[4, 1:5] <- NA  #what's up with this? assuming bad data?
  cvHhat_ay = matrix(Hhat_ay$seH, nrow = A, ncol = Y, byrow = TRUE) /
    matrix(Hhat_ay$Hhat, nrow = A, ncol = Y, byrow = TRUE)
  cvHhat_ay[is.na(cvHhat_ay)] <- 1
  
  
  with(Chat_ay, table(area,year))
  
  matrix_Chat_ay <- cbind(matrix(NA,nrow = A, ncol = Y - length(unique(Chat_ay$year))),
                          matrix(Chat_ay$Chat, nrow = A, ncol = length(unique(Chat_ay$year)), byrow = TRUE))
  
  #matrix_Chat_ay[4, 1:5] <- NA
  cvChat_ay = cbind(matrix(NA,nrow = A, ncol = Y - length(unique(Chat_ay$year))),
                    matrix(Chat_ay$seC, nrow = A, ncol = length(unique(Chat_ay$year)), byrow = TRUE)) /
    matrix_Chat_ay
  # need cv to be 1 when there is no data
  cvChat_ay[is.na(cvChat_ay)] <- 1
  
  dim(matrix_Hhat_ay)
  dim(matrix_Chat_ay)
  
  #Create JAGs data and then bundle it up
  
  jags_dat <- 
    list(
      A = A, Y = Y, C = C,
      #Harvest
      Hhat_ay = matrix_Hhat_ay,
      cvHhat_ay = cvHhat_ay,
      #Catch
      Chat_ay_pH = matrix_Chat_ay,
      Chat_ay_obs = matrix_Chat_ay,
      cvChat_ay = cvChat_ay,
      
      #Logbook data
      #Harvest by species and user: 
      Hlb_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(H_ayg$year))),
                      matrix(H_ayg$H_lb, nrow = A, ncol = length(unique(H_ayg$year)), byrow = TRUE)),
      # logbook pelagic rf harvested by guides
      Hlbp_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(H_ayg$year))),
                       matrix(H_ayg$Hp, nrow = A, ncol = length(unique(H_ayg$year)), byrow = TRUE)),
      # logbook ye rf harvested by guides
      Hlby_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(H_ayg$year))),
                       matrix(H_ayg$Hye, nrow = A, ncol = length(unique(H_ayg$year)), byrow = TRUE)),
      #Releases by species and user: 
      Rlb_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                      matrix(R_ayg$R_lb, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
      Rlb_ayg_bound = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                            matrix(R_ayg$R_lb, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
      Rlb_ayg_trunc = matrix(as.numeric(NA), nrow = A, ncol = Y ),
      # logbook pelagic rf harvested by guides
      Rlbp_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                       matrix(R_ayg$Rp, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
      Rlbp_ayg_bound = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                             matrix(R_ayg$Rp, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
      Rlbp_ayg_trunc = matrix(as.numeric(NA), nrow = A, ncol = Y ),
      # logbook ye rf harvested by guides
      Rlby_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                       matrix(R_ayg$Rye, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
      Rlby_ayg_bound = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                             matrix(R_ayg$Rye, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
      Rlby_ayg_trunc = matrix(as.numeric(NA), nrow = A, ncol = Y ),
      #SWHS DATA:
      # SWHS estimates of rockfish harvests
      Hhat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
                       matrix(Hhat_ayg$Hhat, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)),
      # cv of SWHS estimates
      cvHhat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
                         matrix(Hhat_ayg$seH, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)) / 
        cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
              matrix(Hhat_ayg$Hhat, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)),
      # SWHS estimates of rockfish Catches
      Chat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Chat_ayg$year))),
                       matrix(Chat_ayg$Chat, nrow = A, ncol = length(unique(Chat_ayg$year)), byrow = TRUE)),
      # cv of SWHS estimates
      cvChat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Chat_ayg$year))),
                         matrix(Chat_ayg$seC, nrow = A, ncol = length(unique(Chat_ayg$year)), byrow = TRUE)) / 
        cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Chat_ayg$year))),
              matrix(Chat_ayg$Chat, nrow = A, ncol = length(unique(Chat_ayg$year)), byrow = TRUE)),
      #Spline:
      Z = Z, #spline, same for C and H
      Q = makeQ(2, C), #spline stuff; same for C and H
      zero = rep(0, C), #same for C and H
      #comp data
      comp_area = comp$area_n,
      comp_year = comp$year_n,
      comp_user = comp$user_n,
      comp_N = comp$N, #Nayu
      comp_pelagic = comp$pelagic,
      comp_black = comp$black,
      comp_yellow = comp$yellow,
      N = dim(comp)[1],
      
      comp_pelagic_x = compX$pelagic_x,
      comp_yellow_x = compX$yellow_x,
      comp_N_x = compX$N_x,
      comp_area_x = compX$area_n,
      comp_year_x = compX$year_n,
      comp_user_x = compX$user_n,
      N_x = dim(compX)[1],
      
      regions = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3)
    )
  
  return(list(jags_dat = jags_dat,
              H_ayg = H_ayg,
              R_ayg = R_ayg,
              Hhat_ayu = Hhat_ayu,
              Chat_ayu = Chat_ayu,
              Hhat_ayg = Hhat_ayg,
              Hhat_ay = Hhat_ay,
              Chat_ay = Chat_ay,
              S_ayu = S_ayu,
              comp = comp,
              compX = compX,
              Y = Y, A = A))
}

#-------------------------------------------------------------------------------
# function for loading parameters
jags_params <- function(){
  params <- c(#SWHS bias; assumed same for C and H
    #"logbc", "mu_bc", "sd_bc",
    #SWHS bias, separate C and H
    "logbc_H", "mu_bc_H", "sd_bc_H",
    #"logbc_C", "mu_bc_C", "sd_bc_C",
    "logbc_C","bc_C_offset","mu_bc_C_offset","sd_bcCoff",
    #User proportions (proportion guided); different for H and C
    "pG_H", "b1_pG_H", "b2_pG_H",
    "pG_C", "b1_pG_C", "b2_pG_C",
    #proportion harvested: 
    "pH", 
    #liner:
    #  "pH_int", "pH_slo",
    #polynomial
    "mu_beta0_pH","tau_beta0_pH","beta0_pH","beta1_pH","beta2_pH","beta3_pH",
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
    "mu_lambda_H","sigma_lambda_H","beta_H","beta0_H",
    #catch estimates and spline parts
    "Ctrend_ay", "C_ay", "sigma_C", "lambda_C", "C_ayg", "C_ayu", 
    "Cb_ayg", "Cb_ayu", "Cb_ay",
    "Cy_ayg", "Cy_ayu", "Cy_ay",
    "logChat_ay",
    #with hierarchichal pline lambda
    "mu_lambda_C","sigma_lambda_C","beta_C","beta0_C",
    #releases
    "R_ay", "R_ayg", "R_ayu", 
    "Rb_ayg", "Rb_ayu", "Rb_ay",
    "Ry_ayg", "Ry_ayu", "Ry_ay")
  return(params)
}

#------------------------------------------------------------------------------
# We also need raw data for plotting:
load_raw <- function(start_yr = 1977,
                     end_yr = 2019) {
  H_ayg <- readRDS(".//data//bayes_dat//H_ayg.rds") %>% 
    mutate(H_lb = ifelse(H == 0, 1, H))
  
  # Logbook releases by area, year for guided trips
  R_ayg <- readRDS(".//data//bayes_dat//R_ayg.rds") %>% 
    mutate(R_lb = ifelse(R == 0, 1, R),
           Rye = ifelse(year < 2006, NA,Rye))
  
  # SWHS harvests by area, year and user 
  Hhat_ayu <- 
    readRDS(".//data//bayes_dat//Hhat_ayu.rds")  %>% 
    mutate(Hhat = ifelse(H == 0, 1, H), 
           seH = ifelse(seH == 0, 1, seH)) %>%
    arrange(area, user, year)
  
  Chat_ayu <- 
    readRDS(".//data//bayes_dat//Chat_ayu.rds")  %>% 
    mutate(Chat = ifelse(C == 0, 1, C), 
           seC = ifelse(seC == 0, 1, seC)) %>%
    arrange(area, user, year)
  
  # SWHS harvests by area, year
  Hhat_ay <- 
    readRDS(".//data//bayes_dat//Hhat_ay.rds") %>% 
    rename(Hhat = H) %>%
    mutate(area = as.character(area)) %>%
    bind_rows(Hhat_ayu %>% 
                group_by(region, area, year) %>% 
                summarise(Hhat = sum(H), seH = sqrt(sum(seH^2))) %>%
                mutate(area = as.character(area))) %>%
    arrange(region, area, year) %>%
    mutate(Hhat = ifelse(Hhat == 0, 1, Hhat), 
           seH = ifelse(seH == 0, 1, seH)) %>%
    select(-cv)
  
  Hhat_ay %>% filter(is.na(Hhat))
  #DEV Code; delete once we figure out how to deal with the blanks
  Hhat_ay %>% mutate(Hhat = ifelse(is.na(Hhat),1,Hhat),
                     seH = ifelse(is.na(seH),1,seH)) -> Hhat_ay
  
  
  # SWHS Catch by area, year
  Chat_ay <- 
    readRDS(".//data//bayes_dat//Chat_ay.rds") %>% 
    rename(Chat = C) %>%
    mutate(area = as.character(area)) %>%
    bind_rows(readRDS(".//data//bayes_dat//Chat_ayu.rds") %>% 
                group_by(region, area, year) %>% 
                summarise(Chat = sum(C), seC = sqrt(sum(seC^2))) %>%
                mutate(area = as.character(area))) %>%
    arrange(region, area, year) %>%
    mutate(Chat = ifelse(Chat == 0, 1, Chat), 
           seC = ifelse(seC == 0, 1, seC)) %>%
    select(-cv)
  
  Chat_ay %>% filter(is.na(Chat))
  
  
  # Survey data on catch composition
  S_ayu0 <- 
    readRDS(".//data//bayes_dat//S_ayu.rds") 
  S_ayu <- 
    S_ayu0 %>% mutate(year = as.integer(year)) %>%
    bind_rows(data.frame(area = rep(unique(S_ayu0$area[S_ayu0$region %in% "Southeast"]), each = 4), 
                         year = rep(rep(1996:1997, each = 2), times = 6),
                         user = rep(c("charter", "private"), times = 12),
                         totalrf_n = 0, ye_n = NA, black_n = NA, pelagic_n = NA, nonpel_n = NA, notye_nonpel_n = NA)) %>%
    filter(year >= 1996) %>%
    arrange(user, area, year) 
  
  #When retention of YE is prohibited as n SE AK between 2020 through 2024 we need
  # to censor the survey/creel data
  S_ayu <- S_ayu %>%
    mutate(ye_n = ifelse(region == "Southeast" &
                           year > 2019 & year < 2025,
                         NA,ye_n))
  
    #COMP DATA
  comp <- S_ayu %>% filter(year >= start_yr & year <= end_yr) %>%
    mutate(area_n = as.numeric(area), 
           user_n = ifelse(user == "charter", 0, 1), 
           year_n = year - (start_yr - 1),  #year - 1995, changed with the addition of the old data...
           #region_n = ifelse()
           source = 1) %>% 
    select(year_n, area_n, user_n, source, N = totalrf_n, pelagic = pelagic_n, black = black_n, yellow = ye_n,
           region,area) %>%
    filter(N != 0) %>%
    mutate(yellow = ifelse(N - pelagic == 0, NA, yellow))
  
  raw <- list(H_ayg = H_ayg,
              R_ayg = R_ayg,
              Hhat_ayu = Hhat_ayu,
              Chat_ayu = Chat_ayu,
              Hhat_ay = Hhat_ay,
              Chat_ay = Chat_ay,
              S_ayu = S_ayu,
              comp = comp
              )
  return(raw)
}








