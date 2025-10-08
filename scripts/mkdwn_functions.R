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

#-------------------------------------------------------------------------------
# for plotting logit
logit_to_prob <- function(logit) {
  exp(logit) / (1 + exp(logit))
}

# Read data --------------------------------------------------------
# Logbook harvests by area, year for guided trips
readinData <- function(spl_knts = 7,
                       start_yr = 1977,
                       end_yr = 2019){
  H_ayg <- readRDS("..//data//bayes_dat//H_ayg.rds") %>% 
    mutate(H_lb = ifelse(H == 0, 1, H))
  
  # Logbook releases by area, year for guided trips
  R_ayg <- readRDS("..//data//bayes_dat//R_ayg.rds") %>% 
    mutate(R_lb = ifelse(R == 0, 1, R),
           Rye = ifelse(year < 2006, NA,Rye))

  # SWHS harvests by area, year and user 
  Hhat_ayu <- 
    readRDS("..//data//bayes_dat//Hhat_ayu.rds")  %>% 
    mutate(Hhat = ifelse(H == 0, 1, H), 
           seH = ifelse(seH == 0, 1, seH)) %>%
    arrange(area, user, year)
  
  Chat_ayu <- 
    readRDS("..//data//bayes_dat//Chat_ayu.rds")  %>% 
    mutate(Chat = ifelse(C == 0, 1, C), 
           seC = ifelse(seC == 0, 1, seC)) %>%
    arrange(area, user, year)
  
  Rhat_ayu <- Hhat_ayu %>%
    left_join(Chat_ayu, by = c("year","area","user","region")) %>%
    mutate(R = C - H,
           seR = sqrt(seH^2 + seC^2)) %>%
    mutate(Rhat = ifelse(R <= 0, 1, R), 
           seR = ifelse(seR == 0, 1, seR)) %>%
    arrange(area, user, year)
  
  # SWHS harvests by area, year
  Hhat_ay <- 
    readRDS("..//data//bayes_dat//Hhat_ay.rds") %>% 
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
    readRDS("..//data//bayes_dat//Chat_ay.rds") %>% 
    rename(Chat = C) %>%
    mutate(area = as.character(area)) %>%
    bind_rows(readRDS("..//data//bayes_dat//Chat_ayu.rds") %>% 
                group_by(region, area, year) %>% 
                summarise(Chat = sum(C), seC = sqrt(sum(seC^2))) %>%
                mutate(area = as.character(area))) %>%
    arrange(region, area, year) %>%
    mutate(Chat = ifelse(Chat == 0, 1, Chat), 
           seC = ifelse(seC == 0, 1, seC)) %>%
    select(-cv)
  
  Chat_ay %>% filter(is.na(Chat))
  
  Rhat_ay <- Hhat_ay %>%
    left_join(Chat_ay, by = c("year","area","region")) %>%
    filter(!is.na(Chat)) %>%
    mutate(Rhat = Chat - Hhat,
           seR = sqrt(seH^2 + seC^2)) %>%
    mutate(Rhat = ifelse(Rhat <= 0, 1, Rhat), 
           seR = ifelse(seR == 0, 1, seR)) 
  
  # Survey data on catch composition
  S_ayu0 <- 
    readRDS("..//data//bayes_dat//S_ayu.rds") 
  S_ayu <- 
    S_ayu0 %>% mutate(year = as.integer(year)) %>%
    bind_rows(data.frame(area = rep(unique(S_ayu0$area[S_ayu0$region %in% "Southeast"]), each = 4), 
                         year = rep(rep(1996:1997, each = 2), times = 6),
                         user = rep(c("charter", "private"), times = 12),
                         totalrf_n = 0, ye_n = NA, black_n = NA, pelagic_n = NA, nonpel_n = NA, notye_nonpel_n = NA)) %>%
    filter(year >= 1996) %>%
    arrange(user, area, year) 
  
  #Interview data on kept and released 
  I_ayu0 <- readRDS("..//data//bayes_dat//Int_ayu.rds") %>% arrange(area,user,year) %>%
    filter(!is.na(user))
  
  setdiff(expand_grid(year = unique(I_ayu0$year),
                      area = unique(I_ayu0$area),
                      user = unique(I_ayu0$user)),
          I_ayu0 %>% select(year,area,user)) -> misdat
  na_df <- as.data.frame(matrix(NA, nrow = nrow(misdat), 
                                ncol = length(colnames(I_ayu0)[4:18])))
  colnames(na_df) <- colnames(I_ayu0)[4:18]
  
  I_ayu <- 
    I_ayu0 %>% mutate(year = as.integer(year)) %>%
    bind_rows(bind_cols(misdat, na_df) %>%
                mutate(year = as.integer(year)) %>%
                right_join(I_ayu0 %>% select(area,region) %>% unique(),
                           by = "area")) %>%
    # filter(year >= 1996) %>%
    arrange(user, area, year) 
  
  # Kodiak hydroacoustic supplemental data
  kha <- readRDS("..//data//bayes_dat//kha.rds")
  
  #Weigth and release mortality data 
  wt_rm <- readRDS("..//data//bayes_dat//wt_rm_dat.rds") %>%
    mutate(assemblage = factor(assemblage, 
                               levels = c("black","yelloweye","pelnbrf","dsrlessye","slope"))) %>%
    arrange(assemblage, user,region, area, year) 
  
  # prep data for model
  H_ayg <- H_ayg %>% filter(year >= start_yr & year <= end_yr)
  
  R_ayg <- R_ayg %>% filter(year >= start_yr & year <= end_yr)
  
  Hhat_ayg <- Hhat_ayu %>% filter(user == "guided" & year >= start_yr & year <= end_yr); unique(Hhat_ayg$area)
  Hhat_ayp <- Hhat_ayu %>% filter(user == "private" & year >= start_yr & year <= end_yr)
  
  Chat_ayg <- Chat_ayu %>% filter(user == "guided" & year >= start_yr & year <= end_yr)
  Chat_ayp <- Chat_ayu %>% filter(user == "private" & year >= start_yr & year <= end_yr)
  
  Rhat_ayg <- Rhat_ayu %>% filter(user == "guided" & year >= start_yr & year <= end_yr)
  Rhat_ayp <- Rhat_ayu %>% filter(user == "private" & year >= start_yr & year <= end_yr)
  
  # Separate out the unknowns in the pre-1996 data
  Hhat_Uy <- Hhat_ay %>% filter(area == "UNKNOWN" & year >= start_yr & year <= end_yr)
  Chat_Uy <- Chat_ay %>% filter(area == "UNKNOWN" & year >= start_yr & year <= end_yr)
  Rhat_Uy <- Rhat_ay %>% filter(area == "UNKNOWN" & year >= start_yr & year <= end_yr)
  
  Hhat_ay <- Hhat_ay %>% filter(area != "UNKNOWN" & year >= start_yr & year <= end_yr)
  Chat_ay <- Chat_ay %>% filter(area != "UNKNOWN" & year >= start_yr & year <= end_yr)
  Rhat_ay <- Rhat_ay %>% filter(area != "UNKNOWN" & year >= start_yr & year <= end_yr)
  
  wt_rm <- wt_rm %>% filter(year >= start_yr & year <= end_yr)
  
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
    select(year, year_n, area_n, user_n, source, N = totalrf_n, 
           pelagic = pelagic_n, black = black_n, yellow = ye_n, 
           other = notye_nonpel_n, dsr = dsr_n, slope = slope_n,
           region,area) %>%
    filter(N != 0) %>%
    mutate(yellow = ifelse(N - pelagic == 0, NA, yellow))  #,
  ##         N = ifelse(is.na(yellow),NA,N))
  
  int <- I_ayu %>% filter(year >= start_yr & year <= end_yr) %>%
    mutate(area_n = as.numeric(as.factor(area)), 
           user_n = ifelse(user == "charter", 0, 1), 
           year_n = year - (start_yr - 1),  #year - 1995, changed with the addition of the old data...
           #region_n = ifelse()
           source = 1) %>%
    select(year, year_n, area_n, user_n, source, # N = totalrf_n, 
           inth_pel = pelagic_n, #black = black_n, 
           inth_yellow = ye_n, 
           inth_other = other_n, inth_dsr = dsr_n, inth_slope = slope_n,
           intc_pel = pelagic_c, #black = black_n, 
           intc_yellow = ye_c, 
           intc_other = other_c, 
           intc_dsr = dsr_c, 
           intc_slope = slope_c,
           region,area) %>%
    filter(!is.na(inth_pel))
  
  int_pel <- int %>% select(year,year_n,area_n,user_n,inth_pel,intc_pel) %>%
    filter(!is.na(intc_pel),
           intc_pel != 0)
  int_ye <- int %>% select(year,year_n,area_n,user_n,inth_yellow,intc_yellow) %>%
    filter(!is.na(intc_yellow),
           intc_yellow != 0)
  int_oth <- int %>% select(year,year_n,area_n,user_n,inth_other,intc_other) %>%
    filter(!is.na(intc_other),
           intc_other != 0)
  int_dsr <- int %>% select(year,year_n,area_n,user_n,inth_dsr,intc_dsr) %>%
    filter(!is.na(intc_dsr),
           intc_dsr != 0)
  int_slope <- int %>% select(year,year_n,area_n,user_n,inth_slope,intc_slope) %>%
    filter(!is.na(intc_slope),
           intc_slope != 0)
  
  compX <- comp %>%
    mutate(yellow_x = ifelse(region == "Southeast" & year > 2019 & year < 2025,
                             NA,yellow),
           pelagic_x = pelagic,
           N_x = ifelse(region == "Southeast" & year > 2019 & year < 2025,
                        NA,N)) %>%
    filter(!is.na(N_x))
  
  S_ayu %>% mutate(area_n = as.numeric(area)) %>%
    select(area,area_n) %>% unique() %>% #-> area_ns
    right_join(kha, by = "area") %>% filter(year >= start_yr & year <= end_yr) %>%
    mutate(#area_n = , 
      #user_n = ifelse(user == "charter", 0, 1), 
      year_n = year - (start_yr - 1)) %>% 
    select(year, year_n, area_n, #N = rf_tot, 
           pelagic = rf_tot, black = brf_tot, 
           pel_cv = rf_cv, black_cv = brf_cv,
           prop_brf, prop_cv,
           area) -> kha_dat
  
  kha_dat <- rbind(kha_dat %>% mutate(user_n = 0),
                   kha_dat %>% mutate(user_n = 1))
  
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
  
  matrix_Rhat_ay <- cbind(matrix(NA,nrow = A, ncol = Y - length(unique(Rhat_ay$year))),
                          matrix(Rhat_ay$Rhat, nrow = A, ncol = length(unique(Rhat_ay$year)), byrow = TRUE))
  
  #matrix_Chat_ay[4, 1:5] <- NA
  cvRhat_ay = cbind(matrix(NA,nrow = A, ncol = Y - length(unique(Rhat_ay$year))),
                    matrix(Rhat_ay$seC, nrow = A, ncol = length(unique(Rhat_ay$year)), byrow = TRUE)) /
    matrix_Rhat_ay
  # need cv to be 1 when there is no data
  cvRhat_ay[is.na(cvRhat_ay)] <- 1
  
  dim(matrix_Hhat_ay)
  dim(matrix_Chat_ay)
  
  wt_rm <- wt_rm %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))
  
  #write.csv(wt_rm, "data/wt_rm_check.csv", row.names = F)
  
  sc_rmwt <- wt_rm %>% filter(region != "Southeast") %>% arrange(region,area,year)
  se_rmwt <- wt_rm %>% filter(region == "Southeast") %>% arrange(region,area,year)
  
  #Create JAGs data and then bundle it up
  
  jags_dat <- 
    list(
      A = A, Y = Y, C = C,
      #Harvest
      Hhat_ay = matrix_Hhat_ay,
      cvHhat_ay = cvHhat_ay,
      #Releases
      Rhat_ay = matrix_Rhat_ay,
      cvRhat_ay = cvRhat_ay,
      
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
      Hlbo_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(H_ayg$year))),
                       matrix(H_ayg$Ho, nrow = A, ncol = length(unique(H_ayg$year)), byrow = TRUE)),
      #Releases by species and user: 
      Rlb_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                      matrix(R_ayg$R_lb, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
      Rlb_ayg_cens = matrix(as.numeric(NA), nrow = A, ncol = Y ),
      # logbook pelagic rf harvested by guides
      Rlbp_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                       matrix(R_ayg$Rp, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
      Rlbp_ayg_cens = matrix(as.numeric(NA), nrow = A, ncol = Y ),
      # logbook ye rf harvested by guides
      Rlby_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                       matrix(R_ayg$Rye, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
      Rlby_ayg_bound = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                             matrix(R_ayg$Rye, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
      Rlby_ayg_cens = matrix(as.numeric(NA), nrow = A, ncol = Y ),
      #non-pelagic, non-yelloweye
      Rlbo_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                       matrix(R_ayg$Ro, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
      Rlbo_ayg_cens = matrix(as.numeric(NA), nrow = A, ncol = Y ),
      
      #SWHS DATA:
      # SWHS estimates of rockfish harvests
      Hhat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
                       matrix(Hhat_ayg$Hhat, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)),
      # cv of SWHS estimates
      cvHhat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
                         matrix(Hhat_ayg$seH, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)) / 
        cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
              matrix(Hhat_ayg$Hhat, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)),
      
      Hhat_ayu = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayp$year))),
                       matrix(Hhat_ayp$Hhat, nrow = A, ncol = length(unique(Hhat_ayp$year)), byrow = TRUE)),
      cvHhat_ayu = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayp$year))),
                         matrix(Hhat_ayp$seH, nrow = A, ncol = length(unique(Hhat_ayp$year)), byrow = TRUE)) / 
        cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayp$year))),
              matrix(Hhat_ayp$Hhat, nrow = A, ncol = length(unique(Hhat_ayp$year)), byrow = TRUE)),
      
      # SWHS estimates of rockfish Catches
      Rhat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Rhat_ayg$year))),
                       matrix(Rhat_ayg$Rhat, nrow = A, ncol = length(unique(Rhat_ayg$year)), byrow = TRUE)),
      # cv of SWHS estimates
      cvRhat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Rhat_ayg$year))),
                         matrix(Rhat_ayg$seC, nrow = A, ncol = length(unique(Rhat_ayg$year)), byrow = TRUE)) / 
        cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Rhat_ayg$year))),
              matrix(Rhat_ayg$Rhat, nrow = A, ncol = length(unique(Rhat_ayg$year)), byrow = TRUE)),
      Rhat_ayu = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Rhat_ayp$year))),
                       matrix(Rhat_ayp$Rhat, nrow = A, ncol = length(unique(Rhat_ayp$year)), byrow = TRUE)),
      # cv of SWHS estimates
      cvRhat_ayu = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Rhat_ayp$year))),
                         matrix(Rhat_ayp$seC, nrow = A, ncol = length(unique(Rhat_ayp$year)), byrow = TRUE)) / 
        cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Rhat_ayp$year))),
              matrix(Rhat_ayp$Rhat, nrow = A, ncol = length(unique(Rhat_ayp$year)), byrow = TRUE)),
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
      comp_other = comp$other,
      comp_dsr = comp$dsr,
      comp_slope = comp$slope,
      N = dim(comp)[1],
      
      SEn1 = min(as.numeric(row.names(comp[comp$region == "Southeast" & comp$user_n == 0,]))),
      SEn2 = max(as.numeric(row.names(comp[comp$region == "Southeast" & comp$user_n == 0,]))),
      SEn3 = min(as.numeric(row.names(comp[comp$region == "Southeast" & comp$user_n == 1,]))),
      SEn4 = max(as.numeric(row.names(comp[comp$region == "Southeast" & comp$user_n == 1,]))),
      
      #interview data
      intp_area = int_pel$area_n,
      intp_year = int_pel$year_n,
      intp_user = int_pel$user_n,
      inth_pel = int_pel$inth_pel,
      intc_pel = int_pel$intc_pel,
      Nint_pel = dim(int_pel)[1],
      
      inty_area = int_ye$area_n,
      inty_year = int_ye$year_n,
      inty_user = int_ye$user_n,
      inth_ye = int_ye$inth_yellow,
      intc_ye = int_ye$intc_yellow,
      Nint_ye = dim(int_ye)[1],
      
      into_area = int_oth$area_n,
      into_year = int_oth$year_n,
      into_user = int_oth$user_n,
      inth_other = int_oth$inth_other,
      intc_other = int_oth$intc_other,
      Nint_oth = dim(int_oth)[1],
      
      intd_area = int_dsr$area_n,
      intd_year = int_dsr$year_n,
      intd_user = int_dsr$user_n,
      inth_dsr = int_dsr$inth_dsr,
      intc_dsr = int_dsr$intc_dsr,
      Nint_dsr = dim(int_dsr)[1],
      
      ints_area = int_slope$area_n,
      ints_year = int_slope$year_n,
      ints_user = int_slope$user_n,
      inth_slope = int_slope$inth_slope,
      intc_slope = int_slope$intc_slope,
      Nint_slope = dim(int_slope)[1],
      
      regions = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3),
      
      Nkha = dim(kha_dat)[1],
      kprop_b = kha_dat$prop_brf,
      kprop_cv = kha_dat$prop_cv,
      
      kha_area = kha_dat$area_n,
      kha_year = kha_dat$year_n,
      kha_user = kha_dat$user_n,
      
      r1_gwt_b = matrix(se_rmwt$wt_lbs[se_rmwt$assemblage == "black" & se_rmwt$user == "charter"],
                        nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_gwt_y = matrix(se_rmwt$wt_lbs[se_rmwt$assemblage == "yelloweye" & se_rmwt$user == "charter"],
                        nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_gwt_p = matrix(se_rmwt$wt_lbs[se_rmwt$assemblage == "pelnbrf" & se_rmwt$user == "charter"],
                        nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_gwt_d = matrix(se_rmwt$wt_lbs[se_rmwt$assemblage == "dsrlessye" & se_rmwt$user == "charter"],
                        nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_gwt_s = matrix(se_rmwt$wt_lbs[se_rmwt$assemblage == "slope" & se_rmwt$user == "charter"],
                        nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_uwt_b = matrix(se_rmwt$wt_lbs[se_rmwt$assemblage == "black" & se_rmwt$user == "private"],
                        nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_uwt_y = matrix(se_rmwt$wt_lbs[se_rmwt$assemblage == "yelloweye" & se_rmwt$user == "private"],
                        nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_uwt_p = matrix(se_rmwt$wt_lbs[se_rmwt$assemblage == "pelnbrf" & se_rmwt$user == "private"],
                        nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_uwt_d = matrix(se_rmwt$wt_lbs[se_rmwt$assemblage == "dsrlessye" & se_rmwt$user == "private"],
                        nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_uwt_s = matrix(se_rmwt$wt_lbs[se_rmwt$assemblage == "slope" & se_rmwt$user == "private"],
                        nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      
      r1_gwtcv_b = matrix(se_rmwt$wt_cv[se_rmwt$assemblage == "black" & se_rmwt$user == "charter"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_gwtcv_y = matrix(se_rmwt$wt_cv[se_rmwt$assemblage == "yelloweye" & se_rmwt$user == "charter"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_gwtcv_p = matrix(se_rmwt$wt_cv[se_rmwt$assemblage == "pelnbrf" & se_rmwt$user == "charter"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_gwtcv_d = matrix(se_rmwt$wt_cv[se_rmwt$assemblage == "dsrlessye" & se_rmwt$user == "charter"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_gwtcv_s = matrix(se_rmwt$wt_cv[se_rmwt$assemblage == "slope" & se_rmwt$user == "charter"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_uwtcv_b = matrix(se_rmwt$wt_cv[se_rmwt$assemblage == "black" & se_rmwt$user == "private"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_uwtcv_y = matrix(se_rmwt$wt_cv[se_rmwt$assemblage == "yelloweye" & se_rmwt$user == "private"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_uwtcv_p = matrix(se_rmwt$wt_cv[se_rmwt$assemblage == "pelnbrf" & se_rmwt$user == "private"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_uwtcv_d = matrix(se_rmwt$wt_cv[se_rmwt$assemblage == "dsrlessye" & se_rmwt$user == "private"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_uwtcv_s = matrix(se_rmwt$wt_cv[se_rmwt$assemblage == "slope" & se_rmwt$user == "private"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      
      r1_gmort_b = matrix(se_rmwt$r_mort[se_rmwt$assemblage == "black" & se_rmwt$user == "charter"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_gmort_y = matrix(se_rmwt$r_mort[se_rmwt$assemblage == "yelloweye" & se_rmwt$user == "charter"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_gmort_p = matrix(se_rmwt$r_mort[se_rmwt$assemblage == "pelnbrf" & se_rmwt$user == "charter"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_gmort_d = matrix(se_rmwt$r_mort[se_rmwt$assemblage == "dsrlessye" & se_rmwt$user == "charter"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_gmort_s = matrix(se_rmwt$r_mort[se_rmwt$assemblage == "slope" & se_rmwt$user == "charter"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_umort_b = matrix(se_rmwt$r_mort[se_rmwt$assemblage == "black" & se_rmwt$user == "private"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_umort_y = matrix(se_rmwt$r_mort[se_rmwt$assemblage == "yelloweye" & se_rmwt$user == "private"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_umort_p = matrix(se_rmwt$r_mort[se_rmwt$assemblage == "pelnbrf" & se_rmwt$user == "private"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_umort_d = matrix(se_rmwt$r_mort[se_rmwt$assemblage == "dsrlessye" & se_rmwt$user == "private"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      r1_umort_s = matrix(se_rmwt$r_mort[se_rmwt$assemblage == "slope" & se_rmwt$user == "private"],
                          nrow = 6, ncol = length(unique(se_rmwt$year)), byrow=TRUE),
      
      r2_gwt_b = matrix(sc_rmwt$wt_lbs[sc_rmwt$assemblage == "black" & sc_rmwt$user == "charter"],
                        nrow = 10, ncol = length(unique(sc_rmwt$year)), byrow=TRUE),
      r2_gwt_y = matrix(sc_rmwt$wt_lbs[sc_rmwt$assemblage == "yelloweye" & sc_rmwt$user == "charter"],
                        nrow = 10, ncol = length(unique(sc_rmwt$year)), byrow=TRUE),
      r2_uwt_b = matrix(sc_rmwt$wt_lbs[sc_rmwt$assemblage == "black" & sc_rmwt$user == "private"],
                        nrow = 10, ncol = length(unique(sc_rmwt$year)), byrow=TRUE),
      r2_uwt_y = matrix(sc_rmwt$wt_lbs[sc_rmwt$assemblage == "yelloweye" & sc_rmwt$user == "private"],
                        nrow = 10, ncol = length(unique(sc_rmwt$year)), byrow=TRUE),
      
      r2_gwtcv_b = matrix(sc_rmwt$wt_cv[sc_rmwt$assemblage == "black" & sc_rmwt$user == "charter"],
                          nrow = 10, ncol = length(unique(sc_rmwt$year)), byrow=TRUE),
      r2_gwtcv_y = matrix(sc_rmwt$wt_cv[sc_rmwt$assemblage == "yelloweye" & sc_rmwt$user == "charter"],
                          nrow = 10, ncol = length(unique(sc_rmwt$year)), byrow=TRUE),
      r2_uwtcv_b = matrix(sc_rmwt$wt_cv[sc_rmwt$assemblage == "black" & sc_rmwt$user == "private"],
                          nrow = 10, ncol = length(unique(sc_rmwt$year)), byrow=TRUE),
      r2_uwtcv_y = matrix(sc_rmwt$wt_cv[sc_rmwt$assemblage == "yelloweye" & sc_rmwt$user == "private"],
                          nrow = 10, ncol = length(unique(sc_rmwt$year)), byrow=TRUE),
      
      r2_gmort_b = matrix(sc_rmwt$r_mort[sc_rmwt$assemblage == "black" & sc_rmwt$user == "charter"],
                          nrow = 10, ncol = length(unique(sc_rmwt$year)), byrow=TRUE),
      r2_gmort_y = matrix(sc_rmwt$r_mort[sc_rmwt$assemblage == "yelloweye" & sc_rmwt$user == "charter"],
                          nrow = 10, ncol = length(unique(sc_rmwt$year)), byrow=TRUE),
      r2_umort_b = matrix(sc_rmwt$r_mort[sc_rmwt$assemblage == "black" & sc_rmwt$user == "private"],
                          nrow = 10, ncol = length(unique(sc_rmwt$year)), byrow=TRUE),
      r2_umort_y = matrix(sc_rmwt$r_mort[sc_rmwt$assemblage == "yelloweye" & sc_rmwt$user == "private"],
                          nrow = 10, ncol = length(unique(sc_rmwt$year)), byrow=TRUE)
    )
  
  return(list(jags_dat = jags_dat,
              H_ayg = H_ayg,
              R_ayg = R_ayg,
              Hhat_ayu = Hhat_ayu,
              Chat_ayu = Chat_ayu,
              Rhat_ayu = Rhat_ayu,
              Hhat_ayg = Hhat_ayg,
              Hhat_ay = Hhat_ay,
              Chat_ay = Chat_ay,
              Rhat_ay = Rhat_ay,
              S_ayu = S_ayu,
              comp = comp,
              compX = compX,
              int = int,
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
    "logbc_R", "mu_bc_R", "sd_bc_R",
    "bc_R_offset","mu_bc_R_offset","sd_bcRoff",
    #User proportions (proportion guided); different for H and R
    "pG", "b1_pG", "b2_pG",
    #"pG_R", "b1_pG_R", "b2_pG_R",
    #proportion harvested: 
    "pH", "tau_pH",#"pHu",
    "pH2", "tau_pH2",#"pHu",
    "pHg","pHu","pHnpny",
    "mu_beta0_pH","tau_beta0_pH","beta0_pH","beta1_pH","beta2_pH","beta3_pH","beta4_pH",
    "mu_beta2_pH","tau_beta2_pH", #"beta2_pH2","beta1_pH2","beta2_pH2","beta3_pH2","beta4_pH2",
    #random effects on pH
    "re_pH", "re_pH2",
    "sd_pH", "mu_pH",
    #"re_pH","sd_pH",
    #proportions same for catch and harvest? thinking on it?
    "p_pelagic", "beta0_pelagic", "beta1_pelagic", "beta2_pelagic", "beta3_pelagic", "beta4_pelagic", 
    "mu_beta0_pelagic", "tau_beta0_pelagic",
    "mu_beta1_pelagic", "tau_beta1_pelagic",
    "mu_beta2_pelagic", "tau_beta2_pelagic",
    "mu_beta3_pelagic", "tau_beta3_pelagic",
    "mu_beta4_pelagic", "tau_beta4_pelagic",
    "p_yellow", "beta0_yellow", "beta1_yellow", "beta2_yellow", "beta3_yellow", "beta4_yellow",
    "mu_beta0_yellow", "tau_beta0_yellow",
    "mu_beta1_yellow", "tau_beta1_yellow",
    "mu_beta2_yellow", "tau_beta2_yellow",
    "mu_beta3_yellow", "tau_beta3_yellow",
    "mu_beta4_yellow", "tau_beta4_yellow",
    "p_yellow_x", "beta0_yellow_x", "beta1_yellow_x", "beta2_yellow_x", "beta3_yellow_x", "beta4_yellow_x",
    "mu_beta0_yellow_x", "tau_beta0_yellow_x",
    "mu_beta1_yellow_x", "tau_beta1_yellow_x",
    "mu_beta2_yellow_x", "tau_beta2_yellow_x",
    "mu_beta3_yellow_x", "tau_beta3_yellow_x",
    "mu_beta4_yellow_x", "tau_beta4_yellow_x",
    "p_black", "beta0_black", "beta1_black", "beta2_black",  "beta3_black", "beta4_black",
    "mu_beta0_black", "tau_beta0_black",
    "mu_beta1_black", "tau_beta1_black",
    "mu_beta2_black", "tau_beta2_black",
    "mu_beta3_black", "tau_beta3_black",
    "mu_beta4_black", "tau_beta4_black",
    "p_dsr", "beta0_dsr", "beta1_dsr", "beta2_dsr",  "beta3_dsr", "beta4_dsr",
    "mu_beta0_dsr", "tau_beta0_dsr",
    "mu_beta1_dsr", "tau_beta1_dsr",
    "mu_beta2_dsr", "tau_beta2_dsr",
    "mu_beta3_dsr", "tau_beta3_dsr",
    "mu_beta4_dsr", "tau_beta4_dsrk",
    "p_slope", "beta0_slope", "beta1_slope", "beta2_slope",  "beta3_slope", "beta4_slope",
    "mu_beta0_slope", "tau_beta0_slope",
    "mu_beta1_slope", "tau_beta1_slope",
    "mu_beta2_slope", "tau_beta2_slope",
    "mu_beta3_slope", "tau_beta3_slope",
    "mu_beta4_slope", "tau_beta4_slope",
    #random effects on species
    "re_pelagic", "re_black","re_yellow",
    "sd_comp", "tau_comp",
    #harvest estimates and spline parts
    "Htrend_ay", "H_ay", "sigma_H", "lambda_H", "H_ayg", "H_ayu", 
    "Hb_ayg", "Hb_ayu", "Hb_ay",
    "Hy_ayg", "Hy_ayu", "Hy_ay",
    "Hd_ayg", "Hd_ayu", "Hd_ay",
    "Hs_ayg", "Hs_ayu", "Hs_ay",
    "Ho_ayg", "Ho_ayu", "Ho_ay",
    "logHhat_ay",
    #with hierarchichal pline lambda
    "mu_lambda_H","sigma_lambda_H","beta_H","beta0_H",
    #catch estimates and spline parts
    "Chat_ay","C_ay", "C_ayg", "C_ayu", 
    "Cb_ayg", "Cb_ayu", "Cb_ay",
    "Cy_ayg", "Cy_ayu", "Cy_ay",
    #with hierarchichal pline lambda
    #"mu_lambda_C","sigma_lambda_C","beta_C","beta0_C",
    #releases
    "logRhat_ay","logRhat_ayg",
    "R_ay", "R_ayg", "R_ayu", 
    "Rb_ayg", "Rb_ayu", "Rb_ay",
    "Ry_ayg", "Ry_ayu", "Ry_ay",
    "Ro_ayg", "Ro_ayu", "Ro_ay",
    "Rd_ayg", "Rd_ayu", "Rd_ay",
    "Rs_ayg", "Rs_ayu", "Rs_ay",
    "nonrecR_ayg")
  return(params)
}

#------------------------------------------------------------------------------









