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

source(".\\scripts//functions.R")

REP_YR <- 2023

# Read data --------------------------------------------------------
# Logbook harvests by area, year for guided trips
H_ayg <- readRDS(".//data//bayes_dat//H_ayg.rds") %>% 
  mutate(H_lb = ifelse(H == 0, 1, H))

# Logbook releases by area, year for guided trips
R_ayg <- readRDS(".//data//bayes_dat//R_ayg.rds") %>% 
  mutate(R_lb = ifelse(R == 0, 1, R),
         Rye = ifelse(year < 2006, NA,Rye))

unique(H_ayg$year)
unique(R_ayg$year)

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
#DEV Code; delete once we figure out how to deal with the blanks
#put large CV on seC where there are no estimates.
#Chat_ay %>% mutate(#Chat = ifelse(is.na(Chat),1,Chat),
#                   seC = ifelse(is.na(seC),1,seC)) -> Chat_ay

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

unique(S_ayu$year)

S_ayu %>% filter(year < 1998) %>% print(n=60)
# Plot data --------------------------------------------------------
# * SWHS estimates by user --------------------------------------------------------
unique(Hhat_ayu$year)

Hhat_ayu %>%
  select(year, user, area, region, Hhat, seH) %>%
  rbind(Hhat_ay %>% mutate(user = "all")) %>%
  ggplot(aes(year, Hhat, color = user)) +
  geom_line() +
  facet_wrap(area ~ ., scales = "free")
# * SWHS variability by user --------------------------------------------------------
Hhat_ayu %>%
  select(year, user, area, region, Hhat, seH) %>%
  rbind(Hhat_ay %>% mutate(user = "all")) %>%
  mutate(cv = seH/Hhat) %>%
  ggplot(aes(year, cv, color = user)) +
  geom_line() +
  facet_wrap(area ~ .)

# * Logbook Vrs SWHS total --------------------------------------------------------
left_join(H_ayg, Hhat_ay, by = c("area", "year", "region")) %>%
  select(year, area, H, Hhat) %>%
  pivot_longer(starts_with("H"), names_to = "source", values_to = "H") %>%
  ggplot(aes(year, H, color = source)) +
  geom_line() +
  facet_wrap(area ~ ., scales = "free_y")
left_join(H_ayg, Hhat_ay, by = c("area", "year", "region")) %>%
  ggplot(aes(H_lb, Hhat, color = area)) +
  geom_abline() +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  facet_grid(region ~ ., scales = "free")

# * SWHS catch Vrs SWHS harvest --------------------------------------------------------
left_join(Chat_ay, Hhat_ay, by = c("area", "year", "region")) %>%
  select(year, area, Chat, Hhat) %>%
  pivot_longer(ends_with("hat"), names_to = "source", values_to = "N") %>%
  ggplot(aes(year, N, color = source)) +
  geom_line() +
  facet_wrap(area ~ ., scales = "free")

# * Logbook releases versus swhs releases --------------------------------------
left_join(Chat_ay, Hhat_ay, by = c("area", "year", "region")) %>%
  select(year, area, Chat, Hhat) %>%
  mutate(Rhat = Chat - Hhat,
         cat = "swhs_tot") %>%
  select(year,area,cat,R = Rhat) %>%
  rbind(left_join(Chat_ayu %>% filter(user == "guided"), 
                  Hhat_ayu %>% filter(user == "guided"),
                  by = c("area", "year", "region")) %>%
          select(year, area, gChat = Chat, gHhat = Hhat) %>%
          mutate(gRhat = gChat -gHhat,
                 cat = "swhs_gui") %>%
          select(year,area,cat,R = gRhat)) %>%
  rbind(left_join(Chat_ayu %>% filter(user == "private"), 
                  Hhat_ayu %>% filter(user == "private"), 
                  by = c("area", "year", "region")) %>%
          select(year, area, uChat = Chat, uHhat = Hhat) %>%
          mutate(uRhat = uChat -uHhat,
                 cat = "swhs_pri") %>%
          select(year,area,cat,R = uRhat)) %>%
  rbind(R_ayg %>% mutate(cat = "logbook") %>% select(year,area,cat,R)) %>%
  ggplot(aes(year,R,color = cat)) + 
  geom_line() +
  facet_wrap(area~., scales = "free")

left_join(Chat_ayu %>% filter(user == "guided"), 
          Hhat_ayu %>% filter(user == "guided"),
          by = c("area", "year", "region")) %>%
  select(year, area, region, gChat = Chat, gHhat = Hhat) %>%
  mutate(gRhat = gChat -gHhat) %>%
  select(year,area,region,Rhat = gRhat) %>%
  left_join(R_ayg %>% select(year,area,region,R),
            by = c('area',"year","region")) %>%
  ggplot(aes(R, Rhat, color = area)) +
  geom_abline() +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  facet_grid(region ~ ., scales = "free")

# * logbook vrs sample % pelagic --------------------------------------------------------
rbind(S_ayu %>% mutate(pct_pel = pelagic_n / totalrf_n, source = "survey") %>% 
        select(area, year, user, source, pct_pel),
      H_ayg %>% mutate(pct_pel = Hp / H, user = "charter", source = "logbook") %>% 
        select(area, year, user, source, pct_pel)) %>%
  ggplot(aes(year, pct_pel, color = source)) +
  geom_line(aes(linetype = user)) +
  facet_wrap(area ~ ., scales = "free")

S_ayu %>% 
  mutate(pct_pel = black_n / totalrf_n, 
         source = "survey") %>% 
  select(area, year, user, source, pct_pel) %>%
  ggplot(aes(year, pct_pel)) +
  geom_line(aes(linetype = user)) +
  facet_wrap(area ~ ., scales = "free")

#----------------------------------------------------------------------------
# Prep data for jags --------------------------------------------------------
start_yr <- 1977 # 1996 or 1977
end_yr <- 2019

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
C <- 7
Z <- bspline(1:Y, K = C) #bspline(1:24, K = C)

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
  
range(comp$year_n)

with(comp, table(area_n, area))
with(comp, table(user_n,area))
with(S_ayu, table(user,area))


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
    Rlb_ayg_trunc = matrix(as.numeric(NA), nrow = A, ncol = Y ),
    Rlb_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                    matrix(R_ayg$R_lb, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
    Rlb_ayg_bound = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                    matrix(R_ayg$R_lb, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
    # logbook pelagic rf harvested by guides
    Rlbp_ayg_trunc = matrix(as.numeric(NA), nrow = A, ncol = Y ),
    Rlbp_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                     matrix(R_ayg$Rp, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
    Rlbp_ayg_bound = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                     matrix(R_ayg$Rp, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
    # logbook ye rf harvested by guides
    Rlby_ayg_trunc = matrix(as.numeric(NA), nrow = A, ncol = Y ),
    Rlby_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                     matrix(R_ayg$Rye, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
    Rlby_ayg_bound = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(R_ayg$year))),
                           matrix(R_ayg$Rye, nrow = A, ncol = length(unique(R_ayg$year)), byrow = TRUE)),
    
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
    regions = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3)
    )

area_codes <- comp %>% select(area,area_n) %>% unique() %>%
  add_row(area = "BSAI", area_n = 5) %>%
  add_row(area = "SOKO2SAP", area_n = 6) %>%
  add_row(area = "WKMA", area_n = 7) %>%
  mutate(area_n = as.character(area_n)) %>% arrange(as.numeric(area_n))

jags_dat$Hhat_ayg
jags_dat$Chat_ay

jags_dat$Hlb_ayg   
jags_dat$Hlbp_ayg
jags_dat$Hlby_ayg

jags_dat$Rlb_ayg   
jags_dat$Rlbp_ayg
jags_dat$Rlby_ayg

str(jags_dat$Hlby_ayg); str(jags_dat$Rlby_ayg)

jags_dat$cvHhat_ay
jags_dat$cvChat_ay
# Run Jags --------------------------------------------------------

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
            "p_black", "beta0_black", "beta1_black", "beta2_black", "beta3_black","beta4_black",
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
            "mu_lambda_H","sigma_lambda_H",
            #catch estimates and spline parts
            "Ctrend_ay", "C_ay", "sigma_C", "lambda_C", "C_ayg", "C_ayu", 
            "Cb_ayg", "Cb_ayu", "Cb_ay",
            "Cy_ayg", "Cy_ayu", "Cy_ay",
            "logChat_ay",
            #with hierarchichal pline lambda
            "mu_lambda_C","sigma_lambda_C",
            #releases
            "R_ay", "R_ayg", "R_ayu", 
            "Rb_ayg", "Rb_ayu", "Rb_ay",
            "Ry_ayg", "Ry_ayu", "Ry_ay",
            #release estimation new stuff
            "totRy","uRy_ayg","uRy_alpha","uRy_beta") #need to add in releases:

ni <- 1E5; nb <- ni*.7; nc <- 3; nt <- ni / 1000;

tstart <- Sys.time()
postH <- 
  jagsUI::jags(
    parameters.to.save = params,
    model.file = ".\\models\\model_HCR_censLBR.txt",
    data = jags_dat, 
    parallel = TRUE, verbose = TRUE,
    #inits = list(list(muHhat_ay = log(jags_dat$H_ayg * 1.2)), list(muHhat_ay = log(jags_dat$H_ayg * 1.2)), list(muHhat_ay = log(jags_dat$H_ayg * 1.2))),
    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
    store.data = TRUE)
runtime <- Sys.time() - tstart; runtime

postH
#Note 5e5 iterations about 1 hour and ~95% converged

#Note 5e5 iterations about 1 hour and ~95% converged
#Note 1e5 iterations 20 min and ~92% converged
#Note 5e4 iters of model_H_C_part_pH_trend 10 minutes and 90% converged
#Note 1e7 iters HCR_poly_pH_split_bc 2.2 days and <90% converged 
#15e5 = 8.5 hrs, 91& conv
# 24e5 bcCoff 13.5 hours, 90% converged but better. 

mod_name <- "post_HCR_poly_pH_bcCoff"
#get last mode run initial values:
last_samples <- lapply(1:nc, function(chain) {
  chain_data <- as.matrix(postH$samples[[chain]])
  as.list(chain_data[nrow(chain_data), ])
})

saveRDS(last_samples, paste0(".\\data\\bayes_dat\\",mod_name,"_inits.rds"))



last_samples <- readRDS(paste0(".\\data\\bayes_dat\\",mod_name,"_inits.rds"))

str(last_samples)
for (i in 1:3) {
  last_samples[[i]]$'mu_bc_C_offset[1]' <- runif(1,0.001,5)
  last_samples[[i]]$'mu_bc_C_offset[2]' <- runif(1,0.001,5)
  last_samples[[i]]$'mu_bc_C_offset[3]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[1]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[2]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[3]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[4]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[5]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[6]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[7]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[8]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[9]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[10]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[11]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[12]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[13]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[14]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[15]' <- runif(1,0.001,5)
  last_samples[[i]]$'bc_C_offset[16]' <- runif(1,0.001,5)
}

# Re-run the model with these initial values

tstart <- Sys.time()
postH <- jagsUI::jags(
  parameters.to.save = params,
  model.file = ".\\models\\model_HCR_truncR_bcCoff.txt",
  data = jags_dat, 
  inits = last_samples,
  parallel = TRUE, 
  n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = 0,  # no burn-in for the second run
  store.data = TRUE, verbose = TRUE
)
runtime <- Sys.time() - tstart; runtime

mod_name <- "post_HCR_truncR_bcCoff_60e5_7kn"

saveRDS(postH, paste0(".\\output\\bayes_posts\\",mod_name,".rds"))

postH <- readRDS(paste0(".\\output\\bayes_posts\\",mod_name,".rds"))

#-------------------------------------------------------------------------------
# Convergence exam
#-------------------------------------------------------------------------------
# spline 5, 1e5 = shit convergence (90% Rhat = 2.44)
# spline 7, 1e5 = still shit, but less so... 
# spline 7 with multiplicitive C_offset 93% conv
# 7 knot 25e5 16 hours 95% conv

rhat <- get_Rhat(postH, cutoff = 1.11)
names(rhat)[1] <- "Rhat_values"
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

rhat_exam %>% group_by(variable,area) %>%
  summarise(n = n(),
            badRhat_avg = mean(Rhat)) %>%
  arrange(-badRhat_avg,-n) %>% print(n = 100)

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
  filter(str_detect(variable, "lambda")) %>% print(n = 50)

jagsUI::traceplot(postH, parameters = "lambda_H")

jagsUI::traceplot(postH, parameters = c("mu_lambda_H","sigma_lambda_H"))

jagsUI::traceplot(postH, parameters = "lambda_C")

jagsUI::traceplot(postH, parameters = c("mu_lambda_C","sigma_lambda_C"))

jagsUI::traceplot(postH, parameters = c("mu_beta0_pH","tau_beta0_pH",
                                        "beta0_pH","beta1_pH",
                                        "beta2_pH","beta3_pH"))

rhat_exam %>% group_by(variable,area) %>%
  summarise(n = n(),
            badRhat_avg = mean(Rhat)) %>%
  arrange(-badRhat_avg,-n) %>% 
  filter(str_detect(variable, "pH")) %>% print(n = 50)

jagsUI::traceplot(postH, parameters = c("mu_beta0_yellow","tau_beta0_yellow",
                                        "beta0_yellow","beta1_yellow",
                                        "beta2_yellow","beta3_yellow"))

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
                                        "beta2_pelagic"))

jagsUI::traceplot(postH, parameters = c("mu_beta0_black","tau_beta0_pHblack",
                                        "beta0_black","beta1_black",
                                        "beta2_black"))

jagsUI::traceplot(postH, parameters = c("mu_bc_H","tau_bc_H","sd_bc_H"))
jagsUI::traceplot(postH, parameters = "logbc_H")

#jagsUI::traceplot(postH, parameters = c("mu_bc_C","tau_bc_C","sd_bc_C"))
postH$mean$bc_C_offset; exp(postH$mean$bc_C_offset)

jagsUI::traceplot(postH, parameters = c("bc_C_offset"))
jagsUI::traceplot(postH, parameters = "logbc_C")

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
  rbind(t(jags_dat$Chat_ay_pH),
        t(apply(exp(postH$sims.list$Ctrend_ay), c(2,3), mean)),
        t(postH$mean$C_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 3),
         source = rep(c("SWHS", "trend", "C"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "C") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = C, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

as.data.frame(
  rbind(t(jags_dat$Chat_ayg - jags_dat$Hhat_ayg),
        t(jags_dat$Rlb_ayg), 
        #t(apply(exp(postH$sims.list$Ctrend_ay), c(2,3), mean)),
        t(postH$mean$R_ayg))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 3),
         source = rep(c("SWHS","LB" ,"R"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "R") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = R, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

#as.data.frame(
#  rbind(t(jags_dat$Chat_ay_pH - jags_dat$Hhat_ay),
        #t(apply(exp(postH$sims.list$Ctrend_ay), c(2,3), mean)),
#        t(postH$mean$R_ay))) %>%
#  setNames(nm = unique(H_ayg$area)) %>%
#  mutate(year = rep(start_yr:end_yr, times = 2),
#         source = rep(c("SWHS", "R"), each = Y)) %>%
#  pivot_longer(!c(year, source), names_to = "area", values_to = "R") %>%
#  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
#  ggplot(aes(x = year, y = R, color = source)) +
#  geom_line() + 
#  facet_wrap(. ~ area, scales = "free")

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
jagsUI::traceplot(postH, parameters = c("mu_beta0_pH","tau_beta0_pH",
                                        "beta0_pH","beta1_pH",
                                        "beta2_pH","beta3_pH"))

pH_mod <- 
  postH$mean$pH %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ay$year[1:42]),
         source = "model") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "pH") %>%
  left_join(postH$q2.5$pH %>%
              t() %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = unique(Hhat_ay$year[1:42]),
                     source = "model") %>%
              pivot_longer(-c(year, source), names_to = "area", values_to = "pH_lo95"),
            by = c("year","source","area")) %>%
  left_join(postH$q97.5$pH %>%
              t() %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = unique(Hhat_ay$year[1:42]),
                     source = "model") %>%
              pivot_longer(-c(year, source), names_to = "area", values_to = "pH_hi95"),
            by = c("year","source","area"))

pH_obs <- 
  (jags_dat$Hhat_ay/jags_dat$Chat_ay_pH) %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ay$year),
         source = "observed") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "pH")
rbind(pH_mod, pH_obs %>% mutate(pH_lo95 = NA, pH_hi95 = NA)) %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = pH, color = source)) +
  geom_ribbon(aes(ymin = pH_lo95, ymax = pH_hi95, borders = NA, fill = source), 
              alpha = 0.2, color = NA) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(. ~ area)

# ** slope of logit  --------------------------------------------------------
#data.frame(area = unique(H_ayg$area), lb = postH$q2.5$pH_slo, mean = postH$mean$pH_slo, ub = postH$q97.5$pH_slo) %>%
#  ggplot(aes(x = area, y = mean)) +
#  geom_pointrange(aes(y = mean, ymin = lb, ymax = ub))

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
  facet_wrap(. ~ area, scales = "free")

as.data.frame(
  rbind(t(jags_dat$Chat_ay_pH),
        t(apply(exp(postH$sims.list$logChat_ay), c(2,3), mean)))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(start_yr:end_yr, times = 2),
         source = rep(c("SWHS", "mean"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "C") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = C, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

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
  t(log(postH$mean$C_ay) + postH$mean$logbc_C) - t(log(jags_dat$Chat_ay_pH))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = start_yr:end_yr) %>%
  pivot_longer(!year, names_to = "area", values_to = "C_res") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  left_join(as.data.frame(t(jags_dat$cvChat_ay)) %>% setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = start_yr:end_yr) %>%
              pivot_longer(!year, names_to = "area", values_to = "cv") %>%
              mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)),
            by = c("year","area"))-> swhs_Cres
swhs_Cres %>%  ggplot(aes(x = year, y = C_res)) +
  scale_color_gradientn(colours = pal) + 
  geom_point(aes(color = cv)) +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(. ~ area, scales = "free_y")

area_codes
jagsUI::traceplot(postH, parameters = c("bc_C_offset"))
jagsUI::traceplot(postH, parameters = "logbc_C")
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

# * User comp --------------------------------------------------------
# ** mean by area --------------------------------------------------------
pG_H <- postH$sims.list$b1_pG_H / (postH$sims.list$b1_pG_H + postH$sims.list$b2_pG_H) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) 
pG_H %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "pG_H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = pG_H)) +
  geom_histogram(binwidth = 0.02) +
  facet_wrap(.~area)

pG_C <- postH$sims.list$b1_pG_C / (postH$sims.list$b1_pG_C + postH$sims.list$b2_pG_C) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) 
pG_C %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "pG_C") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = pG_C)) +
  geom_histogram(binwidth = 0.02) +
  facet_wrap(.~area)

# ** annual estimates  --------------------------------------------------------
pG_mod <- 
  postH$mean$pG_H %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ay$year),
         source = "model") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "pG_H") %>%
  left_join(postH$q2.5$pG_H %>%
              t() %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = unique(Hhat_ay$year),
                     source = "model") %>%
              pivot_longer(-c(year, source), names_to = "area", values_to = "pG_H_lo95"),
            by = c("year","source","area")) %>%
  left_join(postH$q97.5$pG_H %>%
              t() %>%
              as.data.frame() %>%
              setNames(nm = unique(H_ayg$area)) %>%
              mutate(year = unique(Hhat_ay$year),
                     source = "model") %>%
              pivot_longer(-c(year, source), names_to = "area", values_to = "pG_H_hi95"),
            by = c("year","source","area"))
pG_obs <- 
  (jags_dat$Hhat_ayg/jags_dat$Hhat_ay)[,35:Y] %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ayg$year),
         source = "observed") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "pG_H")

rbind(pG_mod, pG_obs%>% mutate(pG_H_lo95 = NA, pG_H_hi95 = NA)) %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = pG_H, color = source)) +
  geom_ribbon(aes(ymin = pG_H_lo95, ymax = pG_H_hi95, borders = NA, fill = source), 
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
  rbind(exp(postH$sims.list$mu_bc_H * postH$sims.list$bc_C_offset) %>%
          as.data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "bc") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
                 data = "C")) %>%
  rbind((postH$sims.list$bc_C_offset) %>%
          as.data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "bc") %>%
          mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE),
                 data = "C_offset")) -> bias

range(bias$bc) #YIKES!!!! 

bias %>% filter(bc < 6 & data != "C_offset") %>%
  ggplot(aes(x = bc, col = data, fill = data)) +
  geom_histogram(binwidth = .05, alpha = 0.2, position = "identity") +
  coord_cartesian(xlim = c(0, 3)) +
  geom_vline(aes(xintercept = 1)) +
  facet_wrap(.~area)

bias %>% filter(data == "C_offset") %>%
  ggplot(aes(x = bc, col = data, fill = data)) +
  geom_histogram(binwidth = .05, alpha = 0.2, position = "identity") +
  coord_cartesian(xlim = c(-6, 6)) +
  geom_vline(aes(xintercept = 1)) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  facet_wrap(.~area)

jagsUI::traceplot(postH, parameters = c("mu_bc_H","tau_bc_H","sd_bc_H"))
jagsUI::traceplot(postH, parameters = c("bc_C_offset"))

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
                    array(postH$sims.list$bc_C_offset, 
                          dim = c(dim(postH$sims.list$logbc_H)[1],A,Y))), c(2, 3), mean) %>%
          t() %>%
          as.data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = unique(Hhat_ay$year),
                 source = "model") %>%
          pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
          mutate(data = "C"))

dim(postH$q50$logbc_H)
dim(postH$q50$bc_C_offset)

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
  rbind(exp(postH$q50$logbc_H * array(postH$q50$bc_C_offset,
                                      dim = c(A,Y))) %>%
          t() %>%
          as.data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = unique(Hhat_ay$year),
                 source = "model") %>%
          pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
          mutate(data = "C"))

bc_obs <- 
  (jags_dat$Hhat_ayg/jags_dat$Hlb_ayg)[,35:Y] %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ayg$year),
         source = "observed") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
  mutate(data = "H") %>% 
  rbind((jags_dat$Chat_ayg/(jags_dat$Rlb_ayg + jags_dat$Hlb_ayg))[,35:Y] %>%
          t() %>%
          as.data.frame() %>%
          setNames(nm = unique(H_ayg$area)) %>%
          mutate(year = unique(Hhat_ayg$year),
                 source = "observed") %>%
          pivot_longer(-c(year, source), names_to = "area", values_to = "bc") %>%
          mutate(data = "C"))

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
  filter(data == "C") %>%
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

p_pelagic_obs %>%
  ggplot(aes(x = year, y = p_pelagic, color = user)) +
  geom_ribbon(data = p_pelagic_mod, aes(ymin = p_lo95, ymax = p_hi95, fill = user), 
              alpha = 0.2, color = NA) +
  geom_point(aes(shape = source)) +
  geom_line(data = p_pelagic_mod) +
  geom_hline(data = p_pelagic_trend, aes(yintercept = p_pelagic), linetype = 2) +
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
  jags_dat[grep("comp_", names(jags_dat), value = TRUE)] %>%
  as.data.frame() %>%
  mutate(year = comp_year + 1976,
         area = unique(H_ayg$area)[comp_area],
         user = ifelse(comp_user == 0, "charter", "private"),
         p_black = comp_black / comp_pelagic,
         area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  mutate(p_lo95 = NA, p_hi95 = NA)

p_black_trend <- data.frame(
    p_black = boot::inv.logit(
      unlist(mapply(function(x, y) rep(x, each = y), postH$mean$mu_beta0_black, c(4, 6, 6)))),
    area = unique(H_ayg$area))

rbind(p_black_obs) %>%
  ggplot(aes(x = year, y = p_black, color = user)) +
  geom_ribbon(data = p_black_mod, aes(ymin = p_lo95, ymax = p_hi95, fill = user), 
              alpha = 0.2, color = NA) +
  geom_point() +
  geom_line(data = p_black_mod) +
  geom_hline(data = p_black_trend, aes(yintercept = p_black), linetype = 2) +
  scale_alpha_manual(values = c(0.2, 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(. ~ area)

# ** yellow/non-pelagic annual  --------------------------------------------------------
jagsUI::traceplot(postH, parameters = c("mu_beta0_yellow","tau_beta0_yellow",
                                        "beta0_yellow","beta1_yellow",
                                        "beta2_yellow","beta3_yellow"))

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

p_yellow_mod %>% filter(p_yellow > p_hi50 | p_yellow < p_lo50) %>% print(n =100)
p_yellow_mod %>% filter(med_p > p_hi50 | med_p < p_lo50) %>% print(n =100)
p_yellow_mod %>% filter(p_yellow > p_hi95 | p_yellow < p_lo95) %>% print(n =100)

p_yellow_obs <-
  jags_dat[grep("comp_", names(jags_dat), value = TRUE)] %>%
  as.data.frame() %>%
  mutate(year = comp_year + 1976,
         area = unique(H_ayg$area)[comp_area],
         user = ifelse(comp_user == 0, "charter", "private"),
         p_yellow = comp_yellow / (comp_N - comp_pelagic),
         area = factor(area, unique(H_ayg$area), ordered = TRUE))

p_yellow_trend <-
  data.frame(
    p_yellow = boot::inv.logit(
      unlist(mapply(function(x, y) rep(x, each = y), postH$mean$mu_beta0_yellow, c(4, 6, 6)))),
    area = unique(H_ayg$area))

rbind(p_yellow_obs) %>%
  ggplot(aes(x = year, y = p_yellow, color = user)) +
  geom_ribbon(data = p_yellow_mod, aes(ymin = p_lo95, ymax = p_hi95, fill = user), 
              alpha = 0.15, color = NA) +
  geom_ribbon(data = p_yellow_mod, aes(ymin = p_lo50, ymax = p_hi50, fill = user), 
              alpha = 0.15, color = NA) +
  geom_point() +
  geom_line(data = p_yellow_mod) +
  geom_line(data = p_yellow_mod, aes(, y = med_p), linetype = 2, size = 0.5) +
  #geom_line(data = )
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
postH$mean$R_ay
postH$mean$Rb_ay
postH$mean$Ry_ay

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
  
  ggplot(aes(x = year, y = R, color = user)) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = R_lo95, ymax = R_hi95, fill = user), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") +
  geom_point(aes(x = year, y = R_how, color = user, shape = user)) +
  geom_errorbar(aes(x = year, ymin=lo95_R_how, ymax=hi95_R_how, color = user), width=.2,
                position=position_dodge(0.05))

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
# TODOs

# work on polynomial shape to p_yellow and other comp data.yellow Maybe spline? to allow
#      regulatory changes? 
# eg: logit(p_yellow[a, y, u]) <- beta0_yellow[a] + (beta1_yellow[a] * y) / (1 + beta2_yellow[a] * y) + re_yellow[a, y, u]
# logit(p_yellow[a, y, u]) <- beta0_yellow[a] + beta1_yellow[a] * y + beta2_yellow[a] * (u - 1) + beta3_yellow[a] * pow(y, 2) + beta4_yellow[a] * pow(y, 3) + re_yellow[a, y, u]

# plot releases
# plot SWHS and LB direct estimates 

# Spline YE proportions and experiment with different curves

#------------------------------------------------------------------------------


# BRF annual plots and compare to Howard methods:
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
  mutate(area = toupper(area)) -> arg

arg %>% filter(area == "EASTSIDE") %>% print(n = 150)

ggplot(data = arg, aes(x = year, y = H, color = user)) +
  geom_line() + 
  geom_ribbon(aes(x=year,ymin = H_lo95, ymax = H_hi95, fill = user), alpha = 0.25, color = NA) +
  facet_wrap(. ~ area, scales = "free") 

unique(H_ayg$area)
unique(Hhat_ayu$area) 
unique(Hhat_ay$area) 

H_ayg %>% filter(area == "eastside")
Hhat_ayu %>% filter(area == "eastside") %>% print(n = 30)
Hhat_ay %>% filter(area == "eastside") %>% print(n = 50)

with(comp, table(area_n, area))

comp %>% filter(area == "eastside") %>% print(n = 50)


ye_how %>% filter(area == "EASTSIDE",
                  user == "unguided",
                  year <= end_yr) %>% print(n = 100) ->yehow

arg %>% filter(area == "EASTSIDE",
               user == "unguided",
               year >= 1998) %>% print(n = 150)->yemod

plot(yehow$H_how ~ yemod$H)



