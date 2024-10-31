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

library(ggplot2)
library(tidyverse)
source(".\\scripts//functions.R")

# Read data --------------------------------------------------------
# Logbook harvests by area, year for guided trips
H_ayg <- readRDS(".//data//H_ayg.rds") %>% 
  mutate(H_lb = ifelse(H == 0, 1, H))

# SWHS harvests by area, year and user 
Hhat_ayu <- 
  readRDS(".//data//Hhat_ayu.rds")  %>% 
  mutate(Hhat = ifelse(H == 0, 1, H), 
         seH = ifelse(seH == 0, 1, seH)) %>%
  arrange(area, user, year)

# SWHS harvests by area, year
Hhat_ay <- 
  readRDS(".//data//Hhat_ay.rds") %>% 
  rename(Hhat = H) %>%
  bind_rows(Hhat_ayu %>% 
              group_by(region, area, year) %>% 
              summarise(Hhat = sum(H), seH = sqrt(sum(seH^2)))) %>%
  arrange(region, area, year) %>%
  mutate(Hhat = ifelse(Hhat == 0, 1, Hhat), 
         seH = ifelse(seH == 0, 1, seH))

# SWHS Catch by area, year
Chat_ay <- 
  readRDS(".//data//Chat_ay.rds") %>% 
  rename(Chat = C) %>%
  bind_rows(readRDS(".//data//Chat_ayu.rds") %>% 
              group_by(region, area, year) %>% 
              summarise(Chat = sum(C), seC = sqrt(sum(seC^2)))) %>%
  arrange(region, area, year) %>%
  mutate(Chat = ifelse(Chat == 0, 1, Chat), 
         seC = ifelse(seC == 0, 1, seC))

# Survey data on catch composition
S_ayu0 <- 
  readRDS(".//data//S_ayu.rds") 
S_ayu <- 
  S_ayu0 %>%
  bind_rows(data.frame(area = rep(unique(S_ayu0$area[S_ayu0$region %in% "Southeast"]), each = 4), 
                       year = rep(rep(1996:1997, each = 2), times = 6),
                       user = rep(c("charter", "private"), times = 12),
                       totalrf_n = 0, ye_n = NA, black_n = NA, pelagic_n = NA, nonpel_n = NA, notye_nonpel_n = NA)) %>%
  arrange(user, area, year) %>%
  filter(year >= 1996)


# Plot data --------------------------------------------------------
# * SWHS estimates by user --------------------------------------------------------
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

# Prep data for jags --------------------------------------------------------
Hhat_ayg <- Hhat_ayu %>% filter(user == "guided")
Hhat_ayp <- Hhat_ayu %>% filter(user == "private")
A = length(unique(Hhat_ay$area))
Y = length(unique(Hhat_ay$year))

C<- 5
Z <- bspline(1:24, K = C)

comp <- S_ayu %>% 
         mutate(area_n = as.numeric(area), 
          user_n = ifelse(user == "charter", 0, 1), 
          year_n = year - 1995, 
          source = 1) %>% 
         select(year_n, area_n, user_n, source, N = totalrf_n, pelagic = pelagic_n, black = black_n, yellow = ye_n) %>%
         filter(N != 0) %>%
         mutate(yellow = ifelse(N - pelagic ==0, NA, yellow))
  

matrix_Hhat_ay <- matrix(Hhat_ay$Hhat, nrow = A, ncol = Y, byrow = TRUE)
matrix_Hhat_ay[4, 1:5] <- NA  #what's up with this? assuming bad data?
matrix_Chat_ay <- matrix(Chat_ay$Chat, nrow = A, ncol = Y, byrow = TRUE)
matrix_Chat_ay[4, 1:5] <- NA

jags_dat <- 
  list(
    A = A, Y = Y, C = C,
    Hhat_ay = matrix_Hhat_ay,
    cvHhat_ay = matrix(Hhat_ay$seH, nrow = A, ncol = Y, byrow = TRUE) /
      matrix(Hhat_ay$Hhat, nrow = A, ncol = Y, byrow = TRUE),
    Chat_ay = matrix_Chat_ay,
    cvChat_ay = matrix(Chat_ay$seC, nrow = A, ncol = Y, byrow = TRUE) /
      matrix(Chat_ay$Chat, nrow = A, ncol = Y, byrow = TRUE),
    # logbook rf lbs harvested by guides
    Hlb_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(H_ayg$year))),
                  matrix(H_ayg$H_lb, nrow = A, ncol = length(unique(H_ayg$year)), byrow = TRUE)),
    # logbook pelagic rf lbs harvested by guides
    Hlbp_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(H_ayg$year))),
                    matrix(H_ayg$Hp, nrow = A, ncol = length(unique(H_ayg$year)), byrow = TRUE)),
    # logbook ye rf harvested by guides
    Hlby_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(H_ayg$year))),
                    matrix(H_ayg$Hye, nrow = A, ncol = length(unique(H_ayg$year)), byrow = TRUE)),
    # SWHS estimates of rockfish harvests
    Hhat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
                     matrix(Hhat_ayg$Hhat, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)),
    # cv of SWHS estimates
    cvHhat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
                       matrix(Hhat_ayg$seH, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)) / 
      cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
            matrix(Hhat_ayg$Hhat, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)),
    Z = Z, #spline
    Q = makeQ(2, C), #spline stuff
    zero = rep(0, C),
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

jags_dat$Hlb_ayg   
jags_dat$Hlbp_ayg
jags_dat$Hlby_ayg
# Run Jags --------------------------------------------------------
ni <- 5E3; nb <- ni*.5; nc <- 3; nt <- 10;
params <- c("logbc", "mu_bc", "sd_bc",
            "pG", "b1_pG", "b2_pG",
            "pH", "pH_int", "pH_slo",
            "p_pelagic", "beta0_pelagic", "beta1_pelagic", "beta2_pelagic", "beta3_pelagic", "mu_beta0_pelagic", "tau_beta0_pelagic",
            "p_yellow", "beta0_yellow", "beta1_yellow", "beta2_yellow", "beta3_yellow", "mu_beta0_yellow", "tau_beta0_yellow",
            "p_black", "beta0_black", "beta1_black", "beta2_black", "mu_beta0_black", "tau_beta0_black",
            "re_pelagic", "sd_comp", 
             "Htrend_ay", "H_ay", "sigma", "lambda", "H_ayg", "H_ayu", 
             "Hb_ayg", "Hb_ayu", "Hb_ay",
             "Hy_ayg", "Hy_ayu", "Hy_ay",
             "logHhat_ay") 

postH <- 
  jagsUI::jags(
    parameters.to.save = params,
    model.file = ".\\models\\model_H.txt",
    data = jags_dat, 
    #inits = list(list(muHhat_ay = log(jags_dat$H_ayg * 1.2)), list(muHhat_ay = log(jags_dat$H_ayg * 1.2)), list(muHhat_ay = log(jags_dat$H_ayg * 1.2))),
    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
    store.data = TRUE)
postH

rhat <- get_Rhat(postH, cutoff = 1.15)
rhat
jagsUI::traceplot(postH, Rhat_min = 1.1)
#saveRDS(postH, ".\\posts\\postH.rds")
postH <- readRDS(".\\posts\\postH.rds")

# Inspect posterior --------------------------------------------------------
postH$mean$lambda
postH$mean$sigma
postH$mean$H_ay
postH$q2.5$pH_slo
postH$mean$pH_slo
postH$q97.5$pH_slo
postH$mean$sd_comp
paste0(round(postH$mean$beta1_pelagic, 3), "(", round(postH$q2.5$beta1_pelagic, 3), ", ", round(postH$q97.5$beta1_pelagic, 3), ")")
paste0(round(postH$mean$beta2_pelagic, 3), "(", round(postH$q2.5$beta2_pelagic, 3), ", ", round(postH$q97.5$beta2_pelagic, 3), ")")


# * Harvest --------------------------------------------------------
# ** SWHS total harvest vrs. model total harvest --------------------------------------------------------
as.data.frame(
  rbind(t(jags_dat$Hhat_ay),
        t(apply(exp(postH$sims.list$Htrend_ay), c(2,3), mean)),
        t(postH$mean$H_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(1996:2019, times = 3),
         source = rep(c("SWHS", "trend", "H"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = H, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

# ** observation error --------------------------------------------------------
as.data.frame(
  rbind(t(jags_dat$Hhat_ay),
        t(apply(exp(postH$sims.list$logHhat_ay), c(2,3), mean)))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
   mutate(year = rep(1996:2019, times = 2),
          source = rep(c("SWHS", "mean"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = H, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

# ** logbook harvest vrs. model total harvest -------------------------------------------------------- 
as.data.frame(
  rbind(t(jags_dat$Hlb_ayg),
        t(postH$mean$H_ay),
        t(apply(exp(postH$sims.list$Htrend_ay), c(2,3), mean)),
        t(jags_dat$Hhat_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(1996:2019, times = 4),
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
  mutate(year = rep(1996:2019, times = 4),
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
  mutate(year = 1996:2019) %>%
  pivot_longer(!year, names_to = "area", values_to = "res") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = res)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(. ~ area, scales = "free_y")

# ** SWHS residuals --------------------------------------------------------
as.data.frame(
  t(log(postH$mean$H_ay) + postH$mean$logbc) - t(log(jags_dat$Hhat_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = 1996:2019) %>%
  pivot_longer(!year, names_to = "area", values_to = "res") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = res)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(. ~ area, scales = "free_y")

# ** yelloweye logbook harvest vrs. model charter harvest --------------------------------------------------------
as.data.frame(
  rbind(t(postH$mean$Hy_ayg),
        t(jags_dat$Hlby_ayg))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(1996:2019, times = 2),
         source = rep(c("model", "logbook"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = H, linetype = source)) +
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
  pivot_longer(-c(year, source), names_to = "area", values_to = "pG")
pG_obs <- 
  (jags_dat$Hhat_ayg/jags_dat$Hhat_ay)[,16:24] %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ayg$year),
         source = "observed") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "pG")
rbind(pG_mod, pG_obs) %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = pG, color = source)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(. ~ area)

# * SWHS bias --------------------------------------------------------
# ** mean by area --------------------------------------------------------
exp(postH$sims.list$mu_bc) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "bc") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = bc)) +
  geom_histogram(binwidth = .1) +
  coord_cartesian(xlim = c(0, 2)) +
  geom_vline(aes(xintercept = 1)) +
  facet_wrap(.~area)
# ** sd by area --------------------------------------------------------
postH$sims.list$sd_bc %>%
  as.data.frame() %>%
  setNames(nm = unique(Hhat_ay$area)) %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "se_bc") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = se_bc)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(.~area) + coord_cartesian(xlim = c(0, 1))
# ** annual estimates --------------------------------------------------------
mu_bc <- data.frame(area = unique(H_ayg$area), mu_bc = apply(exp(postH$sims.list$mu_bc), 2, mean))
bc_mod <- 
  apply(exp(postH$sims.list$logbc), c(2, 3), mean) %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ay$year),
         source = "model") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "bc")
bc_obs <- 
  (jags_dat$Hhat_ayg/jags_dat$Hlb_ayg)[,16:24] %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ayg$year),
         source = "observed") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "bc")
rbind(bc_mod, bc_obs) %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = bc, color = source)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0, 2)) +
  geom_hline(aes(yintercept = mu_bc), data = mu_bc) +
  facet_wrap(. ~ area)

# * P(Harvested) --------------------------------------------------------
# ** annual estimates  --------------------------------------------------------
pH_mod <- 
  postH$mean$pH %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ay$year),
         source = "model") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "pH")
pH_obs <- 
  (jags_dat$Hhat_ay/jags_dat$Chat_ay) %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ay$year),
         source = "observed") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "pH")
rbind(pH_mod, pH_obs) %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = pH, color = source)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(. ~ area)

# ** slope of logit  --------------------------------------------------------
data.frame(area = unique(H_ayg$area), lb = postH$q2.5$pH_slo, mean = postH$mean$pH_slo, ub = postH$q97.5$pH_slo) %>%
  ggplot(aes(x = area, y = mean)) +
    geom_pointrange(aes(y = mean, ymin = lb, ymax = ub))


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
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))
p_pelagic_obs <-
  comp %>%
    mutate(year = year_n + 1995,
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
         select(year, area, user, source, p_pelagic))
p_pelagic_trend <-
  data.frame(
    p_pelagic = boot::inv.logit(
      unlist(mapply(function(x, y) rep(x, each = y), postH$mean$mu_beta0_pelagic, c(4, 6, 6)))),
    area = unique(H_ayg$area))
p_pelagic_obs %>%
  ggplot(aes(x = year, y = p_pelagic, color = user)) +
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
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))
p_black_obs <-
  jags_dat[grep("comp_", names(jags_dat), value = TRUE)] %>%
  as.data.frame() %>%
  mutate(year = comp_year + 1995,
         area = unique(H_ayg$area)[comp_area],
         user = ifelse(comp_user == 0, "charter", "private"),
         p_black = comp_black / comp_pelagic,
         area = factor(area, unique(H_ayg$area), ordered = TRUE))
p_black_trend <-
  data.frame(
    p_black = boot::inv.logit(
      unlist(mapply(function(x, y) rep(x, each = y), postH$mean$mu_beta0_black, c(4, 6, 6)))),
    area = unique(H_ayg$area))
rbind(p_black_obs) %>%
  ggplot(aes(x = year, y = p_black, color = user)) +
  geom_point() +
  geom_line(data = p_black_mod) +
  geom_hline(data = p_black_trend, aes(yintercept = p_black), linetype = 2) +
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
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))
p_yellow_obs <-
  jags_dat[grep("comp_", names(jags_dat), value = TRUE)] %>%
  as.data.frame() %>%
  mutate(year = comp_year + 1995,
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
  geom_point() +
  geom_line(data = p_yellow_mod) +
  geom_hline(data = p_yellow_trend, aes(yintercept = p_yellow), linetype = 2) +
  scale_alpha_manual(values = c(0.2, 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(. ~ area)

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

# * Black Rockfish Harvest --------------------------------------------------------
# ** SWHS total harvest vrs. model total harvest --------------------------------------------------------
as.data.frame(
  rbind(t(postH$mean$Hb_ayg),
        t(postH$mean$Hb_ayu),
        t(postH$mean$Hb_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(1996:2019, times = 3),
         user = rep(c("guided", "unguided", "All"), each = Y)) %>%
  pivot_longer(!c(year,user), names_to = "area", values_to = "H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = H, color = user)) +
    geom_line() + 
    facet_wrap(. ~ area, scales = "free")



