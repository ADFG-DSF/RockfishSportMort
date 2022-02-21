# Pelagic Harvest Composition -----------------------------------------------------
library(ggplot2)
library(tidyverse)
source(".\\functions.R")

# Read data --------------------------------------------------------
H_ayg <- readRDS(".//data//H_ayg.rds") %>% 
  mutate(H_lb = ifelse(H == 0, 1, H))

Hhat_ayu <- 
  readRDS(".//data//Hhat_ayu.rds")  %>% 
  mutate(Hhat = ifelse(H == 0, 1, H), 
         seH = ifelse(seH == 0, 1, seH)) %>%
  arrange(area, user, year)

Hhat_ay <- 
  readRDS(".//data//Hhat_ay.rds") %>% 
  rename(Hhat = H) %>%
  bind_rows(Hhat_ayu %>% 
              group_by(region, area, year) %>% 
              summarise(Hhat = sum(H), seH = sqrt(sum(seH^2)))) %>%
  arrange(region, area, year) %>%
  mutate(Hhat = ifelse(Hhat == 0, 1, Hhat), 
         seH = ifelse(seH == 0, 1, seH))

# Plot data --------------------------------------------------------
# * SWHS estimates by user --------------------------------------------------------
Hhat_ayu %>%
  select(year, user, area, region, Hhat, seH) %>%
  rbind(Hhat_ay %>% mutate(user = "all")) %>%
  ggplot(aes(year, Hhat, color = user)) +
  geom_line() +
  facet_wrap(area ~ ., scales = "free")
# * SWHS varibility by user --------------------------------------------------------
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
  facet_wrap(area ~ ., scales = "free")
left_join(H_ayg, Hhat_ay, by = c("area", "year", "region")) %>%
  ggplot(aes(H_lb, Hhat, color = area)) +
  geom_abline() +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x) +
  facet_grid(region ~ ., scales = "free")

# Prep data for jags --------------------------------------------------------
Hhat_ayg <- Hhat_ayu %>% filter(user == "guided")
Hhat_ayp <- Hhat_ayu %>% filter(user == "private")
A = length(unique(Hhat_ay$area))
Y = length(unique(Hhat_ay$year))

C<- 11
Z <- bspline(1:24, K = C)

jags_dat <- 
  list(
    A = A, Y = Y, C = C,
    Hhat_ay = matrix(Hhat_ay$Hhat, nrow = A, ncol = Y, byrow = TRUE),
    seHhat_ay = matrix(Hhat_ay$seH, nrow = A, ncol = Y, byrow = TRUE),
    H_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(H_ayg$year))),
                  matrix(H_ayg$H_lb, nrow = A, ncol = length(unique(H_ayg$year)), byrow = TRUE)),
    Hhat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
                     matrix(Hhat_ayg$Hhat, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)),
    seHhat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
                       matrix(Hhat_ayg$seH, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)),
    Hhat_ayp = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayp$year))),
                     matrix(Hhat_ayp$Hhat, nrow = A, ncol = length(unique(Hhat_ayp$year)), byrow = TRUE)),
    seHhat_ayp = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayp$year))),
                       matrix(Hhat_ayp$seH, nrow = A, ncol = length(unique(Hhat_ayp$year)), byrow = TRUE)),
    Z = Z,
    Q = makeQ(2, C),
    zero = rep(0, C)
    )


# Run Jags --------------------------------------------------------
ni <- 1E4; nb <- ni*.25; nc <- 3; nt <- 1;
params <- parameters_alpha <- c("bc", "mu_bc", "sd_bc",
                                "pU", "b1", "b2",
                                "Htrend_ay", "muHhat_ay", "beta", "H_ay", "sigma", "lambda") 

postH <- 
  jagsUI::jags(
    parameters.to.save = params,
    model.file = ".\\model_H.txt",
    data = jags_dat, 
    inits = list(list(bc = matrix(1, A, Y)), 
                 list(bc = matrix(1, A, Y)), 
                 list(bc = matrix(1, A, Y))),
    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
    store.data = TRUE)
postH
saveRDS(postH, ".\\postH.rds")
postH <- readRDS(".\\postH.rds")

# Inspect posterior --------------------------------------------------------
postH$mean$muHhat_ay
postH$mean$lambda
postH$mean$sigma
postH$mean$beta
postH$mean$H_ay

# * Harvest --------------------------------------------------------
# ** SWHS total harvest vrs. model total harvest --------------------------------------------------------
as.data.frame(
  rbind(t(jags_dat$Hhat_ay),
        t(postH$mean$Htrend_ay),
        t(postH$mean$H_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(1996:2019, times = 3),
         source = rep(c("SWHS", "trend", "H"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "H") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = H, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

# ** logbook harvest vrs. model total harvest -------------------------------------------------------- 
as.data.frame(
  rbind(t(jags_dat$H_ayg),
        t(postH$mean$H_ay),
        t(postH$mean$Htrend_ay))) %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(1996:2019, times = 3),
         source = rep(c("logbook", "Harvest", "trend"), each = Y)) %>%
  pivot_longer(!c(year, source), names_to = "area", values_to = "H") %>%
  mutate(yr_group = ifelse(year <= 1997, "no logbook", ifelse(year <= 2010, "No user", "full data")),
         area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = H, color = source)) +
  geom_line() + 
  facet_wrap(. ~ area, scales = "free")

# * Sex comp --------------------------------------------------------
# ** mean by area --------------------------------------------------------
temp <- postH$sims.list$b1 / (postH$sims.list$b1 + postH$sims.list$b2) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) 
temp %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "pU") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = pU)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(.~area)

# ** annual estimates  --------------------------------------------------------
pU_mod <- 
  postH$mean$pU %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ay$year),
         source = "model") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "pU")
pU_obs <- 
  (jags_dat$Hhat_ayp/jags_dat$Hhat_ay)[,16:24] %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ayg$year),
         source = "observed") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "pU")
rbind(pU_mod, pU_obs) %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = pU, color = source)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0, 1)) +
  geom_hline(aes(yintercept = bc, color = source), data = mu_bc) +
  facet_wrap(. ~ area)

# * SWHS bias --------------------------------------------------------
# ** mean by area --------------------------------------------------------
#Distribution of area bias estimates
postH$sims.list$mu_bc %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "bc") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = bc)) +
    geom_histogram(binwidth = 0.1) +
    coord_cartesian(xlim = c(0, 4)) +
    geom_vline(aes(xintercept = 1)) +
    facet_wrap(.~area)

# ** annual estimates --------------------------------------------------------
mu_bc <- 
  data.frame(model = postH$mean$mu_bc,
             observed = apply(jags_dat$H_ayg/jags_dat$Hhat_ayg, 1, mean, na.rm = TRUE),
             stat = "mean",
             area = unique(H_ayg$area)) %>%
  pivot_longer(model:observed, names_to = "source", values_to = "bc")
bc_mod <- 
  postH$mean$bc %>%
  t() %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = unique(Hhat_ay$year),
         source = "model") %>%
  pivot_longer(-c(year, source), names_to = "area", values_to = "bc")
bc_obs <- 
  (jags_dat$H_ayg/jags_dat$Hhat_ayg)[,16:24] %>%
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
  coord_cartesian(ylim = c(0, 5)) +
  geom_hline(aes(yintercept = bc, color = source), data = mu_bc) +
  facet_wrap(. ~ area)
postH$mean$bc
#observed
apply(jags_dat$H_ayg/jags_dat$Hhat_ayg, 1, mean, na.rm = TRUE)
postH$mean$mu_bc
