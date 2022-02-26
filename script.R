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

Chat_ay <- 
  readRDS(".//data//Chat_ay.rds") %>% 
  rename(Chat = C) %>%
  bind_rows(readRDS(".//data//Chat_ayu.rds") %>% 
              group_by(region, area, year) %>% 
              summarise(Chat = sum(C), seC = sqrt(sum(seC^2)))) %>%
  arrange(region, area, year) %>%
  mutate(Chat = ifelse(Chat == 0, 1, Chat), 
         seC = ifelse(seC == 0, 1, seC))

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
  facet_wrap(area ~ ., scales = "free")
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
rbind(S_ayu %>% mutate(pct_pel = pelagic_n / totalrf_n, source = "survey") %>% select(area, year, user, source, pct_pel),
      H_ayg %>% mutate(pct_pel = Hp / H, user = "charter", source = "logbook") %>% select(area, year, user, source, pct_pel)) %>%
  ggplot(aes(year, pct_pel, color = source)) +
  geom_line(aes(linetype = user)) +
  facet_wrap(area ~ ., scales = "free")

S_ayu %>% 
  mutate(pct_pel = black_n / totalrf_n, source = "survey") %>% select(area, year, user, source, pct_pel) %>%
  ggplot(aes(year, pct_pel)) +
  geom_line(aes(linetype = user)) +
  facet_wrap(area ~ ., scales = "free")

# Prep data for jags --------------------------------------------------------
Hhat_ayg <- Hhat_ayu %>% filter(user == "guided")
Hhat_ayp <- Hhat_ayu %>% filter(user == "private")
A = length(unique(Hhat_ay$area))
Y = length(unique(Hhat_ay$year))

C<- 16
Z <- bspline(1:24, K = C)

comp <-
  rbind(H_ayg %>% 
          mutate(area_n = as.numeric(area), user_n = 1, year_n = year - 1995, black = NA) %>% 
          select(year_n, area_n, user_n, N = H, pelagic = Hp, black),
        S_ayu %>% 
          mutate(area_n = as.numeric(area), user_n = ifelse(user == "charter", 1, 2), year_n = year - 1995) %>% 
          select(year_n, area_n, user_n, N = totalrf_n, pelagic = pelagic_n, black = black_n))

jags_dat <- 
  list(
    A = A, Y = Y, C = C,
    Hhat_ay = matrix(Hhat_ay$Hhat, nrow = A, ncol = Y, byrow = TRUE),
    cvHhat_ay = matrix(Hhat_ay$seH, nrow = A, ncol = Y, byrow = TRUE) /
      matrix(Hhat_ay$Hhat, nrow = A, ncol = Y, byrow = TRUE),
    Chat_ay = matrix(Chat_ay$Chat, nrow = A, ncol = Y, byrow = TRUE),
    cvChat_ay = matrix(Chat_ay$seC, nrow = A, ncol = Y, byrow = TRUE) /
      matrix(Chat_ay$Chat, nrow = A, ncol = Y, byrow = TRUE),
    H_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(H_ayg$year))),
                  matrix(H_ayg$H_lb, nrow = A, ncol = length(unique(H_ayg$year)), byrow = TRUE)),
    Hhat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
                     matrix(Hhat_ayg$Hhat, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)),
    cvHhat_ayg = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
                       matrix(Hhat_ayg$seH, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)) / 
      cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayg$year))),
            matrix(Hhat_ayg$Hhat, nrow = A, ncol = length(unique(Hhat_ayg$year)), byrow = TRUE)),
    Hhat_ayp = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayp$year))),
                     matrix(Hhat_ayp$Hhat, nrow = A, ncol = length(unique(Hhat_ayp$year)), byrow = TRUE)),
    cvHhat_ayp = cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayp$year))),
                       matrix(Hhat_ayp$seH, nrow = A, ncol = length(unique(Hhat_ayp$year)), byrow = TRUE)) /
      cbind(matrix(NA, nrow = A, ncol = Y - length(unique(Hhat_ayp$year))),
            matrix(Hhat_ayp$Hhat, nrow = A, ncol = length(unique(Hhat_ayp$year)), byrow = TRUE)),
    Z = Z,
    Q = makeQ(2, C),
    zero = rep(0, C),
    comp_area = comp$area_n,
    comp_year = comp$year_n,
    comp_user = comp$user_n,
    comp_N = comp$N,
    comp_pelagic = comp$pelagic,
    comp_black = comp$black,
    N = dim(comp)[1],
    regions = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3)
    )


# Run Jags --------------------------------------------------------
ni <- 1E3; nb <- ni*.25; nc <- 3; nt <- 1;
params <- parameters_alpha <- c("logbc", "mu_bc", "sd_bc",
                                "pU", "b1", "b2",
                                "pH", "pH_int", "pH_slo",
                                "p_pelagic", "beta0_pelagic", "beta1_pelagic", "beta2_pelagic", "mu_beta0_pelagic", "mu_beta1_pelagic", "mu_beta2_pelagic",
                                "p_black", "beta0_black", "beta1_black", "beta2_black", "mu_beta0_black", "mu_beta1_black", "mu_beta2_black",
                                "sd_comp", 
                                "Htrend_ay", "muHhat_ay", "beta", "H_ay", "sigma", "lambda") 

postH <- 
  jagsUI::jags(
    parameters.to.save = params,
    model.file = ".\\model_H.txt",
    data = jags_dat, 
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
postH$q2.5$pH_slo
postH$mean$pH_slo
postH$q97.5$pH_slo
postH$mean$pH_int

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

# ** sigma by area --------------------------------------------------------
postH$sims.list$sigma %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "bc") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = bc)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(.~area)

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
pU <- postH$sims.list$b1 / (postH$sims.list$b1 + postH$sims.list$b2) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) 
pU %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "pU") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = pU)) +
  geom_histogram(binwidth = 0.02) +
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
  facet_wrap(. ~ area)

# * SWHS bias --------------------------------------------------------
# ** mean by area --------------------------------------------------------
exp(postH$sims.list$mu_bc) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "bc") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = bc)) +
    geom_histogram(binwidth = 0.1) +
    coord_cartesian(xlim = c(0, 4)) +
    geom_vline(aes(xintercept = 1)) +
    facet_wrap(.~area)
# ** sd by area --------------------------------------------------------
postH$sims.list$sd_bc %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = where(is.numeric), names_to = "area", values_to = "bc") %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE)) %>%
  ggplot(aes(x = bc)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(.~area)

# ** annual estimates --------------------------------------------------------
mu_bc <- 
  data.frame(model = apply(exp(postH$sims.list$mu_bc), 2, mean),
             observed = apply(jags_dat$H_ayg/jags_dat$Hhat_ayg, 1, mean, na.rm = TRUE),
             stat = "mean",
             area = unique(H_ayg$area)) %>%
  pivot_longer(model:observed, names_to = "source", values_to = "bc")
bc_mod <- 
  apply(exp(postH$sims.list$logbc), c(2, 3), mean) %>%
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
data.frame(area = unique(H_ayg$area), lb = postH$q2.5$pH_slo, mean = postH$q2.5$pH_slo, ub = postH$q2.5$pH_slo) %>%
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
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user, source), names_to = "area", values_to = "p_pelagic")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))
p_pelagic_obs <-
  jags_dat[grep("comp_", names(jags_dat), value = TRUE)] %>%
    as.data.frame() %>%
    mutate(year = comp_year + 1995,
           area = unique(H_ayg$area)[comp_area],
           user = ifelse(comp_user == 1, "charter", "private"),
           p_pelagic = comp_pelagic / comp_N,
           area = factor(area, unique(H_ayg$area), ordered = TRUE))
rbind(p_pelagic_obs) %>%
  ggplot(aes(x = year, y = p_pelagic, color = user)) +
  geom_point() +
  geom_line(data = p_pelagic_mod) +
  scale_alpha_manual(values = c(0.2, 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(. ~ area)

# ** black/pelagic annual  --------------------------------------------------------
p_pelagic_mod <- 
  rbind(postH$mean$p_black[,,1] %>% t(),
        postH$mean$p_black[,,2] %>% t()) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year))),
         source = "model") %>%
  pivot_longer(-c(year, user, source), names_to = "area", values_to = "p_pelagic")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))
p_pelagic_obs <-
  jags_dat[grep("comp_", names(jags_dat), value = TRUE)] %>%
  as.data.frame() %>%
  mutate(year = comp_year + 1995,
         area = unique(H_ayg$area)[comp_area],
         user = ifelse(comp_user == 1, "charter", "private"),
         p_pelagic = comp_black / comp_pelagic,
         area = factor(area, unique(H_ayg$area), ordered = TRUE))
p_pelagic_trend <-
  rbind(boot::inv.logit(mapply(function(x, y) x + y * 1:Y, postH$mean$beta0_black, postH$mean$beta1_black)),
        boot::inv.logit(mapply(function(x, y, z) x + y * 1:Y + z, postH$mean$beta0_black, postH$mean$beta1_black, postH$mean$beta2_black))) %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  mutate(year = rep(unique(Hhat_ay$year), times = 2),
         user = rep(c("charter", "private"), each = length(unique(Hhat_ay$year)))) %>%
  pivot_longer(-c(year, user), names_to = "area", values_to = "p_pelagic")  %>%
  mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))
rbind(p_pelagic_obs) %>%
  ggplot(aes(x = year, y = p_pelagic, color = user)) +
  geom_point() +
  geom_line(data = p_pelagic_mod) +
  geom_line(data = p_pelagic_trend, linetype = 2) +
  scale_alpha_manual(values = c(0.2, 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(. ~ area)

