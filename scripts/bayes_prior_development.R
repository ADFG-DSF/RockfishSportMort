gs <- rgamma(10000,0.5,1); hist(gs,breaks = 100)

mu_lam <- rnorm(10000,0.5,0.1)
sig_lam<- runif(10000,0,5)

a_lam <- mu_lam^2 / sig_lam^2
b_lam <- mu_lam / sig_lam^2

gs <- vector()
for (i in 1:10000) {
  gs[i] <- rgamma(1,a_lam[i],b_lam[i])
}
hist(gs,breaks = 100)

#-------------------------------------------------------------------------------
# gamma priors for uR

alpha_p <-rgamma(10000, 1, 0.1)
beta_p <-rgamma(10000,1,0.1)

alpha_p <-exp(dnorm(10000, 0, 1))
beta_p <-exp(dnorm(10000, 0, 1))

gsim <- rgamma(10000,alpha_p,beta_p)

{par(mfrow = c(3,1))
hist(alpha_p, breaks = 100)
hist(beta_p, breaks = 100)
hist(gsim, breaks = 1000)}

#- Proportional inflation of R with lognormal
try <- rlnorm(10000,1,1); hist(try, breaks = 100)

exp(10)

R_lb <- round(runif(100,5,100))
mu_bc_R <- rnorm(100,1,0.001); hist(mu_bc_R, breaks = 100)
mu_bc_R <- log(sample(mu_bc_R,100,replace = F)); hist(mu_bc_R, breaks = 100)

mu_bc_R <- rgamma(100,1,100); hist(mu_bc_R, breaks = 100)

sd <- rgamma(100,0.001,0.001)
tau_bc_R <- 1/sd/sd; mean(tau_bc_R)
tau_bc_R <- 0.5 #mean(tau_bc_R)

tau_bc_R <- rgamma(100,0.01, 0.01); mean(tau_bc_R)

#logbc_R <- rnorm(100,mu_bc_R,tau_bc_R); hist(logbc_R, breaks = 100)

#logbc_R <- rnorm(100,mu_bc_R,tau_bc_R); hist(logbc_R, breaks = 100)
logbc_R <- vector()
for (i in 1:100) {
  logbc_R[i] <- rlnorm(1,mu_bc_R[i],tau_bc_R[i])
}; hist(logbc_R, breaks = 100)

#logbc_R <- rlnorm(100,mu_bc_R,tau_bc_R); hist(logbc_R, breaks = 100)

exp(log(10) + log(2))

mean(logbc_R)

tR <- exp(log(R_lb) + logbc_R)

R_lb_adj <- R_lb / tR ; hist(R_lb_adj, breaks = 50)

hist(tR, breaks = 100)
length(R_lb_adj[R_lb_adj<1])/length(R_lb_adj)
hist(R_lb_adj, breaks = 100)

#------------------------------------------------------------------------------
# pH Logistic curves
# best model runs: 
m1 <- "HR_fitLBR_thru2023_1600000_2024-12-11"
m2 <- "HR_censLBR_thru2023_1600000_2024-12-11"
m3 <- "HR_hybLBR_thru2023_1600000_2024-12-11"

post_m1 <- readRDS(paste0(".\\output\\bayes_posts\\",m1,".rds"))
post_m2 <- readRDS(paste0(".\\output\\bayes_posts\\",m2,".rds"))
post_m3 <- readRDS(paste0(".\\output\\bayes_posts\\",m3,".rds"))

post_m1$sims.list$beta0_pH %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pH")-> fit_beta0_pH 
post_m1$sims.list$beta1_pH %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pH")-> fit_beta1_pH
post_m1$sims.list$beta2_pH %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pH")-> fit_beta2_pH
post_m1$sims.list$beta3_pH %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pH")-> fit_beta3_pH

post_m2$sims.list$beta0_pH %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pH")-> cens_beta0_pH
post_m2$sims.list$beta1_pH %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pH")-> cens_beta1_pH
post_m2$sims.list$beta2_pH %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pH")-> cens_beta2_pH
post_m2$sims.list$beta3_pH %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pH")-> cens_beta3_pH

post_m3$sims.list$beta0_pH %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pH")-> hyb_beta0_pH
post_m3$sims.list$beta1_pH %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pH")-> hyb_beta1_pH
post_m3$sims.list$beta2_pH %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pH")-> hyb_beta2_pH
post_m3$sims.list$beta3_pH %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pH")-> hyb_beta3_pH

rbind(fit_beta0_pH %>% mutate(model = "fit"),
      cens_beta0_pH %>% mutate(model = "cens"),
      hyb_beta0_pH %>% mutate(model = "hyb")) -> pH_b0_mods

rbind(fit_beta1_pH %>% mutate(model = "fit"),
      cens_beta1_pH %>% mutate(model = "cens"),
      hyb_beta1_pH %>% mutate(model = "hyb")) -> pH_b1_mods

rbind(fit_beta2_pH %>% mutate(model = "fit"),
      cens_beta2_pH %>% mutate(model = "cens"),
      hyb_beta2_pH %>% mutate(model = "hyb")) -> pH_b2_mods

rbind(fit_beta3_pH %>% mutate(model = "fit"),
      cens_beta3_pH %>% mutate(model = "cens"),
      hyb_beta3_pH %>% mutate(model = "hyb")) -> pH_b3_mods

n <- 1000
Y <- 47





{
  tau_mu0 <- 0.5
  mu_beta0 <- rnorm(n,-0.25,1/sqrt(tau_mu0)) #rnorm(n,-0.25,1)
tau_beta0 <- rgamma(n,0.001,0.001)

beta0 <- rnorm(n*3,mu_beta0, tau_beta0) #INTERCEPT
beta0 <- beta0[beta0 > -4]; beta0 <- sample(beta0,n,replace = F)

tau1 <- 0.25
beta1 <- rnorm(3*n,0, 1/sqrt(tau1))
beta1 <- beta1[beta1 > 0]; beta1 <- sample(beta1,n,replace = F) # SCALING FACTOR (amplitude) #rep(2,n)

tau2 <- 5
beta2 <- rnorm(n*3,0,  1/sqrt(tau2)) #0.1
beta2 <- beta2[beta2 > 0]; beta2 <- sample(beta2,n,replace = F) #T(0,)          # SLOPE

tau3 <- 0.01#sd = 10
beta3 <- rnorm(n*3,30,  1/sqrt(tau3)); beta3 <- beta3[beta3 > 0 & beta3 < Y-3]; beta3 <- sample(beta3,n,replace = F) #T(0,Y-3)    # INFLECTION POINT 

cbind(beta0,beta1,beta2,beta3) %>% data.frame -> pr_sim
props <- data.frame(matrix(nrow = n, ncol=Y)) 
colnames(props) <- seq(1,Y,1)
  
for (i in 1:n) { #i <- 1
  for (y in 1:Y){
    props[i,y] <- logit_to_prob(pr_sim$beta0[i] +
                                  pr_sim$beta1[i] / (1 + exp(-pr_sim$beta2[i] * (y - pr_sim$beta3[i]))))
  }
}

props %>% mutate(iter = row_number()) %>%
  pivot_longer(cols = seq(1,Y,1),
                       names_to = "year",
                       values_to = "prop") %>%
  mutate(year = as.numeric(year)) %>%
  data.frame() -> props
  str(props)
  
props %>% group_by(year) %>%
  dplyr::summarise(mean_prop = mean(prop),
                   lo50 = as.numeric(quantile(prop,c(0.25))),
                   hi50 = as.numeric(quantile(prop,c(0.75))),
                   lo95 = as.numeric(quantile(prop,c(0.025))),
                   hi95 = as.numeric(quantile(prop,c(0.975))))  -> mean

ggplot(props,aes(x = year, y = prop)) +
  geom_ribbon(data = mean, 
              aes(y = mean_prop, ymin = lo95,ymax = hi95),color = NA, alpha = 0.2) +
  geom_ribbon(data = mean, aes(y = mean_prop, ymin = lo50,ymax = hi50),color = NA, alpha = 0.2) +
  geom_line(aes(group = iter),alpha = 0.05, color = "blue") +  # Adjust alpha for transparency
  geom_line(data = mean, aes(x = year, y = mean_prop)) + #,aes(x = year, y = prop)) +
  labs(
    title = "Lines for Each Row in props",
    x = "y",
    y = "Probability"
  ) + ylim(0,1) +
  theme_minimal()
}

range(pr_sim$beta0)

ggplot(pr_sim %>% filter(beta0 > -20 & beta0 < 15)) +
  geom_histogram(aes(beta0, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = pH_b0_mods, 
                 aes(pH, y = ..density.. ,fill = area, color = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)
  
ggplot(pr_sim) +
  geom_histogram(aes(beta1, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = pH_b1_mods, 
                 aes(pH, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta2, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = pH_b2_mods, 
                 aes(pH, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)
 
ggplot(pr_sim) +
  geom_histogram(aes(beta3, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = pH_b3_mods, 
                 aes(pH, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

#-------------------------------------------------------------------------------
#yelloweye logistic priors:

post_m3$sims.list$beta0_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow")-> hyb_beta0_yellow
post_m3$sims.list$beta1_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow")-> hyb_beta1_yellow
post_m3$sims.list$beta2_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow")-> hyb_beta2_yellow
post_m3$sims.list$beta3_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow")-> hyb_beta3_yellow
post_m1$sims.list$beta0_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow")-> fit_beta0_yellow 
post_m1$sims.list$beta1_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow")-> fit_beta1_yellow
post_m1$sims.list$beta2_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow")-> fit_beta2_yellow
post_m1$sims.list$beta3_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow")-> fit_beta3_yellow

post_m2$sims.list$beta0_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow")-> cens_beta0_yellow
post_m2$sims.list$beta1_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow")-> cens_beta1_yellow
post_m2$sims.list$beta2_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow")-> cens_beta2_yellow
post_m2$sims.list$beta3_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow")-> cens_beta3_yellow

rbind(fit_beta0_yellow %>% mutate(model = "fit"),
      cens_beta0_yellow %>% mutate(model = "cens"),
      hyb_beta0_yellow %>% mutate(model = "hyb")) -> yellow_b0_mods

rbind(fit_beta1_yellow %>% mutate(model = "fit"),
      cens_beta1_yellow %>% mutate(model = "cens"),
      hyb_beta1_yellow %>% mutate(model = "hyb")) -> yellow_b1_mods

rbind(fit_beta2_yellow %>% mutate(model = "fit"),
      cens_beta2_yellow %>% mutate(model = "cens"),
      hyb_beta2_yellow %>% mutate(model = "hyb")) -> yellow_b2_mods

rbind(fit_beta3_yellow %>% mutate(model = "fit"),
      cens_beta3_yellow %>% mutate(model = "cens"),
      hyb_beta3_yellow %>% mutate(model = "hyb")) -> yellow_b3_mods

n <- 1000
Y <- 47

{
  tau_mu0 <- 0.25
  mu_beta0 <- rnorm(n,0,1/sqrt(tau_mu0)) #rnorm(n,-0.25,1)
  tau_beta0 <- rgamma(n,0.001,0.001)
  
  beta0 <- rnorm(n*3,mu_beta0, tau_beta0) #INTERCEPT
  beta0 <- beta0[beta0 > -5]; beta0 <- sample(beta0,n,replace = F)
  
  tau1 <- 0.1
  beta1 <- rnorm(n,0, 1/sqrt(tau1))
  #beta1 <- beta1[beta1 < 1]; beta1 <- sample(beta1,n,replace = F) # SCALING FACTOR (amplitude) #rep(2,n)
  
  tau2 <- 0.025
  beta2 <- rnorm(n,-5,  1/sqrt(tau2)) #0.1
  #beta2 <- beta2[beta2 < 1]; beta2 <- sample(beta2,n,replace = F) #T(0,)          # SLOPE
  
  tau3 <- 0.025#sd = 10
  beta3 <- rnorm(n*3,30,  1/sqrt(tau3)); beta3 <- beta3[beta3 > 0 & beta3 < Y-3]; beta3 <- sample(beta3,n,replace = F) #T(0,Y-3)    # INFLECTION POINT 
  
  cbind(beta0,beta1,beta2,beta3) %>% data.frame -> pr_sim
  props <- data.frame(matrix(nrow = n, ncol=Y)) 
  colnames(props) <- seq(1,Y,1)
  
  for (i in 1:n) { #i <- 1
    for (y in 1:Y){
      props[i,y] <- logit_to_prob(pr_sim$beta0[i] +
                                    pr_sim$beta1[i] / (1 + exp(-pr_sim$beta2[i] * (y - pr_sim$beta3[i]))))
    }
  }
  
  props %>% mutate(iter = row_number()) %>%
    pivot_longer(cols = seq(1,Y,1),
                 names_to = "year",
                 values_to = "prop") %>%
    mutate(year = as.numeric(year)) %>%
    data.frame() -> props
  str(props)
  
  props %>% group_by(year) %>%
    dplyr::summarise(mean_prop = mean(prop),
                     lo50 = as.numeric(quantile(prop,c(0.25))),
                     hi50 = as.numeric(quantile(prop,c(0.75))),
                     lo95 = as.numeric(quantile(prop,c(0.025))),
                     hi95 = as.numeric(quantile(prop,c(0.975))))  -> mean
  
  ggplot(props,aes(x = year, y = prop)) +
    geom_ribbon(data = mean, 
                aes(y = mean_prop, ymin = lo95,ymax = hi95),color = NA, alpha = 0.2) +
    geom_ribbon(data = mean, aes(y = mean_prop, ymin = lo50,ymax = hi50),color = NA, alpha = 0.2) +
    geom_line(aes(group = iter),alpha = 0.05, color = "blue") +  # Adjust alpha for transparency
    geom_line(data = mean, aes(x = year, y = mean_prop)) + #,aes(x = year, y = prop)) +
    labs(
      title = "Lines for Each Row in props",
      x = "y",
      y = "Probability"
    ) + ylim(0,1) +
    theme_minimal()
}

ggplot(pr_sim %>% filter(beta0 > -20 & beta0 < 15)) +
  geom_histogram(aes(beta0, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_b0_mods, 
                 aes(yellow, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta1, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_b1_mods, 
                 aes(yellow, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta2, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_b2_mods, 
                 aes(yellow, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta3, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_b3_mods, 
                 aes(yellow, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

#-------------------------------------------------------------------------------
#yelloweye_X logistic priors:
post_m1$sims.list$beta0_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x")-> fit_beta0_yellow_x 
post_m1$sims.list$beta1_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x")-> fit_beta1_yellow_x
post_m1$sims.list$beta2_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x")-> fit_beta2_yellow_x
post_m1$sims.list$beta3_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x")-> fit_beta3_yellow_x

post_m2$sims.list$beta0_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x")-> cens_beta0_yellow_x
post_m2$sims.list$beta1_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x")-> cens_beta1_yellow_x
post_m2$sims.list$beta2_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x")-> cens_beta2_yellow_x
post_m2$sims.list$beta3_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x")-> cens_beta3_yellow_x

post_m3$sims.list$beta0_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x")-> hyb_beta0_yellow_x
post_m3$sims.list$beta1_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x")-> hyb_beta1_yellow_x
post_m3$sims.list$beta2_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x")-> hyb_beta2_yellow_x
post_m3$sims.list$beta3_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x")-> hyb_beta3_yellow_x

rbind(fit_beta0_yellow_x %>% mutate(model = "fit"),
      cens_beta0_yellow_x %>% mutate(model = "cens"),
      hyb_beta0_yellow_x %>% mutate(model = "hyb")) -> yellow_x_b0_mods

rbind(fit_beta1_yellow_x %>% mutate(model = "fit"),
      cens_beta1_yellow_x %>% mutate(model = "cens"),
      hyb_beta1_yellow_x %>% mutate(model = "hyb")) -> yellow_x_b1_mods

rbind(fit_beta2_yellow_x %>% mutate(model = "fit"),
      cens_beta2_yellow_x %>% mutate(model = "cens"),
      hyb_beta2_yellow_x %>% mutate(model = "hyb")) -> yellow_x_b2_mods

rbind(fit_beta3_yellow_x %>% mutate(model = "fit"),
      cens_beta3_yellow_x %>% mutate(model = "cens"),
      hyb_beta3_yellow_x %>% mutate(model = "hyb")) -> yellow_x_b3_mods

n <- 1000
Y <- 47

{
  tau_mu0 <- 0.1
  mu_beta0 <- rnorm(n,0,1/sqrt(tau_mu0)) #rnorm(n,-0.25,1)
  tau_beta0 <- rgamma(n,0.001,0.001)
  
  beta0 <- rnorm(n*3,mu_beta0, tau_beta0) #INTERCEPT
  beta0 <- beta0[beta0 > -2.75]; beta0 <- sample(beta0,n,replace = F)
  
  tau1 <- 0.25
  beta1 <- rnorm(n,0, 1/sqrt(tau1))
  # beta1 <- beta1[beta1 > 0]; beta1 <- sample(beta1,n,replace = F) # SCALING FACTOR (amplitude) #rep(2,n)
  
  tau2 <- 1
  beta2 <- rnorm(n,0,  1/sqrt(tau2)) #0.1
  #beta2 <- beta2[beta2 > 0]; beta2 <- sample(beta2,n,replace = F) #T(0,)          # SLOPE
  
  tau3 <- 0.1#sd = 10
  beta3 <- rnorm(n*3,30,  1/sqrt(tau3)); beta3 <- beta3[beta3 > 0 & beta3 < Y-3]; beta3 <- sample(beta3,n,replace = F) #T(0,Y-3)    # INFLECTION POINT 
  
  cbind(beta0,beta1,beta2,beta3) %>% data.frame -> pr_sim
  props <- data.frame(matrix(nrow = n, ncol=Y)) 
  colnames(props) <- seq(1,Y,1)
  
  for (i in 1:n) { #i <- 1
    for (y in 1:Y){
      props[i,y] <- logit_to_prob(pr_sim$beta0[i] +
                                    pr_sim$beta1[i] / (1 + exp(-pr_sim$beta2[i] * (y - pr_sim$beta3[i]))))
    }
  }
  
  props %>% mutate(iter = row_number()) %>%
    pivot_longer(cols = seq(1,Y,1),
                 names_to = "year",
                 values_to = "prop") %>%
    mutate(year = as.numeric(year)) %>%
    data.frame() -> props
  str(props)
  
  props %>% group_by(year) %>%
    dplyr::summarise(mean_prop = mean(prop),
                     lo50 = as.numeric(quantile(prop,c(0.25))),
                     hi50 = as.numeric(quantile(prop,c(0.75))),
                     lo95 = as.numeric(quantile(prop,c(0.025))),
                     hi95 = as.numeric(quantile(prop,c(0.975))))  -> mean
  
  ggplot(props,aes(x = year, y = prop)) +
    geom_ribbon(data = mean, 
                aes(y = mean_prop, ymin = lo95,ymax = hi95),color = NA, alpha = 0.2) +
    geom_ribbon(data = mean, aes(y = mean_prop, ymin = lo50,ymax = hi50),color = NA, alpha = 0.2) +
    geom_line(aes(group = iter),alpha = 0.05, color = "blue") +  # Adjust alpha for transparency
    geom_line(data = mean, aes(x = year, y = mean_prop)) + #,aes(x = year, y = prop)) +
    labs(
      title = "Lines for Each Row in props",
      x = "y",
      y = "Probability"
    ) + ylim(0,1) +
    theme_minimal()
}

pr_sim %>% filter(beta0 < -10)

ggplot(pr_sim %>% filter(beta0 > -20 & beta0 < 15)) +
  geom_histogram(aes(beta0, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_x_b0_mods, 
                 aes(yellow_x, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta1, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_x_b1_mods, 
                 aes(yellow_x, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta2, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_x_b2_mods, 
                 aes(yellow_x, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta3, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_x_b3_mods, 
                 aes(yellow_x, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

#-------------------------------------------------------------------------------
#pelagic logistic priors:
post_m1$sims.list$beta0_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic")-> fit_beta0_pelagic 
post_m1$sims.list$beta1_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic")-> fit_beta1_pelagic
post_m1$sims.list$beta2_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic")-> fit_beta2_pelagic
post_m1$sims.list$beta3_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic")-> fit_beta3_pelagic

post_m2$sims.list$beta0_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic")-> cens_beta0_pelagic
post_m2$sims.list$beta1_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic")-> cens_beta1_pelagic
post_m2$sims.list$beta2_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic")-> cens_beta2_pelagic
post_m2$sims.list$beta3_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic")-> cens_beta3_pelagic

post_m3$sims.list$beta0_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic")-> hyb_beta0_pelagic
post_m3$sims.list$beta1_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic")-> hyb_beta1_pelagic
post_m3$sims.list$beta2_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic")-> hyb_beta2_pelagic
post_m3$sims.list$beta3_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic")-> hyb_beta3_pelagic

rbind(fit_beta0_pelagic %>% mutate(model = "fit"),
      cens_beta0_pelagic %>% mutate(model = "cens"),
      hyb_beta0_pelagic %>% mutate(model = "hyb")) -> pelagic_b0_mods

rbind(fit_beta1_pelagic %>% mutate(model = "fit"),
      cens_beta1_pelagic %>% mutate(model = "cens"),
      hyb_beta1_pelagic %>% mutate(model = "hyb")) -> pelagic_b1_mods

rbind(fit_beta2_pelagic %>% mutate(model = "fit"),
      cens_beta2_pelagic %>% mutate(model = "cens"),
      hyb_beta2_pelagic %>% mutate(model = "hyb")) -> pelagic_b2_mods

rbind(fit_beta3_pelagic %>% mutate(model = "fit"),
      cens_beta3_pelagic %>% mutate(model = "cens"),
      hyb_beta3_pelagic %>% mutate(model = "hyb")) -> pelagic_b3_mods

n <- 1000
Y <- 47

{
  tau_mu0 <- 0.1
  mu_beta0 <- rnorm(n,0,1/sqrt(tau_mu0)) #rnorm(n,-0.25,1)
  tau_beta0 <- rgamma(n,0.001,0.001)
  
  beta0 <- rnorm(n*3,mu_beta0, tau_beta0) #INTERCEPT
  beta0 <- beta0[beta0 > -2.75]; beta0 <- sample(beta0,n,replace = F)
  
  tau1 <- 0.25
  beta1 <- rnorm(n,0, 1/sqrt(tau1))
  # beta1 <- beta1[beta1 > 0]; beta1 <- sample(beta1,n,replace = F) # SCALING FACTOR (amplitude) #rep(2,n)
  
  tau2 <- 1
  beta2 <- rnorm(n,0,  1/sqrt(tau2)) #0.1
  #beta2 <- beta2[beta2 > 0]; beta2 <- sample(beta2,n,replace = F) #T(0,)          # SLOPE
  
  tau3 <- 0.015#sd = 10
  beta3 <- rnorm(n*3,30,  1/sqrt(tau3)); beta3 <- beta3[beta3 > 0 & beta3 < Y-3]; beta3 <- sample(beta3,n,replace = F) #T(0,Y-3)    # INFLECTION POINT 
  
  cbind(beta0,beta1,beta2,beta3) %>% data.frame -> pr_sim
  props <- data.frame(matrix(nrow = n, ncol=Y)) 
  colnames(props) <- seq(1,Y,1)
  
  for (i in 1:n) { #i <- 1
    for (y in 1:Y){
      props[i,y] <- logit_to_prob(pr_sim$beta0[i] +
                                    pr_sim$beta1[i] / (1 + exp(-pr_sim$beta2[i] * (y - pr_sim$beta3[i]))))
    }
  }
  
  props %>% mutate(iter = row_number()) %>%
    pivot_longer(cols = seq(1,Y,1),
                 names_to = "year",
                 values_to = "prop") %>%
    mutate(year = as.numeric(year)) %>%
    data.frame() -> props
  str(props)
  
  props %>% group_by(year) %>%
    dplyr::summarise(mean_prop = mean(prop),
                     lo50 = as.numeric(quantile(prop,c(0.25))),
                     hi50 = as.numeric(quantile(prop,c(0.75))),
                     lo95 = as.numeric(quantile(prop,c(0.025))),
                     hi95 = as.numeric(quantile(prop,c(0.975))))  -> mean
  
  ggplot(props,aes(x = year, y = prop)) +
    geom_ribbon(data = mean, 
                aes(y = mean_prop, ymin = lo95,ymax = hi95),color = NA, alpha = 0.2) +
    geom_ribbon(data = mean, aes(y = mean_prop, ymin = lo50,ymax = hi50),color = NA, alpha = 0.2) +
    geom_line(aes(group = iter),alpha = 0.05, color = "blue") +  # Adjust alpha for transparency
    geom_line(data = mean, aes(x = year, y = mean_prop)) + #,aes(x = year, y = prop)) +
    labs(
      title = "Lines for Each Row in props",
      x = "y",
      y = "Probability"
    ) + ylim(0,1) +
    theme_minimal()
}

pr_sim %>% filter(beta0 < -10)

ggplot(pr_sim %>% filter(beta0 > -20 & beta0 < 15)) +
  geom_histogram(aes(beta0, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = pelagic_b0_mods, 
                 aes(pelagic, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta1, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = pelagic_b1_mods, 
                 aes(pelagic, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta2, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = pelagic_b2_mods, 
                 aes(pelagic, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta3, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = pelagic_b3_mods, 
                 aes(pelagic, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

#-------------------------------------------------------------------------------
#black logistic priors:
post_m1$sims.list$beta0_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black")-> fit_beta0_black 
post_m1$sims.list$beta1_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black")-> fit_beta1_black
post_m1$sims.list$beta2_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black")-> fit_beta2_black
post_m1$sims.list$beta3_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black")-> fit_beta3_black

post_m2$sims.list$beta0_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black")-> cens_beta0_black
post_m2$sims.list$beta1_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black")-> cens_beta1_black
post_m2$sims.list$beta2_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black")-> cens_beta2_black
post_m2$sims.list$beta3_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black")-> cens_beta3_black

post_m3$sims.list$beta0_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black")-> hyb_beta0_black
post_m3$sims.list$beta1_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black")-> hyb_beta1_black
post_m3$sims.list$beta2_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black")-> hyb_beta2_black
post_m3$sims.list$beta3_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black")-> hyb_beta3_black

rbind(fit_beta0_black %>% mutate(model = "fit"),
      cens_beta0_black %>% mutate(model = "cens"),
      hyb_beta0_black %>% mutate(model = "hyb")) -> black_b0_mods

rbind(fit_beta1_black %>% mutate(model = "fit"),
      cens_beta1_black %>% mutate(model = "cens"),
      hyb_beta1_black %>% mutate(model = "hyb")) -> black_b1_mods

rbind(fit_beta2_black %>% mutate(model = "fit"),
      cens_beta2_black %>% mutate(model = "cens"),
      hyb_beta2_black %>% mutate(model = "hyb")) -> black_b2_mods

rbind(fit_beta3_black %>% mutate(model = "fit"),
      cens_beta3_black %>% mutate(model = "cens"),
      hyb_beta3_black %>% mutate(model = "hyb")) -> black_b3_mods

n <- 1000
Y <- 47

{
  tau_mu0 <- 0.1
  mu_beta0 <- rnorm(n,0,1/sqrt(tau_mu0)) #rnorm(n,-0.25,1)
  mu_beta0_lo <- rnorm(n,-0.5,1/sqrt(tau_mu0))
  mu_beta0_hi <- rnorm(n,0.5,1/sqrt(tau_mu0))
  tau_beta0 <- rgamma(n,0.001,0.001)
  
  beta0 <- rnorm(n*3,mu_beta0, tau_beta0) #INTERCEPT
  beta0 <- beta0[beta0 > -2.75]; beta0 <- sample(beta0,n,replace = F)
  
  beta0_lo <- rnorm(n*3,mu_beta0_lo, tau_beta0) #INTERCEPT
  beta0_lo <- beta0_lo[beta0_lo > -2.75]; beta0_lo <- sample(beta0_lo,n,replace = F)
  
  beta0_hi <- rnorm(n*3,mu_beta0_hi, tau_beta0) #INTERCEPT
  beta0_hi <- beta0_hi[beta0_hi > -2.75]; beta0_hi <- sample(beta0_hi,n,replace = F)
  
  tau1 <- 0.25
  beta1 <- rnorm(n,0, 1/sqrt(tau1))
  # beta1 <- beta1[beta1 > 0]; beta1 <- sample(beta1,n,replace = F) # SCALING FACTOR (amplitude) #rep(2,n)
  
  tau2 <- 1
  beta2 <- rnorm(n,0,  1/sqrt(tau2)) #0.1
  #beta2 <- beta2[beta2 > 0]; beta2 <- sample(beta2,n,replace = F) #T(0,)          # SLOPE
  
  tau3 <- 0.005#sd = 10
  beta3 <- rnorm(n*3,30,  1/sqrt(tau3)); beta3 <- beta3[beta3 > 0 & beta3 < Y-3]; beta3 <- sample(beta3,n,replace = F) #T(0,Y-3)    # INFLECTION POINT 
  
  cbind(beta0,beta1,beta2,beta3,
        beta0_lo,beta0_hi) %>% data.frame -> pr_sim
  props <- data.frame(matrix(nrow = n, ncol=Y)) 
  colnames(props) <- seq(1,Y,1)
  
  for (i in 1:n) { #i <- 1
    for (y in 1:Y){
      props[i,y] <- logit_to_prob(pr_sim$beta0[i] +
                                    pr_sim$beta1[i] / (1 + exp(-pr_sim$beta2[i] * (y - pr_sim$beta3[i]))))
    }
  }
  
  props %>% mutate(iter = row_number()) %>%
    pivot_longer(cols = seq(1,Y,1),
                 names_to = "year",
                 values_to = "prop") %>%
    mutate(year = as.numeric(year)) %>%
    data.frame() -> props
  str(props)
  
  props %>% group_by(year) %>%
    dplyr::summarise(mean_prop = mean(prop),
                     lo50 = as.numeric(quantile(prop,c(0.25))),
                     hi50 = as.numeric(quantile(prop,c(0.75))),
                     lo95 = as.numeric(quantile(prop,c(0.025))),
                     hi95 = as.numeric(quantile(prop,c(0.975))))  -> mean
  
  ggplot(props,aes(x = year, y = prop)) +
    geom_ribbon(data = mean, 
                aes(y = mean_prop, ymin = lo95,ymax = hi95),color = NA, alpha = 0.2) +
    geom_ribbon(data = mean, aes(y = mean_prop, ymin = lo50,ymax = hi50),color = NA, alpha = 0.2) +
    geom_line(aes(group = iter),alpha = 0.05, color = "blue") +  # Adjust alpha for transparency
    geom_line(data = mean, aes(x = year, y = mean_prop)) + #,aes(x = year, y = prop)) +
    labs(
      title = "Lines for Each Row in props",
      x = "y",
      y = "Probability"
    ) + ylim(0,1) +
    theme_minimal()
}

pr_sim %>% filter(beta0 < -10)

ggplot(pr_sim %>% filter(beta0 > -20 & beta0 < 15 &
                           beta0_lo > -20 & beta0_lo < 15 &
                           beta0_hi > -20 & beta0_hi < 15)) +
  geom_histogram(aes(beta0, y = ..density..), 
                 fill = "blue", color = "blue", alpha = 0.25, bins = 100, position="identity") +
  geom_histogram(aes(beta0_lo, y = ..density..), 
                 fill = "cyan", color = "cyan", alpha = 0.25, bins = 100, position="identity") +
  geom_histogram(aes(beta0_hi, y = ..density..), 
                 fill = "darkblue", color = "darkblue", alpha = 0.25, bins = 100, position="identity") #+

ggplot(pr_sim %>% filter(beta0 > -20 & beta0 < 15)) +
         geom_histogram(aes(beta0, y = ..density..), 
                        fill = "blue", color = "blue", alpha = 0.25, bins = 100, position="identity") +
  geom_histogram(data = black_b0_mods, 
                 aes(black, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta1, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = black_b1_mods, 
                 aes(black, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta2, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = black_b2_mods, 
                 aes(black, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)

ggplot(pr_sim) +
  geom_histogram(aes(beta3, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = black_b3_mods, 
                 aes(black, y = ..density.. ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model)





