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
# p_X tau priors
opt1 <- rgamma(10000,0.001,0.001)
opt2 <- rgamma(10000,0.01,0.001)
opt3 <- rgamma(10000,0.1,0.1)
opt4 <- rgamma(10000,1,0.1)
opt5 <- rgamma(10000,0.1,0.5)

taus <- cbind(opt1 = opt1, opt2 = opt2, opt3 = opt3, opt4 = opt4, opt5 = opt5)

ggplot(taus) +
  #geom_histogram(aes(opt1), color = "red", fill = "red", alpha = 0.25, bins = 100) +
  #geom_histogram(aes(opt2), color = "red", fill = "orange", alpha = 0.25, bins = 100) +
  geom_histogram(aes(opt3), color = "goldenrod", fill = "goldenrod", alpha = 0.25, bins = 100) +
  geom_histogram(aes(opt4), color = "violet", fill = "violet", alpha = 0.25, bins = 100) +
  geom_histogram(aes(opt5), color = "forestgreen", fill = "forestgreen", alpha = 0.25, bins = 100)
  
range(opt1); range(opt2); range(opt3); range(opt4); range(opt5)

#-------------------------------------------------------------------------------
opt1 <- rnorm(10000,-3,0.1)
opt2 <- rnorm(10000,-3,0.5)

mus <- cbind(opt1 = opt1, opt2 = opt2)

ggplot(mus) +
  geom_histogram(aes(opt1), color = "red", fill = "violet", alpha = 0.25, bins = 100) +
  geom_histogram(aes(opt2), color = "forestgreen", fill = "forestgreen", alpha = 0.25, bins = 100)

#-------------------------------------------------------------------------------
# Beta prior for pH
library(ggplot2)

alpha <- 3 #0.00001
beta <- 1.5

# Plot the distribution
x <- seq(0, 1, length.out = 100)
y <- dbeta(x, alpha, beta)

qplot(x, y, geom = "line") +
  labs(title = "Beta Distribution", x = "x", y = "Density") +
  theme_minimal()

# Check mean and variance
mean <- alpha / (alpha + beta)
variance <- alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1))
mean
variance

#-------------------------------------------------------------------------------
# General p_ curves and starting values:
{
  Y <- 47
  B0 <- 0 #mu_beta0_mu
  B1 <- 5 #beta1_mu
  B2 <- -1 #beta2_mu
  B3 <- 30 #beta3_mu
  
  sv_line <- data.frame()
  for (y in 1:Y){
    sv_line[y,"prop"] <- logit_to_prob(B0 + B1 / (1 + exp(-B2 * (y - B3))))
  }
  sv_line %>% mutate(y = seq(1,Y,1)) -> sv_line
  plot(sv_line$prop ~ sv_line$y)
}

p_sv_fun <- function(B0,B1,B2,B3){
  sv_line <- data.frame()
  for (y in 1:Y){
    sv_line[y,"prop"] <- logit_to_prob(B0 + B1 / (1 + exp(-B2 * (y - B3))))
  }
  sv_line %>% mutate(y = seq(1,Y,1)) -> sv_line
  plot(sv_line$prop ~ sv_line$y, ylim = c(0,1))
}

#pH all
p_sv_fun(B0 = 1.2, B1 = 1.5, B2 = 0.5, B3 = 30) #central
p_sv_fun(B0 = 0, B1 = 1, B2 = 0.5, B3 = 30) #Kodiak
p_sv_fun(B0 = 0.5, B1 = 1.75, B2 = 0.5, B3 = 30) #southeast

#pH pel
p_sv_fun(B0 = 1, B1 = 1.5, B2 = 0.5, B3 = 30) #central
p_sv_fun(B0 = 0, B1 = 2.5, B2 = 0.20, B3 = 30) #Kodiak
p_sv_fun(B0 = -0.25, B1 = 3, B2 = 0.5, B3 = 30) #southeast

#pH ye
p_sv_fun(B0 = 2, B1 = 2, B2 = -1, B3 = 45) #central
p_sv_fun(B0 = 3, B1 = 0, B2 = 0.20, B3 = 30) #Kodiak
p_sv_fun(B0 = -3, B1 = 6, B2 = -3, B3 = 44) #southeast

#pH nonpel - nonye
p_sv_fun(B0 = 1, B1 = 2.5, B2 = 0.25, B3 = 37) #central
p_sv_fun(B0 = 0, B1 = 2.5, B2 = 0.25, B3 = 37) #Kodiak
p_sv_fun(B0 = -2, B1 = 3, B2 = -0.5, B3 = 44) #southeast

#pH DSR
p_sv_fun(B0 = -2, B1 = 5, B2 = -0.5, B3 = 40) #southeast

#pH slope
p_sv_fun(B0 = -2, B1 = 5, B2 = -0.5, B3 = 40) #southeast

#-------------------------------------------------------------------------------
# pH Logistic curves
# best model runs: 
m1 <- "HR_fitLBR_2bias_thru2023_1400000_7kn_2024-12-13"
m2 <- "HR_censLBR_thru2023_1400000_7kn_2024-12-13"
m3 <- "HR_hybLBR_2bias_thru2023_1400000_7kn_2024-12-13"

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
ind <- seq(1,Y,1)
ys <- seq(1977,2023,1)
cbind(ind,ys)


post_m3$sims.list$beta0_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast"))) -> hyb_beta0_yellow
post_m3$sims.list$beta1_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta1_yellow
post_m3$sims.list$beta2_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta2_yellow
post_m3$sims.list$beta3_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta3_yellow
post_m1$sims.list$beta0_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta0_yellow 
post_m1$sims.list$beta1_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta1_yellow
post_m1$sims.list$beta2_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta2_yellow
post_m1$sims.list$beta3_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta3_yellow

post_m2$sims.list$beta0_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta0_yellow
post_m2$sims.list$beta1_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta1_yellow
post_m2$sims.list$beta2_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta2_yellow
post_m2$sims.list$beta3_yellow %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta3_yellow

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

mods <- unique(yellow_b0_mods$model)

#get modes from these model runs
postmodes_B0 <- postmodes_B1 <- postmodes_B2 <- postmodes_B3 <- data.frame()
it<-1
for (i in 1:3){  #i<-1
  for (a in unique(H_ayg$area)){ #a<-unique(H_ayg$area)[1] 
    postmodes_B0[it,"model"] <- mods[i]
    postmodes_B0[it,"param"] <- "ye_B0"
    postmodes_B0[it,"area"] <- a
    postmodes_B0[it,"mode1"] <- find_modes(yellow_b0_mods$yellow[yellow_b0_mods$area == a &
                                                                yellow_b0_mods$model == mods[i]])[1,1]
    postmodes_B0[it,"mode2"] <- find_modes(yellow_b0_mods$yellow[yellow_b0_mods$area == a &
                                                                yellow_b0_mods$model == mods[i]])[2,1]
    postmodes_B0[it,"mode3"] <- find_modes(yellow_b0_mods$yellow[yellow_b0_mods$area == a &
                                                                yellow_b0_mods$model == mods[i]])[3,1]
    postmodes_B0[it,"mode_ht1"] <- find_modes(yellow_b0_mods$yellow[yellow_b0_mods$area == a &
                                                                   yellow_b0_mods$model == mods[i]])[1,2]
    postmodes_B0[it,"mode_ht2"] <- find_modes(yellow_b0_mods$yellow[yellow_b0_mods$area == a &
                                                                   yellow_b0_mods$model == mods[i]])[2,2]
    postmodes_B0[it,"mode_ht3"] <- find_modes(yellow_b0_mods$yellow[yellow_b0_mods$area == a &
                                                                   yellow_b0_mods$model == mods[i]])[3,2]
    
    postmodes_B1[it,"model"] <- mods[i]
    postmodes_B1[it,"param"] <- "ye_B1"
    postmodes_B1[it,"area"] <- a
    postmodes_B1[it,"mode1"] <- find_modes(yellow_b1_mods$yellow[yellow_b1_mods$area == a &
                                                                   yellow_b1_mods$model == mods[i]])[1,1]
    postmodes_B1[it,"mode2"] <- find_modes(yellow_b1_mods$yellow[yellow_b1_mods$area == a &
                                                                   yellow_b1_mods$model == mods[i]])[2,1]
    postmodes_B1[it,"mode3"] <- find_modes(yellow_b1_mods$yellow[yellow_b1_mods$area == a &
                                                                   yellow_b1_mods$model == mods[i]])[3,1]
    postmodes_B1[it,"mode_ht1"] <- find_modes(yellow_b1_mods$yellow[yellow_b1_mods$area == a &
                                                                      yellow_b1_mods$model == mods[i]])[1,2]
    postmodes_B1[it,"mode_ht2"] <- find_modes(yellow_b1_mods$yellow[yellow_b1_mods$area == a &
                                                                      yellow_b1_mods$model == mods[i]])[2,2]
    postmodes_B1[it,"mode_ht3"] <- find_modes(yellow_b1_mods$yellow[yellow_b1_mods$area == a &
                                                                      yellow_b1_mods$model == mods[i]])[3,2]
    
    postmodes_B2[it,"model"] <- mods[i]
    postmodes_B2[it,"param"] <- "ye_B2"
    postmodes_B2[it,"area"] <- a
    postmodes_B2[it,"mode1"] <- find_modes(yellow_b2_mods$yellow[yellow_b2_mods$area == a &
                                                                   yellow_b2_mods$model == mods[i]])[1,1]
    postmodes_B2[it,"mode2"] <- find_modes(yellow_b2_mods$yellow[yellow_b2_mods$area == a &
                                                                   yellow_b2_mods$model == mods[i]])[2,1]
    postmodes_B2[it,"mode3"] <- find_modes(yellow_b2_mods$yellow[yellow_b2_mods$area == a &
                                                                   yellow_b2_mods$model == mods[i]])[3,1]
    postmodes_B2[it,"mode_ht1"] <- find_modes(yellow_b2_mods$yellow[yellow_b2_mods$area == a &
                                                                      yellow_b2_mods$model == mods[i]])[1,2]
    postmodes_B2[it,"mode_ht2"] <- find_modes(yellow_b2_mods$yellow[yellow_b2_mods$area == a &
                                                                      yellow_b2_mods$model == mods[i]])[2,2]
    postmodes_B2[it,"mode_ht3"] <- find_modes(yellow_b2_mods$yellow[yellow_b2_mods$area == a &
                                                                      yellow_b2_mods$model == mods[i]])[3,2]
    
    postmodes_B3[it,"model"] <- mods[i]
    postmodes_B3[it,"param"] <- "ye_B3"
    postmodes_B3[it,"area"] <- a
    postmodes_B3[it,"mode1"] <- find_modes(yellow_b3_mods$yellow[yellow_b3_mods$area == a &
                                                                   yellow_b3_mods$model == mods[i]])[1,1]
    postmodes_B3[it,"mode2"] <- find_modes(yellow_b3_mods$yellow[yellow_b3_mods$area == a &
                                                                   yellow_b3_mods$model == mods[i]])[2,1]
    postmodes_B3[it,"mode3"] <- find_modes(yellow_b3_mods$yellow[yellow_b3_mods$area == a &
                                                                   yellow_b3_mods$model == mods[i]])[3,1]
    postmodes_B3[it,"mode_ht1"] <- find_modes(yellow_b3_mods$yellow[yellow_b3_mods$area == a &
                                                                      yellow_b3_mods$model == mods[i]])[1,2]
    postmodes_B3[it,"mode_ht2"] <- find_modes(yellow_b3_mods$yellow[yellow_b3_mods$area == a &
                                                                      yellow_b3_mods$model == mods[i]])[2,2]
    postmodes_B3[it,"mode_ht3"] <- find_modes(yellow_b3_mods$yellow[yellow_b3_mods$area == a &
                                                                      yellow_b3_mods$model == mods[i]])[3,2]
    it <- it+1
  }
}

postmodes_B0_ye <- postmodes_B0 %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))
postmodes_B1_ye <- postmodes_B1 %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))
postmodes_B2_ye <- postmodes_B2 %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))
postmodes_B3_ye <- postmodes_B3 %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))

n <- 1000
Y <- 47

#priors
{
tau_mu0 <- 0.5
mu_beta0_mu <- 0
tau1 <- 0.1
beta1_mu <- 0
tau2 <- 0.1
beta2_mu <- -0.5
tau3 <- 0.1 
beta3_mu <- 30
}

{
  mu_beta0 <- rnorm(n,mu_beta0_mu,1/sqrt(tau_mu0)) #rnorm(n,-0.25,1)
  tau_beta0 <- rgamma(n,0.001,0.001)
  
  beta0 <- rnorm(n*3,mu_beta0, tau_beta0) #INTERCEPT
  beta0 <- beta0[beta0 > -5]; beta0 <- sample(beta0,n,replace = F)
  
  beta1 <- rnorm(n*3, beta1_mu, 1/sqrt(tau1))
  beta1 <- beta1[beta1 >= 0]; beta1 <- sample(beta1,n,replace = F) # SCALING FACTOR (amplitude) #rep(2,n)
  
  beta2 <- rnorm(n,beta2_mu,  1/sqrt(tau2)) #0.1
  
  beta3 <- rnorm(n*3,beta3_mu,  1/sqrt(tau3)); beta3 <- beta3[beta3 > 0 & beta3 < Y-3]; beta3 <- sample(beta3,n,replace = F) #T(0,Y-3)    # INFLECTION POINT 
  
  cbind(beta0,beta1,beta2,beta3) %>% data.frame -> pr_sim
  props <- data.frame(matrix(nrow = n, ncol=Y)) 
  modprops <- data.frame() 
  colnames(props) <- seq(1,Y,1)
  
  for (i in 1:n) { #i <- 1
    for (y in 1:Y){
      props[i,y] <- logit_to_prob(pr_sim$beta0[i] +
                                    pr_sim$beta1[i] / (1 + exp(-pr_sim$beta2[i] * (y - pr_sim$beta3[i]))))
    }
  }
  
  prior_line <- data.frame()
  for (y in 1:Y){
    prior_line[y,"prop"] <- logit_to_prob(mu_beta0_mu +
                                      beta1_mu / (1 + exp(-beta2_mu * (y - beta3_mu))))
  }
  
  for (a in unique(H_ayg$area)){ #a<-unique(H_ayg$area)[1]; m<- mods[1]; y <- 1
    for (m in mods){
      for (y in 1:Y){
        modprops[y,a] <- logit_to_prob(postmodes_B0$mode1[postmodes_B0$area == a & postmodes_B0$model == m] +
                                          postmodes_B1$mode1[postmodes_B1$area == a & postmodes_B1$model == m] / 
                                          (1 + exp(-postmodes_B2$mode1[postmodes_B2$area == a & postmodes_B2$model == m] * 
                                                     (y - postmodes_B3$mode1[postmodes_B3$area == a & postmodes_B3$model == m]))))
      }
    }
  }
  
  modprops <- modprops %>% 
    mutate(y = seq(1,Y,1)) %>% 
    pivot_longer(cols = unique(H_ayg$area), names_to = "area",values_to = "prop")
  
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
    theme_minimal() +
    geom_line(data = modprops,
              aes(x = y, y = prop, color = area), linewidth = 1.5) +
    geom_line(data = prior_line %>% mutate(y = seq(1,Y,1)),
              aes(x = y, y = prop), color = "black", linetype = 2, linewidth = 1.35)
}

ggplot(pr_sim %>% filter(beta0 > -20 & beta0 < 15)) +
  geom_histogram(aes(beta0, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_b0_mods, 
                 aes(yellow, y = after_stat(density) ,fill = area), 
                 alpha = 0.25, bins = 100, position="identity") +
  geom_vline(data = postmodes_B0_ye, aes(xintercept = mode1, color = area)) +
  facet_wrap(~model + region, scale = "free") 

ggplot(pr_sim) +
  geom_histogram(aes(beta1, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_b1_mods, 
                 aes(yellow, y = after_stat(density) ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  geom_vline(data = postmodes_B1_ye, aes(xintercept = mode1, color = area)) +
  facet_wrap(~model+ region, scale = "free")

ggplot(pr_sim) +
  geom_histogram(aes(beta2, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_b2_mods, 
                 aes(yellow, y = after_stat(density) ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  geom_vline(data = postmodes_B2_ye, aes(xintercept = mode1, color = area)) +
  facet_wrap(~model+ region, scale = "free")

ggplot(pr_sim) +
  geom_histogram(aes(beta3, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_b3_mods, 
                 aes(yellow, y = after_stat(density) ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  geom_vline(data = postmodes_B3_ye, aes(xintercept = mode1, color = area)) +
  facet_wrap(~model+ region, scale = "free")

# Figure out starting values and better shape to priors: 
{
B0 <- -3 #mu_beta0_mu
B1 <- 2 #beta1_mu
B2 <- -0.7 #beta2_mu
B3 <- 41 #beta3_mu

sv_line <- data.frame()
for (y in 1:Y){
  sv_line[y,"prop"] <- logit_to_prob(B0 + B1 / (1 + exp(-B2 * (y - B3))))
}
sv_line %>% mutate(y = seq(1,Y,1)) -> sv_line
plot(sv_line$prop ~ sv_line$y)
}

postmodes_B0_ye;ggplot(postmodes_B0_ye) +
  geom_histogram(aes(mode1), fill = "blue", alpha = 0.3, bins = 50) +
  geom_histogram(data = postmodes_B0_ye %>% filter(mode_ht2 > 0.3),
                 aes(mode2), fill = "red", alpha = 0.3, bins = 50) +
  facet_wrap(~region)

plot(postmodes_B0_ye$mode1[postmodes_B0_ye$model == "fit"]~
       postmodes_B0_ye$mode1[postmodes_B0_ye$model == "cens"])

B0_ye_inits <- c(as.vector(postmodes_B0_ye$mode1[postmodes_B0_ye$model == "fit"][1:10]),
                 0,0,0,0,0,0)

postmodes_B1_ye; ggplot(postmodes_B1_ye) +
  geom_histogram(aes(mode1), fill = "blue", alpha = 0.3, bins = 50) +
  geom_histogram(data = postmodes_B1_ye %>% filter(mode_ht2 > 0.3),
                 aes(mode2), fill = "red", alpha = 0.3, bins = 50) +
  facet_wrap(~region)

plot(postmodes_B1_ye$mode1[postmodes_B1_ye$model == "fit"]~
       postmodes_B1_ye$mode1[postmodes_B1_ye$model == "cens"])

postmodes_B1_ye %>% filter(mode_ht2 > 0.33)

B1_ye_inits <- c(as.vector(abs(postmodes_B1_ye$mode1[postmodes_B1_ye$model == "fit"][1:10])),
                 0.5,0.5,0.5,0.5,0.5,0.5)

postmodes_B2_ye; ggplot(postmodes_B2_ye) +
  geom_histogram(aes(mode1), fill = "blue", alpha = 0.3, bins = 50) +
  geom_histogram(data = postmodes_B2_ye %>% filter(mode_ht2 > 0.3),
                 aes(mode2), fill = "red", alpha = 0.3, bins = 50) +
  facet_wrap(~region)

plot(postmodes_B2_ye$mode1[postmodes_B2_ye$model == "fit"]~
       postmodes_B2_ye$mode1[postmodes_B2_ye$model == "cens"])

B2_ye_inits <- rnorm(16,-0.25,0.25)

postmodes_B3_ye; ggplot(postmodes_B3_ye) +
  geom_histogram(aes(mode1), fill = "blue", alpha = 0.3, bins = 50) +
  geom_histogram(data = postmodes_B3_ye %>% filter(mode_ht2 > 0.3),
                 aes(mode2), fill = "red", alpha = 0.3, bins = 50) +
  facet_wrap(~region)

plot(postmodes_B3_ye$mode1[postmodes_B3_ye$model == "fit"]~
       postmodes_B3_ye$mode1[postmodes_B3_ye$model == "cens"])

B3_ye_inits <- c(as.vector(abs(postmodes_B3_ye$mode1[postmodes_B3_ye$model == "fit"][1:10])),
                 35,35,35,35,35,35)

inits <- rbind(B0_ye_inits = B0_ye_inits,
               B1_ye_inits = B1_ye_inits,
               B2_ye_inits = B2_ye_inits,
               B3_ye_inits = B3_ye_inits)

#-------------------------------------------------------------------------------
#yelloweye_X logistic priors:
post_m1$sims.list$beta0_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta0_yellow_x 
post_m1$sims.list$beta1_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta1_yellow_x
post_m1$sims.list$beta2_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta2_yellow_x
post_m1$sims.list$beta3_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta3_yellow_x

post_m2$sims.list$beta0_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta0_yellow_x
post_m2$sims.list$beta1_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta1_yellow_x
post_m2$sims.list$beta2_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta2_yellow_x
post_m2$sims.list$beta3_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta3_yellow_x

post_m3$sims.list$beta0_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta0_yellow_x
post_m3$sims.list$beta1_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta1_yellow_x
post_m3$sims.list$beta2_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta2_yellow_x
post_m3$sims.list$beta3_yellow_x %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "yellow_x") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta3_yellow_x

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

#get modes from these model runs
postmodes_B0 <- postmodes_B1 <- postmodes_B2 <- postmodes_B3 <- data.frame()
it<-1
for (i in 1:3){  #i<-1
  for (a in c(unique(H_ayg$area)[11:16])){ #a<-unique(H_ayg$area)[1] 
    postmodes_B0[it,"model"] <- mods[i]
    postmodes_B0[it,"param"] <- "yeX_B0"
    postmodes_B0[it,"area"] <- a
    postmodes_B0[it,"mode1"] <- find_modes(yellow_x_b0_mods$yellow_x[yellow_x_b0_mods$area == a &
                                                                   yellow_x_b0_mods$model == mods[i]])[1,1]
    postmodes_B0[it,"mode2"] <- find_modes(yellow_x_b0_mods$yellow_x[yellow_x_b0_mods$area == a &
                                                                   yellow_x_b0_mods$model == mods[i]])[2,1]
    postmodes_B0[it,"mode3"] <- find_modes(yellow_x_b0_mods$yellow_x[yellow_x_b0_mods$area == a &
                                                                   yellow_x_b0_mods$model == mods[i]])[3,1]
    postmodes_B0[it,"mode_ht1"] <- find_modes(yellow_x_b0_mods$yellow_x[yellow_x_b0_mods$area == a &
                                                                      yellow_x_b0_mods$model == mods[i]])[1,2]
    postmodes_B0[it,"mode_ht2"] <- find_modes(yellow_x_b0_mods$yellow_x[yellow_x_b0_mods$area == a &
                                                                      yellow_x_b0_mods$model == mods[i]])[2,2]
    postmodes_B0[it,"mode_ht3"] <- find_modes(yellow_x_b0_mods$yellow_x[yellow_x_b0_mods$area == a &
                                                                      yellow_x_b0_mods$model == mods[i]])[3,2]
    
    postmodes_B1[it,"model"] <- mods[i]
    postmodes_B1[it,"param"] <- "yeX_B1"
    postmodes_B1[it,"area"] <- a
    postmodes_B1[it,"mode1"] <- find_modes(yellow_x_b1_mods$yellow_x[yellow_x_b1_mods$area == a &
                                                                   yellow_x_b1_mods$model == mods[i]])[1,1]
    postmodes_B1[it,"mode2"] <- find_modes(yellow_x_b1_mods$yellow_x[yellow_x_b1_mods$area == a &
                                                                   yellow_x_b1_mods$model == mods[i]])[2,1]
    postmodes_B1[it,"mode3"] <- find_modes(yellow_x_b1_mods$yellow_x[yellow_x_b1_mods$area == a &
                                                                   yellow_x_b1_mods$model == mods[i]])[3,1]
    postmodes_B1[it,"mode_ht1"] <- find_modes(yellow_x_b1_mods$yellow_x[yellow_x_b1_mods$area == a &
                                                                      yellow_x_b1_mods$model == mods[i]])[1,2]
    postmodes_B1[it,"mode_ht2"] <- find_modes(yellow_x_b1_mods$yellow_x[yellow_x_b1_mods$area == a &
                                                                      yellow_x_b1_mods$model == mods[i]])[2,2]
    postmodes_B1[it,"mode_ht3"] <- find_modes(yellow_x_b1_mods$yellow_x[yellow_x_b1_mods$area == a &
                                                                      yellow_x_b1_mods$model == mods[i]])[3,2]
    
    postmodes_B2[it,"model"] <- mods[i]
    postmodes_B2[it,"param"] <- "yeX_B2"
    postmodes_B2[it,"area"] <- a
    postmodes_B2[it,"mode1"] <- find_modes(yellow_x_b2_mods$yellow_x[yellow_x_b2_mods$area == a &
                                                                   yellow_x_b2_mods$model == mods[i]])[1,1]
    postmodes_B2[it,"mode2"] <- find_modes(yellow_x_b2_mods$yellow_x[yellow_x_b2_mods$area == a &
                                                                   yellow_x_b2_mods$model == mods[i]])[2,1]
    postmodes_B2[it,"mode3"] <- find_modes(yellow_x_b2_mods$yellow_x[yellow_x_b2_mods$area == a &
                                                                   yellow_x_b2_mods$model == mods[i]])[3,1]
    postmodes_B2[it,"mode_ht1"] <- find_modes(yellow_x_b2_mods$yellow_x[yellow_x_b2_mods$area == a &
                                                                      yellow_x_b2_mods$model == mods[i]])[1,2]
    postmodes_B2[it,"mode_ht2"] <- find_modes(yellow_x_b2_mods$yellow_x[yellow_x_b2_mods$area == a &
                                                                      yellow_x_b2_mods$model == mods[i]])[2,2]
    postmodes_B2[it,"mode_ht3"] <- find_modes(yellow_x_b2_mods$yellow_x[yellow_x_b2_mods$area == a &
                                                                      yellow_x_b2_mods$model == mods[i]])[3,2]
    
    postmodes_B3[it,"model"] <- mods[i]
    postmodes_B3[it,"param"] <- "yeX_B3"
    postmodes_B3[it,"area"] <- a
    postmodes_B3[it,"mode1"] <- find_modes(yellow_x_b3_mods$yellow_x[yellow_x_b3_mods$area == a &
                                                                   yellow_x_b3_mods$model == mods[i]])[1,1]
    postmodes_B3[it,"mode2"] <- find_modes(yellow_x_b3_mods$yellow_x[yellow_x_b3_mods$area == a &
                                                                   yellow_x_b3_mods$model == mods[i]])[2,1]
    postmodes_B3[it,"mode3"] <- find_modes(yellow_x_b3_mods$yellow_x[yellow_x_b3_mods$area == a &
                                                                   yellow_x_b3_mods$model == mods[i]])[3,1]
    postmodes_B3[it,"mode_ht1"] <- find_modes(yellow_x_b3_mods$yellow_x[yellow_x_b3_mods$area == a &
                                                                      yellow_x_b3_mods$model == mods[i]])[1,2]
    postmodes_B3[it,"mode_ht2"] <- find_modes(yellow_x_b3_mods$yellow_x[yellow_x_b3_mods$area == a &
                                                                      yellow_x_b3_mods$model == mods[i]])[2,2]
    postmodes_B3[it,"mode_ht3"] <- find_modes(yellow_x_b3_mods$yellow_x[yellow_x_b3_mods$area == a &
                                                                      yellow_x_b3_mods$model == mods[i]])[3,2]
    it <- it+1
  }
}

postmodes_B0_yeX <- postmodes_B0 %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))
postmodes_B1_yeX <- postmodes_B1 %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))
postmodes_B2_yeX <- postmodes_B2 %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))
postmodes_B3_yeX <- postmodes_B3 %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))

n <- 1000
Y <- 47

#priors
{
  tau_mu0 <- 0.1
  mu_beta0_mu <- 0
  tau1 <- 0.25
  beta1_mu <- 0
  tau2 <- 1
  beta2_mu <- 0
  tau3 <- 0.1 
  beta3_mu <- 30
}

{
  mu_beta0 <- rnorm(n,mu_beta0_mu,1/sqrt(tau_mu0)) #rnorm(n,-0.25,1)
  tau_beta0 <- rgamma(n,0.001,0.001)
  
  beta0 <- rnorm(n*3,mu_beta0, tau_beta0) #INTERCEPT
  beta0 <- beta0[beta0 > -5]; beta0 <- sample(beta0,n,replace = F)
  
  beta1 <- rnorm(n,beta1_mu, 1/sqrt(tau1))
  #beta1 <- beta1[beta1 < 1]; beta1 <- sample(beta1,n,replace = F) # SCALING FACTOR (amplitude) #rep(2,n)
  
  beta2 <- rnorm(n,beta2_mu,  1/sqrt(tau2)) #0.1
  
  beta3 <- rnorm(n*3,beta3_mu,  1/sqrt(tau3)); beta3 <- beta3[beta3 > 0 & beta3 < Y-3]; beta3 <- sample(beta3,n,replace = F) #T(0,Y-3)    # INFLECTION POINT 
  
  cbind(beta0,beta1,beta2,beta3) %>% data.frame -> pr_sim
  props <- data.frame(matrix(nrow = n, ncol=Y)) 
  modprops <- data.frame() 
  colnames(props) <- seq(1,Y,1)
  
  for (i in 1:n) { #i <- 1
    for (y in 1:Y){
      props[i,y] <- logit_to_prob(pr_sim$beta0[i] +
                                    pr_sim$beta1[i] / (1 + exp(-pr_sim$beta2[i] * (y - pr_sim$beta3[i]))))
    }
  }
  
  prior_line <- data.frame()
  for (y in 1:Y){
    prior_line[y,"prop"] <- logit_to_prob(mu_beta0_mu +
                                            beta1_mu / (1 + exp(-beta2_mu * (y - beta3_mu))))
  }
  
  for (a in unique(H_ayg$area)){ #a<-unique(H_ayg$area)[1]; m<- mods[1]; y <- 1
    for (m in mods){
      for (y in 1:Y){
        modprops[y,a] <- logit_to_prob(postmodes_B0$mode1[postmodes_B0$area == a & postmodes_B0$model == m] +
                                         postmodes_B1$mode1[postmodes_B1$area == a & postmodes_B1$model == m] / 
                                         (1 + exp(-postmodes_B2$mode1[postmodes_B2$area == a & postmodes_B2$model == m] * 
                                                    (y - postmodes_B3$mode1[postmodes_B3$area == a & postmodes_B3$model == m]))))
      }
    }
  }
  
  modprops <- modprops %>% 
    mutate(y = seq(1,Y,1)) %>% 
    pivot_longer(cols = unique(H_ayg$area), names_to = "area",values_to = "prop")
  
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
    theme_minimal() +
    geom_line(data = modprops,
              aes(x = y, y = prop, color = area), linewidth = 1.5) +
    geom_line(data = prior_line %>% mutate(y = seq(1,Y,1)),
              aes(x = y, y = prop), color = "black", linetype = 2, linewidth = 1.35)
}
pr_sim %>% filter(beta0 < -10)

ggplot(pr_sim %>% filter(beta0 > -20 & beta0 < 15)) +
  geom_histogram(aes(beta0, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_x_b0_mods, 
                 aes(yellow_x, y = after_stat(density) ,fill = area), 
                 alpha = 0.25, bins = 100, position="identity") +
  facet_wrap(~model + region, scale = "free")

ggplot(pr_sim) +
  geom_histogram(aes(beta1, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_x_b1_mods, 
                 aes(yellow_x, y = after_stat(density) ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model+ region, scale = "free")

ggplot(pr_sim) +
  geom_histogram(aes(beta2, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_x_b2_mods, 
                 aes(yellow_x, y = after_stat(density) ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model+ region, scale = "free")

ggplot(pr_sim) +
  geom_histogram(aes(beta3, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = yellow_x_b3_mods, 
                 aes(yellow_x, y = after_stat(density) ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model+ region, scale = "free")

#-------------------------------------------------------------------------------
#pelagic logistic priors:
post_m1$sims.list$beta0_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta0_pelagic 
post_m1$sims.list$beta1_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta1_pelagic
post_m1$sims.list$beta2_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta2_pelagic
post_m1$sims.list$beta3_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta3_pelagic

post_m2$sims.list$beta0_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta0_pelagic
post_m2$sims.list$beta1_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta1_pelagic
post_m2$sims.list$beta2_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta2_pelagic
post_m2$sims.list$beta3_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta3_pelagic

post_m3$sims.list$beta0_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta0_pelagic
post_m3$sims.list$beta1_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta1_pelagic
post_m3$sims.list$beta2_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta2_pelagic
post_m3$sims.list$beta3_pelagic %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "pelagic") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta3_pelagic

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

#get modes from these model runs
postmodes_B0 <- postmodes_B1 <- postmodes_B2 <- postmodes_B3 <- data.frame()
it<-1
for (i in 1:3){  #i<-1
  for (a in unique(H_ayg$area)){ #a<-unique(H_ayg$area)[1] 
    postmodes_B0[it,"model"] <- mods[i]
    postmodes_B0[it,"param"] <- "pel_B0"
    postmodes_B0[it,"area"] <- a
    postmodes_B0[it,"mode1"] <- find_modes(pelagic_b0_mods$pelagic[pelagic_b0_mods$area == a &
                                                                       pelagic_b0_mods$model == mods[i]])[1,1]
    postmodes_B0[it,"mode2"] <- find_modes(pelagic_b0_mods$pelagic[pelagic_b0_mods$area == a &
                                                                       pelagic_b0_mods$model == mods[i]])[2,1]
    postmodes_B0[it,"mode3"] <- find_modes(pelagic_b0_mods$pelagic[pelagic_b0_mods$area == a &
                                                                       pelagic_b0_mods$model == mods[i]])[3,1]
    postmodes_B0[it,"mode_ht1"] <- find_modes(pelagic_b0_mods$pelagic[pelagic_b0_mods$area == a &
                                                                          pelagic_b0_mods$model == mods[i]])[1,2]
    postmodes_B0[it,"mode_ht2"] <- find_modes(pelagic_b0_mods$pelagic[pelagic_b0_mods$area == a &
                                                                          pelagic_b0_mods$model == mods[i]])[2,2]
    postmodes_B0[it,"mode_ht3"] <- find_modes(pelagic_b0_mods$pelagic[pelagic_b0_mods$area == a &
                                                                          pelagic_b0_mods$model == mods[i]])[3,2]
    
    postmodes_B1[it,"model"] <- mods[i]
    postmodes_B1[it,"param"] <- "pel_B1"
    postmodes_B1[it,"area"] <- a
    postmodes_B1[it,"mode1"] <- find_modes(pelagic_b1_mods$pelagic[pelagic_b1_mods$area == a &
                                                                       pelagic_b1_mods$model == mods[i]])[1,1]
    postmodes_B1[it,"mode2"] <- find_modes(pelagic_b1_mods$pelagic[pelagic_b1_mods$area == a &
                                                                       pelagic_b1_mods$model == mods[i]])[2,1]
    postmodes_B1[it,"mode3"] <- find_modes(pelagic_b1_mods$pelagic[pelagic_b1_mods$area == a &
                                                                       pelagic_b1_mods$model == mods[i]])[3,1]
    postmodes_B1[it,"mode_ht1"] <- find_modes(pelagic_b1_mods$pelagic[pelagic_b1_mods$area == a &
                                                                          pelagic_b1_mods$model == mods[i]])[1,2]
    postmodes_B1[it,"mode_ht2"] <- find_modes(pelagic_b1_mods$pelagic[pelagic_b1_mods$area == a &
                                                                          pelagic_b1_mods$model == mods[i]])[2,2]
    postmodes_B1[it,"mode_ht3"] <- find_modes(pelagic_b1_mods$pelagic[pelagic_b1_mods$area == a &
                                                                          pelagic_b1_mods$model == mods[i]])[3,2]
    
    postmodes_B2[it,"model"] <- mods[i]
    postmodes_B2[it,"param"] <- "pel_B2"
    postmodes_B2[it,"area"] <- a
    postmodes_B2[it,"mode1"] <- find_modes(pelagic_b2_mods$pelagic[pelagic_b2_mods$area == a &
                                                                       pelagic_b2_mods$model == mods[i]])[1,1]
    postmodes_B2[it,"mode2"] <- find_modes(pelagic_b2_mods$pelagic[pelagic_b2_mods$area == a &
                                                                       pelagic_b2_mods$model == mods[i]])[2,1]
    postmodes_B2[it,"mode3"] <- find_modes(pelagic_b2_mods$pelagic[pelagic_b2_mods$area == a &
                                                                       pelagic_b2_mods$model == mods[i]])[3,1]
    postmodes_B2[it,"mode_ht1"] <- find_modes(pelagic_b2_mods$pelagic[pelagic_b2_mods$area == a &
                                                                          pelagic_b2_mods$model == mods[i]])[1,2]
    postmodes_B2[it,"mode_ht2"] <- find_modes(pelagic_b2_mods$pelagic[pelagic_b2_mods$area == a &
                                                                          pelagic_b2_mods$model == mods[i]])[2,2]
    postmodes_B2[it,"mode_ht3"] <- find_modes(pelagic_b2_mods$pelagic[pelagic_b2_mods$area == a &
                                                                          pelagic_b2_mods$model == mods[i]])[3,2]
    
    postmodes_B3[it,"model"] <- mods[i]
    postmodes_B3[it,"param"] <- "pel_B3"
    postmodes_B3[it,"area"] <- a
    postmodes_B3[it,"mode1"] <- find_modes(pelagic_b3_mods$pelagic[pelagic_b3_mods$area == a &
                                                                       pelagic_b3_mods$model == mods[i]])[1,1]
    postmodes_B3[it,"mode2"] <- find_modes(pelagic_b3_mods$pelagic[pelagic_b3_mods$area == a &
                                                                       pelagic_b3_mods$model == mods[i]])[2,1]
    postmodes_B3[it,"mode3"] <- find_modes(pelagic_b3_mods$pelagic[pelagic_b3_mods$area == a &
                                                                       pelagic_b3_mods$model == mods[i]])[3,1]
    postmodes_B3[it,"mode_ht1"] <- find_modes(pelagic_b3_mods$pelagic[pelagic_b3_mods$area == a &
                                                                          pelagic_b3_mods$model == mods[i]])[1,2]
    postmodes_B3[it,"mode_ht2"] <- find_modes(pelagic_b3_mods$pelagic[pelagic_b3_mods$area == a &
                                                                          pelagic_b3_mods$model == mods[i]])[2,2]
    postmodes_B3[it,"mode_ht3"] <- find_modes(pelagic_b3_mods$pelagic[pelagic_b3_mods$area == a &
                                                                          pelagic_b3_mods$model == mods[i]])[3,2]
    it <- it+1
  }
}

postmodes_B0_pelagic <- postmodes_B0 %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))
postmodes_B1_pelagic <- postmodes_B1 %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))
postmodes_B2_pelagic <- postmodes_B2 %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))
postmodes_B3_pelagic <- postmodes_B3 %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))

n <- 1000
Y <- 47

#priors
{
  tau_mu0 <- 0.1
  mu_beta0_mu <- 0
  tau1 <- 0.1
  beta1_mu <- 0
  tau2 <- 0.1
  beta2_mu <- 0
  tau3 <- 0.05
  beta3_mu <- 30
}

{
  mu_beta0 <- rnorm(n,mu_beta0_mu,1/sqrt(tau_mu0)) #rnorm(n,-0.25,1)
  tau_beta0 <- rgamma(n,0.001,0.001)
  
  beta0 <- rnorm(n*4,mu_beta0, tau_beta0) #INTERCEPT
  beta0 <- beta0[beta0 > -5]; beta0 <- sample(beta0,n,replace = F)
  
  beta1 <- rnorm(n*4 ,beta1_mu, 1/sqrt(tau1))
  beta1 <- beta1[beta1 >= 0]; beta1 <- sample(beta1,n,replace = F) # SCALING FACTOR (amplitude) #rep(2,n)
  
  beta2 <- rnorm(n,beta2_mu,  1/sqrt(tau2)) #0.1
  
  beta3 <- rnorm(n*3,beta3_mu,  1/sqrt(tau3)); beta3 <- beta3[beta3 > 0 & beta3 < Y-3]; beta3 <- sample(beta3,n,replace = F) #T(0,Y-3)    # INFLECTION POINT 
  
  cbind(beta0,beta1,beta2,beta3) %>% data.frame -> pr_sim
  props <- data.frame(matrix(nrow = n, ncol=Y)) 
  modprops <- data.frame() 
  colnames(props) <- seq(1,Y,1)
  
  for (i in 1:n) { #i <- 1
    for (y in 1:Y){
      props[i,y] <- logit_to_prob(pr_sim$beta0[i] +
                                    pr_sim$beta1[i] / (1 + exp(-pr_sim$beta2[i] * (y - pr_sim$beta3[i]))))
    }
  }
  
  prior_line <- data.frame()
  for (y in 1:Y){
    prior_line[y,"prop"] <- logit_to_prob(mu_beta0_mu +
                                            beta1_mu / (1 + exp(-beta2_mu * (y - beta3_mu))))
  }
  
  for (a in unique(H_ayg$area)){ #a<-unique(H_ayg$area)[1]; m<- mods[1]; y <- 1
    for (m in mods){
      for (y in 1:Y){
        modprops[y,a] <- logit_to_prob(postmodes_B0$mode1[postmodes_B0$area == a & postmodes_B0$model == m] +
                                         postmodes_B1$mode1[postmodes_B1$area == a & postmodes_B1$model == m] / 
                                         (1 + exp(-postmodes_B2$mode1[postmodes_B2$area == a & postmodes_B2$model == m] * 
                                                    (y - postmodes_B3$mode1[postmodes_B3$area == a & postmodes_B3$model == m]))))
      }
    }
  }
  
  modprops <- modprops %>% 
    mutate(y = seq(1,Y,1)) %>% 
    pivot_longer(cols = unique(H_ayg$area), names_to = "area",values_to = "prop")
  
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
    theme_minimal() +
    geom_line(data = modprops,
              aes(x = y, y = prop, color = area), linewidth = 1.5) +
    geom_line(data = prior_line %>% mutate(y = seq(1,Y,1)),
              aes(x = y, y = prop), color = "black", linetype = 2, linewidth = 1.35)
}
pr_sim %>% filter(beta0 < -10)

ggplot(pr_sim %>% filter(beta0 > -20 & beta0 < 15)) +
  geom_histogram(aes(beta0, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = pelagic_b0_mods, 
                 aes(pelagic, y = after_stat(density) ,fill = area), 
                 alpha = 0.25, bins = 100, position="identity") +
  facet_wrap(~model + region, scale = "free")

ggplot(pr_sim) +
  geom_histogram(aes(beta1, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = pelagic_b1_mods, 
                 aes(pelagic, y = after_stat(density) ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model+ region, scale = "free")

ggplot(pr_sim) +
  geom_histogram(aes(beta2, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = pelagic_b2_mods, 
                 aes(pelagic, y = after_stat(density) ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model+ region, scale = "free")

ggplot(pr_sim) +
  geom_histogram(aes(beta3, y = ..density..), fill = "blue", alpha = 1, bins = 100) +
  geom_histogram(data = pelagic_b3_mods, 
                 aes(pelagic, y = after_stat(density) ,fill = area), 
                 alpha = 0.2, bins = 100, position="identity") +
  facet_wrap(~model+ region, scale = "free")

{
  B0 <- 0 #mu_beta0_mu
  B1 <- 0.25 #beta1_mu
  B2 <- 0.25 #beta2_mu
  B3 <- 30 #beta3_mu
  
  sv_line <- data.frame()
  for (y in 1:Y){
    sv_line[y,"prop"] <- logit_to_prob(B0 + B1 / (1 + exp(-B2 * (y - B3))))
  }
  sv_line %>% mutate(y = seq(1,Y,1)) -> sv_line
  plot(sv_line$prop ~ sv_line$y)
  }

postmodes_B0_pelagic;ggplot(postmodes_B0_pelagic) +
  geom_histogram(aes(mode1), fill = "blue", alpha = 0.3, bins = 50) +
  geom_histogram(data = postmodes_B0_pelagic %>% filter(mode_ht2 > 0.3),
                 aes(mode2), fill = "red", alpha = 0.3, bins = 50) +
  facet_wrap(~region)

plot(postmodes_B0_pelagic$mode1[postmodes_B0_pelagic$model == "fit"]~
       postmodes_B0_pelagic$mode1[postmodes_B0_pelagic$model == "cens"])

median(postmodes_B0_pelagic$mode1); range(postmodes_B0_pelagic$mode1)

B0_pelagic_inits <- c(rep(median(postmodes_B0_pelagic$mode1[postmodes_B0_pelagic$region == "central"]),4),
                      rep(median(postmodes_B0_pelagic$mode1[postmodes_B0_pelagic$region == "Kodiak"]),6),
                      rep(median(postmodes_B0_pelagic$mode1[postmodes_B0_pelagic$region == "southeast"]),6))

postmodes_B1_pelagic; ggplot(postmodes_B1_pelagic) +
  geom_histogram(aes(mode1), fill = "blue", alpha = 0.3, bins = 50) +
  geom_histogram(data = postmodes_B1_pelagic %>% filter(mode_ht2 > 0.3),
                 aes(mode2), fill = "red", alpha = 0.3, bins = 50) +
  facet_wrap(~region)

plot(postmodes_B1_pelagic$mode1[postmodes_B1_pelagic$model == "fit"]~
       postmodes_B1_pelagic$mode1[postmodes_B1_pelagic$model == "cens"])

postmodes_B1_pelagic %>% filter(mode_ht2 > 0.33)

B1_pelagic_inits <- c(as.vector(abs(postmodes_B1_pelagic$mode1[postmodes_B1_pelagic$model == "fit"])))

postmodes_B2_pelagic; ggplot(postmodes_B2_pelagic) +
  geom_histogram(aes(mode1), fill = "blue", alpha = 0.3, bins = 50) +
  geom_histogram(data = postmodes_B2_pelagic %>% filter(mode_ht2 > 0.3),
                 aes(mode2), fill = "red", alpha = 0.3, bins = 50) +
  facet_wrap(~region)

plot(postmodes_B2_pelagic$mode1[postmodes_B2_pelagic$model == "fit"]~
       postmodes_B2_pelagic$mode1[postmodes_B2_pelagic$model == "cens"])

B2_pelagic_inits <- rnorm(16,-0.25,0.25)

postmodes_B3_pelagic; ggplot(postmodes_B3_pelagic) +
  geom_histogram(aes(mode1), fill = "blue", alpha = 0.3, bins = 50) +
  geom_histogram(data = postmodes_B3_pelagic %>% filter(mode_ht2 > 0.3),
                 aes(mode2), fill = "red", alpha = 0.3, bins = 50) +
  facet_wrap(~region)

plot(postmodes_B3_pelagic$mode1[postmodes_B3_pelagic$model == "fit"]~
       postmodes_B3_pelagic$mode1[postmodes_B3_pelagic$model == "cens"])

B3_pelagic_inits <- c(as.vector(abs(postmodes_B3_pelagic$mode1[postmodes_B3_pelagic$model == "fit"][1:10])),
                 35,35,35,35,35,35)

inits <- rbind(inits,
               B0_pelagic_inits = B0_pelagic_inits,
               B1_pelagic_inits = B1_pelagic_inits, 
               B2_pelagic_inits = B2_pelagic_inits,
               B3_pelagic_inits = B3_pelagic_inits)
getwd()
write.csv(inits,"data/bayes_dat/p_comp_inits.csv")
#-------------------------------------------------------------------------------
#black logistic priors:
post_m1$sims.list$beta0_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area)) %>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta0_black 
post_m1$sims.list$beta1_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta1_black
post_m1$sims.list$beta2_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta2_black
post_m1$sims.list$beta3_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> fit_beta3_black

post_m2$sims.list$beta0_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta0_black
post_m2$sims.list$beta1_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta1_black
post_m2$sims.list$beta2_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta2_black
post_m2$sims.list$beta3_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> cens_beta3_black

post_m3$sims.list$beta0_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta0_black
post_m3$sims.list$beta1_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta1_black
post_m3$sims.list$beta2_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta2_black
post_m3$sims.list$beta3_black %>%
  as.data.frame() %>%
  setNames(nm = unique(H_ayg$area))%>%
  pivot_longer(cols = unique(H_ayg$area),names_to = "area", values_to = "black") %>%
  mutate(region = ifelse(area %in% unique(H_ayg$area)[1:4],"central",
                         ifelse(area %in% unique(H_ayg$area)[5:10],"Kodiak","southeast")))-> hyb_beta3_black

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





