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
n <- 1000
Y <- 47

{
  tau_mu0 <- 0.25
  mu_beta0 <- rnorm(n,-0.25,1/sqrt(tau_mu0)) #rnorm(n,-0.25,1)
tau_beta0 <- rgamma(n,0.001,0.001)

beta0 <- rnorm(n,mu_beta0, tau_beta0) #INTERCEPT

tau1 <- 0.25
beta1 <- rnorm(3*n,0, 1/sqrt(tau1))
beta1 <- beta1[beta1 > 0]; beta1 <- sample(beta1,n,replace = F) # SCALING FACTOR (amplitude) #rep(2,n)

tau2 <- 5
beta2 <- rnorm(n*3,0,  1/sqrt(tau2)) #0.1
beta2 <- beta2[beta2 > 0]; beta2 <- sample(beta2,n,replace = F) #T(0,)          # SLOPE

tau3 <- 0.005#sd = 10
beta3 <- rnorm(n*3,34,  1/sqrt(tau3)); beta3 <- beta3[beta3 > 0 & beta3 < Y-3]; beta3 <- sample(beta3,n,replace = F) #T(0,Y-3)    # INFLECTION POINT 

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

ggplot(pr_sim) +
  geom_histogram(aes(beta0), fill = "blue", alpha = 0.2) +
  geom_vline(xintercept = 1.081984, color = "blue") +#,breaks = 50, fill = "red", alpha = 0.2)
  geom_histogram(aes(beta1), fill = "red", alpha = 0.2) +
  geom_vline(xintercept = -1.427739, color = "red") +
  geom_histogram(aes(beta2), fill = "forestgreen", alpha = 0.2) +
  geom_vline(xintercept = -0.2892534, color = "forestgreen") +#,breaks = 50, fill = "red", alpha = 0.2)
  geom_histogram(aes(beta3), fill = "blue", alpha = 0.2) +
  geom_vline(xintercept = 34.79994, color = "blue")
  


#-------------------------------------------------------------------------------
#comp logistic priors:
n <- 1000
Y <- 47

{
  tau_mu0 <- 0.1
  mu_beta0 <- rnorm(n,-0.5,1/sqrt(tau_mu0)) #rnorm(n,-0.25,1)
  tau_beta0 <- rgamma(n,0.001,0.001)
  
  beta0 <- rnorm(n,mu_beta0, tau_beta0) #INTERCEPT
  
  tau1 <- 0.25
  beta1 <- rnorm(n,0, 1/sqrt(tau1))
 # beta1 <- beta1[beta1 > 0]; beta1 <- sample(beta1,n,replace = F) # SCALING FACTOR (amplitude) #rep(2,n)
  
  tau2 <- 1
  beta2 <- rep(-5,n) #rnorm(n,0,  1/sqrt(tau2)) #0.1
  #beta2 <- beta2[beta2 > 0]; beta2 <- sample(beta2,n,replace = F) #T(0,)          # SLOPE
  
  tau3 <- 0.005#sd = 10
  beta3 <- rnorm(n*3,34,  1/sqrt(tau3)); beta3 <- beta3[beta3 > 0 & beta3 < Y-3]; beta3 <- sample(beta3,n,replace = F) #T(0,Y-3)    # INFLECTION POINT 
  
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

ggplot(pr_sim) +
  #geom_histogram(aes(beta0), fill = "blue", alpha = 0.2) +
  #geom_vline(xintercept = -0.3006037, color = "blue") +#YE
  #geom_vline(xintercept = 1.017154, color = "blue") +#black
  #geom_vline(xintercept = 1.450994, color = "blue") +#pelagic
  geom_histogram(aes(beta1), fill = "red", alpha = 0.2) +
  geom_vline(xintercept = -0.155412, color = "red") +
  geom_vline(xintercept = -1.138323, color = "red") +
  geom_vline(xintercept = 0.9330462, color = "red") +
  geom_histogram(aes(beta2), fill = "forestgreen", alpha = 0.2) +
  geom_vline(xintercept = -0.89695, color = "forestgreen") +#,breaks = 50, fill = "red", alpha = 0.2)
  geom_vline(xintercept = -1.917029, color = "forestgreen") +
  geom_vline(xintercept = -0.844492, color = "forestgreen") +
  geom_histogram(aes(beta3), fill = "blue", alpha = 0.2) +
  geom_vline(xintercept = 32.78195, color = "blue") +
  geom_vline(xintercept = 41.98974, color = "blue") + 
  geom_vline(xintercept = 24.61498, color = "blue")

pr_sim %>% filter(beta0 < -10)












