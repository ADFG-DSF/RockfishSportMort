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

