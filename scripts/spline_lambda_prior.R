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
