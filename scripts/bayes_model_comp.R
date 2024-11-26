# Savage-Dickey Density Ratio

# Assume posterior_samples contains samples of beta1
posterior_density <- density(posterior_samples$beta1)
posterior_at_null <- approx(posterior_density$x, posterior_density$y, xout = 0)$y

# Compute the prior density at the same point (use the prior distribution)
prior_at_null <- dnorm(0, mean = prior_mean, sd = prior_sd)

bayes_factor <- posterior_at_null / prior_at_null

# Bridge sampleing
library(bridgesampling)
library(BayesTools)

JAGS_bridgesampling(
  fit,
  log_posterior,
  data = NULL,
  prior_list = NULL,
  formula_list = NULL,
  formula_data_list = NULL,
  formula_prior_list = NULL,
  add_parameters = NULL,
  add_bounds = NULL,
  maxiter = 10000,
  silent = TRUE,
  ...
)

#Examples
#Run this code
if (FALSE) {
  # simulate data
  set.seed(1)
  data <- list(
    x = rnorm(10),
    N = 10
  )
  data$x
  
  # define priors
  priors_list <- list(mu = prior("normal", list(0, 1)))
  
  # define likelihood for the data
  model_syntax <-
    "model{
    for(i in 1:N){
      x[i] ~ dnorm(mu, 1)
    }
  }"
  
  # fit the models
  fit <- JAGS_fit(model_syntax, data, priors_list)
  
  # define log posterior for bridge sampling
  log_posterior <- function(parameters, data){
    sum(dnorm(data$x, parameters$mu, 1, log = TRUE))
  }
  
  # get marginal likelihoods
  marglik <- JAGS_bridgesampling(fit, log_posterior, data, priors_list)
}