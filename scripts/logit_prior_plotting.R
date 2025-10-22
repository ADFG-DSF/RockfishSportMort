source(".\\scripts//bayes_data_param_load.R")

logit_to_prob(-10)

plot_logit <- function(n = 5000,
                       Y,b0_mu,sd_b0,
                       b1_mu,sd_b1,
                       b2_mu,sd_b2,
                       b3_lo,b3_hi,
                       b4_mu,sd_b4){
  #n <- 5000
  
  b0 <- rnorm(n,b0_mu,sd_b0)
  b1 <- rlnorm(n,log(b1_mu),sd_b1)
  b2 <- rnorm(n,b2_mu,sd_b2)
  b3 <- runif(n,b3_lo,b3_hi)
  #b4 <- rlnorm(n,log(b4_mu),sd_b4)
  b4 <- rnorm(n,b4_mu,sd_b4)
  
  cbind(b0,b1,b2,b3,b4) %>% data.frame -> pr_sim
  props <- data.frame(matrix(nrow = n, ncol=Y)) 
  colnames(props) <- seq(1,Y,1)
  props2 <- data.frame(matrix(nrow = n, ncol=Y)) 
  colnames(props2) <- seq(1,Y,1)
  
  for (i in 1:n) { #i <- 1
    for (y in 1:Y){
      props[i,y] <- logit_to_prob(pr_sim$b0[i] +
                                    pr_sim$b1[i] / (1 + exp(-pr_sim$b2[i] * (y - pr_sim$b3[i])))) * pr_sim$b4[i]
      props2[i,y] <- logit_to_prob(pr_sim$b0[i] +
                                    pr_sim$b1[i] / (1 + exp(-pr_sim$b2[i] * (y - pr_sim$b3[i])))) 
    }
  }
  
  props %>% mutate(iter = row_number()) %>%
    pivot_longer(cols = seq(1,Y,1),
                 names_to = "year",
                 values_to = "prop") %>%
    mutate(year = as.numeric(year)) %>%
    data.frame() -> props
  
  props2 %>% mutate(iter = row_number()) %>%
    pivot_longer(cols = seq(1,Y,1),
                 names_to = "year",
                 values_to = "prop") %>%
    mutate(year = as.numeric(year)) %>%
    data.frame() -> props2
  #str(props)
  
  props %>% group_by(year) %>%
    dplyr::summarise(mean_prop = mean(prop),
                     median = median(prop),
                     lo50 = as.numeric(quantile(prop,c(0.25))),
                     hi50 = as.numeric(quantile(prop,c(0.75))),
                     lo95 = as.numeric(quantile(prop,c(0.025))),
                     hi95 = as.numeric(quantile(prop,c(0.975))),
                     lo24 = as.numeric(quantile(prop,c(0.38))),
                     hi24 = as.numeric(quantile(prop,c(0.62))))  -> mean
  
  props2 %>% group_by(year) %>%
    dplyr::summarise(mean_prop = mean(prop),
                     median = median(prop),
                     lo50 = as.numeric(quantile(prop,c(0.25))),
                     hi50 = as.numeric(quantile(prop,c(0.75))),
                     lo95 = as.numeric(quantile(prop,c(0.025))),
                     hi95 = as.numeric(quantile(prop,c(0.975))),
                     lo24 = as.numeric(quantile(prop,c(0.38))),
                     hi24 = as.numeric(quantile(prop,c(0.62))))  -> mean2
  
  ggplot() +
  #ggplot(props,aes(x = year, y = prop)) +
    geom_ribbon(data = mean, 
                aes(x = year,y = mean_prop, ymin = lo95,ymax = hi95),color = NA, fill = "blue", alpha = 0.2) +
    geom_ribbon(data = mean, aes(x = year,y = mean_prop, ymin = lo50,ymax = hi50),color = NA, fill = "blue",alpha = 0.2) +
    geom_ribbon(data = mean, aes(x = year,y = mean_prop, ymin = lo24,ymax = hi24),color = NA, fill = "blue",alpha = 0.2) +
    
    geom_ribbon(data = mean2, 
                aes(x = year,y = mean_prop, ymin = lo95,ymax = hi95),color = NA, fill = "green", alpha = 0.2) +
    geom_ribbon(data = mean2, aes(x = year,y = mean_prop, ymin = lo50,ymax = hi50),color = NA, fill = "green",alpha = 0.2) +
    geom_ribbon(data = mean2, aes(x = year,y = mean_prop, ymin = lo24,ymax = hi24),color = NA, fill = "green",alpha = 0.2) +
    
    #geom_line(aes(group = iter),alpha = 0.05, color = "blue") +  # Adjust alpha for transparency
    geom_line(data = mean, aes(x = year, y = mean_prop),color = "blue", linetype = 2) + #,aes(x = year, y = prop)) +
    geom_line(data = mean, aes(x = year, y = median),color = "blue", linetype = 1) +
    geom_line(data = mean2, aes(x = year, y = mean_prop),color = "green", linetype = 2) + #,aes(x = year, y = prop)) +
    geom_line(data = mean2, aes(x = year, y = median),color = "green", linetype = 1) +
    labs(
      title = "Lines for Each Row in props",
      x = "y",
      y = "Probability"
    ) + ylim(0,1) +
    theme_minimal()
}

#test with no uncertainty:
plot_logit(n = 10,
           Y = 47, b0_mu <- 0,
           sd_b0 <- 0.1,
           b1_mu <- 1,
           sd_b1 <- 0.1,
           b2_mu <- 0.5,
           sd_b2 <- 0.1,
           b3_lo <- 18,
           b3_hi <- 42,
           b4_mu <- -0.01
           ,
           sd_b4 <- 0)


#fixed black
plot_logit(Y = 47, b0_mu <- 0,
           sd_b0 <- 1,
           b1_mu <- 0.5,
           sd_b1 <- 0.0001,
           b2_mu <- 1,
           sd_b2 <- 0.0001,
           b3_lo <- 25,
           b3_hi <- 45,
           b4_mu <- 0.5,
           sd_b4 <- 0)

#Central pelagic pH
plot_logit(Y = 47, b0_mu <- 1,
           sd_b0 <- 1,
           b1_mu <- 1.5,
           sd_b1 <- 1,
           b2_mu <- 0.5,
           sd_b2 <- 2,
           b3_lo <- 19,
           b3_hi <- 46)

#Central YE pH
plot_logit(Y = 47, b0_mu <- 2,
           sd_b0 <- 1,
           b1_mu <- 2,
           sd_b1 <- 1,
           b2_mu <- -1,
           sd_b2 <- 2,
           b3_lo <- 19,
           b3_hi <- 46)

#Central nonpel nonye pH
plot_logit(Y = 47, b0_mu <- 1,
           sd_b0 <- 1,
           b1_mu <- 2.5,
           sd_b1 <- 1,
           b2_mu <- 0.25,
           sd_b2 <- 2,
           b3_lo <- 19,
           b3_hi <- 46)

#Kodiak pelagic pH
plot_logit(Y = 47, b0_mu <- -0.5,
           sd_b0 <- 1,
           b1_mu <- 2,
           sd_b1 <- 1,
           b2_mu <- 0.5,
           sd_b2 <- 2,
           b3_lo <- 19,
           b3_hi <- 43)

#Kodiak YE pH
plot_logit(Y = 47, b0_mu <- 3,
           sd_b0 <- 1,
           b1_mu <- 0.001,
           sd_b1 <- 1,
           b2_mu <- 0.2,
           sd_b2 <- 2,
           b3_lo <- 19,
           b3_hi <- 46)

#Kodiak nonpel nonye pH
plot_logit(Y = 47, b0_mu <- -0.5,
           sd_b0 <- 1,
           b1_mu <- 2.5,
           sd_b1 <- 1,
           b2_mu <- 0.5,
           sd_b2 <- 2,
           b3_lo <- 19,
           b3_hi <- 46)

#SE pelagic pH
plot_logit(Y = 47, b0_mu <- -0.5,
           sd_b0 <- 1,
           b1_mu <- 3,
           sd_b1 <- 1,
           b2_mu <- 0.75,
           sd_b2 <- 2,
           b3_lo <- 29,
           b3_hi <- 43)

#SE YE pH
plot_logit(Y = 47, b0_mu <- -3,
           sd_b0 <- 1,
           b1_mu <- 6,
           sd_b1 <- 1,
           b2_mu <- -3,
           sd_b2 <- 1,
           b3_lo <- 29,
           b3_hi <- 46)

#SE DSR pH 
plot_logit(Y = 47, b0_mu <- -1,
           sd_b0 <- 1,
           b1_mu <- 3,
           sd_b1 <- 1,
           b2_mu <- -3,
           sd_b2 <- 1,
           b3_lo <- 29,
           b3_hi <- 46)

#SE Slope pH 
plot_logit(Y = 47, b0_mu <- -2,
           sd_b0 <- 1,
           b1_mu <- 5,
           sd_b1 <- 1,
           b2_mu <- -3,
           sd_b2 <- 1,
           b3_lo <- 29,
           b3_hi <- 46)




hist(rlnorm(1000,log(2),0.1))






#--------------------------------------------------------------------------------
n <- 1000
Y <- 47

b0_mu <- 1
sd_b0 <- 1
b1_mu <- 1.5
sd_b1 <- 1
b2_mu <- 0.5
sd_b2 <- 1
b3_mu <- 30
sd_b3 <- 1

{
  b0 <- rnorm(n,b0_mu,sd_b0)
  b1 <- rlnorm(n,b1_mu,sd_b1)
  b2 <- rnorm(n,b2_mu,sd_b2)
  b3 <- rnorm(n,b3_mu,sd_b3)
  
  cbind(b0,b1,b2,b3) %>% data.frame -> pr_sim
  props <- data.frame(matrix(nrow = n, ncol=Y)) 
  colnames(props) <- seq(1,Y,1)
  
  for (i in 1:n) { #i <- 1
    for (y in 1:Y){
      props[i,y] <- logit_to_prob(pr_sim$b0[i] +
                                    pr_sim$b1[i] / (1 + exp(-pr_sim$b2[i] * (y - pr_sim$b3[i]))))
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
                     median = median(prop),
                     lo50 = as.numeric(quantile(prop,c(0.25))),
                     hi50 = as.numeric(quantile(prop,c(0.75))),
                     lo95 = as.numeric(quantile(prop,c(0.025))),
                     hi95 = as.numeric(quantile(prop,c(0.975))),
                     lo24 = as.numeric(quantile(prop,c(0.38))),
                     hi24 = as.numeric(quantile(prop,c(0.62))))  -> mean
  
  ggplot(props,aes(x = year, y = prop)) +
    geom_ribbon(data = mean, 
                aes(y = mean_prop, ymin = lo95,ymax = hi95),color = NA, fill = "blue", alpha = 0.2) +
    geom_ribbon(data = mean, aes(y = mean_prop, ymin = lo50,ymax = hi50),color = NA, fill = "blue",alpha = 0.2) +
    geom_ribbon(data = mean, aes(y = mean_prop, ymin = lo24,ymax = hi24),color = NA, fill = "blue",alpha = 0.2) +
    #geom_line(aes(group = iter),alpha = 0.05, color = "blue") +  # Adjust alpha for transparency
    geom_line(data = mean, aes(x = year, y = mean_prop),color = "blue", linetype = 2) + #,aes(x = year, y = prop)) +
    geom_line(data = mean, aes(x = year, y = median),color = "blue", linetype = 1) +
    labs(
      title = "Lines for Each Row in props",
      x = "y",
      y = "Probability"
    ) + ylim(0,1) +
    theme_minimal()
}
