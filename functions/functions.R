#Penalized spline regression taken from https://bragqut.github.io/2016/05/24/samclifford-splines/
bspline <- function(x, K, bdeg=3, cyclic=FALSE, xl=min(x), xr=max(x)){
  x <- as.matrix(x,ncol=1)
  
  ndx <- K - bdeg
  
  # as outlined in Eilers and Marx (1996)
  dx <- (xr - xl) / ndx
  t <- xl + dx * (-bdeg:(ndx+bdeg))
  T <- (0 * x + 1) %*% t
  X <- x %*% (0 * t + 1)
  P <- (X - T) / dx
  B <- (T <= X) & (X < (T + dx))
  r <- c(2:length(t), 1)
  
  for (k in 1:bdeg){
    B <- (P * B + (k + 1 - P) * B[ ,r]) / k; 
  }
  
  B <- B[,1:(ndx+bdeg)]
  
  if (cyclic == 1){
    for (i in 1:bdeg){
      B[ ,i] <- B[ ,i] + B[ ,K-bdeg+i]    
    }
    B <- B[ , 1:(K-bdeg)]
  }
  
  return(B)
}



X <- bspline(1:24, 11, xl=1, xr=24)

makeQ = function(degree, K, epsilon=1e-3){
  x <- diag(K)
  E <- diff(x, differences=degree)
  return( t(E) %*% E + x*epsilon)
} 

long <- function(df){
  t(df) %>%
    as.data.frame() %>%
    setNames(unique(H_ayg$area)) %>%
    mutate(year = unique(Hhat_ay$year)) %>%
    pivot_longer(-year, names_to = "area", values_to = "val") %>%
    mutate(val = round(val, 2)) %>%
    mutate(area = factor(area, unique(H_ayg$area), ordered = TRUE))
}

tableH <- function(mean, sd, lb, ub){
  mean %>%
    left_join(sd, c("year", "area")) %>%
    left_join(lb, c("year", "area")) %>% 
    left_join(ub, c("year", "area")) %>%
    rename(mean = val.x,
           sd = val.y,
           q2.5 = val.x.x,
           q97.5 = val.y.y)
}

get_Rhat <- function(post, cutoff = 1.1){
  list(
    data.frame("Rhat" = post$summary[, "Rhat"][post$summary[, "Rhat"] > cutoff & !is.na(post$summary[, "Rhat"])]),
    "R^ quantiles" = quantile(post$summary[, "Rhat"], probs = seq(0.9, 1, by = .01), na.rm = TRUE))
}