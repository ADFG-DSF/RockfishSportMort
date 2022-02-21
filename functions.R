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

