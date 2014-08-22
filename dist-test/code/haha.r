
## this program will explore new methods for association detection
## based on distribution of sample distances.

library(mvtnorm)

n <- 100
rho <- 0.3

a <- rmvnorm(n,c(0,0),diag(2))
b <- rmvnorm(n,
             c(0,0),
             matrix(c(1,rho,rho,1),2))

aa <- as.numeric(dist(a))
bb <- as.numeric(dist(b))
