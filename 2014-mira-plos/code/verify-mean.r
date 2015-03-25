
## this program is gonna verify the assumption that the mean
## observation distance is smaller for dependent pairs compared with
## independent pairs.

library(mvtnorm)
library(lattice)

mean_dist <- function(x){
  ## this function is gonna return the mean distance between
  ## observations
  return(mean(as.numeric(dist(x))))  
}

gen_ind <- function(n=100,rho=0){
  ## this function is gonna generate bivariate normal observations
  ## with covariate rho.
  temp <- rmvnorm(n,
                  mean=c(0,0),
                  sigma = matrix(c(1,rho,rho,1),2))
  return(temp)  
}

run_sim <- function(n,rho){
  return(mean_dist(gen_ind(n,rho)))
}

temp_ind <- replicate(1000,run_sim(100, 0))
temp_cor <- replicate(1000,run_sim(100, 0.9))
out <- data.frame(x = c(temp_ind, temp_cor),
                  label = rep(c("independent","correlated"),each=1000))

histogram(~x|label,data=out,layout=c(1,2))
