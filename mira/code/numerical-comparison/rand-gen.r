
## this program specifies the random variable situations for
## comparison.

library(mvtnorm)

lin_sim <- function(n,p,rho=0.1){
  ## this function will generate a bunch of linear assocations between
  ## X and Y (multi-variate normal distribution). we'll see how that
  ## goes.
  temp_sigma <- matrix(rep(rho,(2*p)^2),2*p)
  diag(temp_sigma) <- 1  
  temp <- rmvnorm(n,
                  mean = rep(0,2*p),
                  sigma = temp_sigma)
  return(list(x = temp[,1:p],
              y = temp[,(p+1):(2*p)]))
}

var_sim <- function(n,p){
  ## this will generate random samples where the standard deviance of
  ## one is the absolute value of another.
  x <- matrix(rnorm(n*p),n)
  y <- rnorm(n*p, 0, abs(x))
  return(list(x= x,
              y= matrix(y,n)))  
}

cube_sim <- function(n,p){
  ## this will simulate the three way interaction relationship.
  x <- matrix(rnorm(2*n*p),n)
  y <- sign(x[,1:p]*x[,(p+1):(2*p)])*abs(matrix(rnorm(n*p),n))
  return(list(x=x,y=y))    
}
