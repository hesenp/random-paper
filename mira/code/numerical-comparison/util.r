
## this file contains the programs used to generate the numbers for
## permutation test. we will generate: 1) mutual information 2)
## brownian covariate, 3) mira score, 4) mean distance.

library(FNN)
library(boot)
library(energy)

## this part of the code generates the test using mutual information.

dist_fun <- function(x,y,type){
  ## this function will return the values on the observation graph
  ## subject to the specification of the type field. current planned
  ## types are below.
  p <- pmatch(type,
              c("mutual_information",
                "mira_score",
                "mean_distance"))
  if(p %in% c(1,2)){
    temp <- get.knn(cbind(x,y),k=1)$nn.dist
    return(switch(p,
                  mean(log(temp)),
                  mean(temp)))
  }else{
    temp <- as.numeric(dist(cbind(x,y)))
    return(mean(temp))
  }
}

perm_test <- function(x,y,type,R=100){
  boot_core <- function(original,index,type){
    return(dist_fun(original[index,],y,type))
  }
  out <- boot(data=x,
              statistic = boot_core,
              R=R,
              sim="permutation",
              type=type)
  return(pnorm(out$t0,mean(out$t),sd(out$t)))
}

compare_test <- function(x,y,type,R=100){
  ## this function wraps the above operation with the dcov test in the
  ## energy function.
  if(type=="brownian_covariate"){
    return(dcov.test(x,y)$p.value)
  }else{
    return(perm_test(x,y,type,R))
  }
}

