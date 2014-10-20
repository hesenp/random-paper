
## this file contains the programs used to generate the numbers for
## permutation test. we will generate: 1) mutual information 2)
## brownian covariate, 3) mira score, 4) mean distance.

library(FNN)
library(boot)
library(energy)

## this part of the code generates the test using mutual information.

###########################################
## HOW TO USE:

## use compare_test function to generate permutation test p-values
## using one of the four methods: Mira score, mutual information, mean
## observation distance, and brownian covariate.

## for example:
## x and y are matrices of 100*10 and 100*2 dimensions, respectively.
# x <- matrix(rnorm(1000),100)
# y <- matrix(rnorm(200),100)

## to generate the p-values of Mira score, use:

## compare_test(x,y,"mira_score")

## for other methods, substitute "mira_score" with one of the
## following: "mutual_information", "mean_distance", or
## "brownian_covariate"
#################################################


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

## the function below would generate the distribution of observation
## distance and do the association testing using kolgonorove test.

dist_test_core <- function(x,y){
    return(as.numeric(dist(cbind(x,y))))
}

dist_test <- function(x,y,R=50){
    ## this function will generate the observation distances and use
    ## KS test to compare it with null distribution.
    
    test_dist <- dist_test_core(x,y)
    null_dist <- as.numeric(replicate(R,
                                      dist_test_core(x,
                                                     y[sample(nrow(y)),])))

    return(ks.test(test_dist,null_dist)$p.value)    
}

## finally, test the performance with one general portal

compare_test <- function(x,y,type,R=100){
    ## this function wraps the above operation with the dcov test in the
    ## energy function.

    p_select <- pmatch(type,c("brownian_covariate",
                              "ks_dist"))

    out <- switch(p_select,
                  dcov.test(x,y)$p.value,
                  dist_test(x,y))
    if(is.null(out)){
        out <- perm_test(x,y,type,R)
    }
    return(out)
}

## ## test
## n <- 100
## p <- 5
## a <- matrix(rnorm(n*p),n)
## b <- matrix(rnorm(n*p,sd=abs(a)),n)
## compare_test(a,b,"ks")
