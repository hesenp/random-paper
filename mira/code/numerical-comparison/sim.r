
## this program will make use of the functions specified in util.r and
## rand-gen.r and generate the simulation comparison.

source("rand-gen.r")
source("util.r")

library(lattice)
library(doMC)
registerDoMC(cores=16)

scramble_rows <- function(data){
  ## this function will randomly permute the rows of the input data.
  ## the result is used to be balanced in simulation.
  return(data[sample(1:nrow(data)),])
}

gen_association <- function(select){
  ## this function will select between three methods for the
  ## simulation.
  p <- pmatch(select,c("lin_sim","var_sim","cube_sim"))
  return(switch(p,
                lin_sim,var_sim,cube_sim))
}

## let's first see how things behave with linear models. we are gonna
## look at the power for linear models.

R <- 1000

scenarios <- scramble_rows(expand.grid(association = c("lin","var","cube"),
                                       method = c("mutual_information",
                                           "mira_score",
                                           "mean_distance",
                                           "brownian_covariate",
                                           "ks"),
                                       n = seq(40,1000,by=20)))

out <- foreach(i= 1:nrow(scenarios),
               .combine = rbind) %dopar% {
  replicate(R, {
    temp <- gen_association(scenarios[i,1])(scenarios[i,3], 5)
    return(compare_test(temp$x,temp$y,type = scenarios[i,2]))
  })
}

save(out,scenarios,
     file="output/sim-p-value-with-ks.RData")

load("output/sim-p-value-with-ks.RData")

oo <- cbind(scenarios,
            apply(out,1,
                  function(x){
                    sum(x<=0.05)/R
                  }))
names(oo)[4] <- "power"

xyplot(power~n|association,
       group=method,
       data = oo,
       auto.key=TRUE,
       type="a")
