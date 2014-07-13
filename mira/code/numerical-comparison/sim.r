
## this program will make use of the functions specified in util.r and
## rand-gen.r and generate the simulation comparison.

source("rand-gen.r")
source("util.r")

library(lattice)
library(doMC)
registerDoMC(cores=3)

## let's first see how things behave with linear models. we are gonna
## look at the power for linear models.

R <- 1000

scenarios <- expand.grid(method = c("mutual_information",
                           "mira_score",
                           "mean_distance",
                           "brownian_covariate"),
                         n = seq(40,1000,by=20))


out <- foreach(i= 1:nrow(scenarios), .combine = rbind) %dopar% {
  replicate(R, {
    temp <- lin_sim(scenarios[i,2], 3, rho = 0.1)
    return(compare_test(temp$x,temp$y,type = scenarios[i,1]))
  })
}

save(out,file="output/linear-comparison.RData")

oo <- apply(out,1,
            function(x){
              sum(x<=0.05)
            })

sc <- cbind(scenarios,oo)

xyplot(oo~n,group = method, data= sc, type="l",auto.key=TRUE)
