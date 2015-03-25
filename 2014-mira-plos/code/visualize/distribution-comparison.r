
## this program would compare the distributions of observation
## distances under different scenarios.

library(lattice)

source("../numerical-comparison/rand-gen.r")

gen_association <- function(select){
  ## this function will select between three methods for the
  ## simulation.
  p <- pmatch(select,c("lin_sim","var_sim","cube_sim"))
  return(switch(p,
                lin_sim,var_sim,cube_sim))
}

dist_gen_core <- function(x,y){
  return(as.numeric(dist(cbind(x,y))))
}

distance_gen <- function(scenario,n=300,p=5){
  ## this function would generate the distribution of observation
  ## distances under different scenarios. it will then be compared
  ## with its null after random permutation.
  temp <- gen_association(scenario)(n,p)
  dist_test <- dist_gen_core(temp$x,temp$y)
  dist_ind <- dist_gen_core(temp$x,temp$y[sample(nrow(temp$y)),])  
  return(data.frame(distance = c(dist_test,dist_ind),
                    label = rep(c("Dependent","Independent"),
                      each=length(dist_test))))
}

temp_1 <- distance_gen("lin_sim",n=500, p=10)
temp_2 <- distance_gen("var_sim",n=500, p=10)
temp_3 <- distance_gen("cube_sim",n=500, p=10)

out <- rbind(temp_1,
             temp_2,
             temp_3)
out$scenario <- rep(c("Linear", "Var", "Cube"),
                    c(nrow(temp_1),nrow(temp_2),nrow(temp_3)))


trellis.device(device = "postscript",
               file = "plot/distance-dist.eps",
               width = 6,
               height = 6,
               horizontal = FALSE,
               paper = "special")
densityplot(~distance|scenario,
            group=label,
            data=out,
            plot.points=FALSE,
            subset = scenario == "Var",
            auto.key=list(space = "bottom",
              points=FALSE,
              lines=TRUE,
              columns=2),
            xlab="Observation Distance")
dev.off()

trellis.device(device = "postscript",
               file = "plot/distance-dist-line-cube.eps",
               width = 10,
               height = 6,
               horizontal = FALSE,
               paper = "special")
densityplot(~distance|scenario,
            group=label,
            data=out,
            plot.points=FALSE,
            subset = scenario %in% c("Cube","Linear"),
            auto.key=list(space = "bottom",
              points=FALSE,
              lines=TRUE,
              columns=2),
            xlab="Observation Distance",
            xlim=c(2,6))
dev.off()

## now take a look on why brownian covariate does not perform well
## under nonlinear associations.

library(hexbin)

var_distance_core <- function(x){
  ## how come R does not have lambda form? 
  return(as.numeric(dist(x)))
}

var_distance <- function(x,y){
  ## this function will calculate the observation distances between
  ## two random vectors and return the distance pairs.
  xx <- var_distance_core(x)
  yy <- var_distance_core(y)
  return(cbind(xx,yy))  
}

temp <- gen_association("lin")(500,10)
tt <- var_distance(temp$x,temp$y)

u <- hexbin(tt[,1],tt[,2])
plot(u)
