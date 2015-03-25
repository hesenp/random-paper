
## this program will generate the 2d plot example for illustration in
## the paper.

library(FNN)
library(mvtnorm)
library(xtable)

source("../numerical-comparison/util.r")

gen_stat <- function(x,y,methods){
  ## this function would generate the three types of statistics that
  ## the dist_fun function calculates for comparison purpose.
  oo <- sapply(methods,
               function(i){
                 dist_fun(x,y,i)
               })
  return(oo)  
}

find_nn <- function(x){
  ## this function takes the input observation matrix X and returns
  ## the corresponding nearest neighbore positions corresponding to
  ## the X.
  temp <- get.knn(x,1)
  print(mean(temp$nn.dist))
  return(x[temp$nn.index,])  
}

plot_nn <- function(x,main="",xlim,ylim){
  ## this function will plot the observation points x and connect them
  ## to their nearest neighbores.

  temp <- find_nn(x)
  plot(x[,1],x[,2],xlab="X",ylab="Y",main=main)
  for(i in 1:nrow(temp)){
    lines(c(temp[i,1],x[i,1]),c(temp[i,2],x[i,2]),lty=2)
  }
}


compare_plot <- function(n=100, rho=0.8, main=""){
  ## this function would do all the plotting work.

  x <- rmvnorm(n,mean=c(0,0),sigma = matrix(c(1,0,0,1),2))
  xx <- rmvnorm(n,mean=c(0,0),sigma = matrix(c(1,rho,rho,1),2))

  xx[,2] <- xx[,2]*sample(c(1,-1),n,replace=TRUE)
  xlim <- range(c(x[,1],xx[,1]))
  ylim <- range(c(x[,2],xx[,2]))

  par(mfcol=c(1,2))
  plot_nn(x,xlim=xlim,ylim=ylim)
  plot_nn(xx,xlim=xlim,ylim=ylim)

  methods <- c("mutual_information","mira_score","mean_distance")
  out <- data.frame(metric=methods,
                    left=gen_stat(x[,1],x[,2],methods),
                    right=gen_stat(xx[,1],xx[,2],methods))
  rownames(out) <- NULL
  return(out)
}

postscript(file = "plot/example-1.eps",
           width=11,
           height=7,
           horizontal=FALSE,
           paper="special")
u <- compare_plot(300)
dev.off()

temp <- xtable(u,
               label = "tab:example-compare",
               digits = 2)
