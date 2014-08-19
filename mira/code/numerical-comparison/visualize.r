
library(lattice)

load("output/sim-p-value.RData")

power <- 0.05

scenarios$power <- apply(out, 1,
                         function(x){
                           sum(x<power)
                         })/ncol(out)

trellis.device(device = "postscript",
               file = "sim-power-linear.eps",
               width = 6,
               height = 6,
               horizontal = FALSE,
               paper = "special")
xyplot(power~n|association,
       group = method,
       data = scenarios,
       type = "a",
       auto.key=list(space = "bottom",points=FALSE, lines=TRUE, columns=2),
       xlab = "Sample Size",
       ylab = "Power",
       subset = scenarios == "lin")
dev.off()

trellis.device(device = "postscript",
               file = "sim-power-nonlinear.eps",
               width = 10,
               height = 6,
               horizontal = FALSE,
               paper = "special")
xyplot(power~n|association,
       group = method,
       data = scenarios,
       type = "a",
       auto.key=list(space = "bottom",points=FALSE, lines=TRUE, columns=4),
       layout = c(2,1),
       xlab = "Sample Size",
       ylab = "Power",
       subset = scenarios != "lin" & n<=700)
dev.off()
