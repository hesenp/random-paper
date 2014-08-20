
## this program would explore the dataset that we can have for the
## functionals on observation graph paper.

data(iris)

pairs(iris)

source("../numerical-comparison/util.r")

compare_test(as.matrix(iris[,1]),
             as.matrix(iris[,2]),"me")



a <- read.csv("data/seeds_dataset.txt",
              sep="\t",header=FALSE)
