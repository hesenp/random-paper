
## this file will take a look on the data from Tianwei

library(doMC)
registerDoMC(cores=4)

source("../numerical-comparison/util.r")

load("data/GO_select_70%_40%_10.bin")

load("data/GSE10255_entrez.bin")

a <- read.table("data/GSE10255_key.txt",
                sep="\t",
                header=TRUE)

## exploration shows that not all the gene labels are available in the
## array.

test_all_methods <- function(x,y){
  ## this method will use all existing methods in the paper to
  ## identify the associations between X and Y.
  temp <- sapply(c("mu","mi","me","bro"),
                 function(method){
                   return(compare_test(x,y,method))
                 })
  return(temp)  
}

standardize <- function(input){
  ## this function will use gaussian transformation to standardize the
  ## matrix by column
  return(qnorm(apply(input,2,rank) / (1+nrow(input))))
}

new.array <- t(standardize(t(new.array)))

y <- standardize(a[,5:6])

find_association <- function(i,y){
  ## this function will find the i-th set of genes in GO.select and
  ## see if their available elements are associated with the outcome
  ## stored in y, using the methods mentioned in the paper.
  genes <- GO.select[[i]]
  if(any(genes %in% rownames(new.array))){
    ## if we have something to compare:
    x <- new.array[rownames(new.array) %in% genes,]
    if(class(x)=="matrix"){
      x <- t(x)
    }else{
      ## if x is univariate
      x <- as.matrix(x)
    }
    return(test_all_methods(x,y))
  }else{
    ## if none of the genes in the GO.select item does not show up,
    ## then return null.
    return(NULL)
  }
}

out <- mclapply(1:length(GO.select),
                function(i) find_association(i,y))
