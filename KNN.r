# K-Nearest Neighbor Function (from scratch)

# First we simulate some data

# generate covariates, p = 5
set.seed(1)
x1 <- rnorm(1000, mean = 0, sd = 1)
set.seed(2)
x2 <- rnorm(1000, mean = 0, sd = 1)
set.seed(3)
x3 <- rnorm(1000, mean = 0, sd = 1)
set.seed(4)
x4 <- rnorm(1000, mean = 0, sd = 1)
set.seed(5)
x5 <- rnorm(1000, mean = 0, sd = 1)
set.seed(6)
error <- rnorm(1000, mean = 0, sd = 1)
y <- x1 + 0.5 * x2 - x3 + error
X <- matrix(c(x1,x2,x3,x4,x5),nrow = 1000, ncol= 5)
xtrain <- X[1:500,]
xtest <- X[501:1000,]
ytrain <- y[1:500]
ytest <- y[501:1000]

# write the euclidean distance function
euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:length(a)))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

# write the myknn function
myknn <- function(xtest,xtrain,ytrain,k){
  predict <- c()
  for (i in c(1:nrow(xtest))){
    eu_dist = c()
    eu_char = c()
    for (j in c(1:nrow(xtrain))){
      eu_dist <- c(eu_dist,euclideanDist(xtest[i,],xtrain[j,]))
      # eu_char  <- c(eu_char, ytrain[j])
    }
    eu <- data.frame(ytrain,eu_dist)
    eu <- eu[order(eu$eu_dist),]
    eu <- eu[1:k,] # get k nearest neighbors
    predict <- c(predict, mean(eu$ytrain)) # average all the neighbors
  }
  return(predict)
}

# prediction accuracy
pred_accuracy <- function(prediction,truevalue){
  output = 0
  for (i in c(1:length(prediction))){
    output <- output + (prediction[i]-truevalue[i])^2
  }
  output = output/length(prediction)
  return(output)
}

pred_accuracy(myknn(xtest,xtrain,ytrain,5),ytest)
