### Jessica Miller ###
### ST 758 Project 1 #
### Oct. 25, 2016 ####

# Load dataset
setwd("F:/ST 758/jmmill13ST758")
data <- read.csv("binarydata.csv", header=F)
                 
# Set dimensions
n <- dim(data)[2]
k <- dim(data)[1]

# Create function for model 1
model1 <- function(h, beta) {

  J <- toeplitz(c(0, beta, rep(0, times = n-2)))
  H <- vector()
  for (j in 2:(k-1)) {
    for (i in 1:(k-2)){
      H <- append(H, -sum(J[j,i]*t(as.numeric(data[j,]))%*%as.numeric(data[j+1,])) - sum(h*as.numeric(data[j,])))
    }
  }
  
  pmf <- exp(-H)/sum(exp(-H))
  return(pmf)
  
}

# Create function for model 2
model2 <- function(h, beta) {
  
  J <- toeplitz(c(0, beta, rep(0, times = n-2)))
  H <- vector()
  for (j in 2:(k-1)) {
    for (i in 1:(k-2)){
      H <- append(H, -sum(J[j,i]*t(as.numeric(data[j,]))%*%as.numeric(data[j+1,])) - sum(h*as.numeric(data[j,])))
    }
  }
  
  pmf <- exp(-H)/sum(exp(-H))
  return(pmf)
  
}

# Create function for model 3
model3 <- function(h, beta) {
  
  J <- toeplitz(c(0, beta, rep(0, times = n-2)))
  H <- vector()
  for (j in 2:(k-1)) {
    for (i in 1:(k-2)){
      H <- append(H, -sum(J[j,i]*t(as.numeric(data[j,]))%*%as.numeric(data[j+1,])) - sum(h*as.numeric(data[j,])))
    }
  }
  
  pmf <- exp(-H)/sum(exp(-H))
  return(pmf)
  
}

# Create function for exponential model
modelexp <- function(h, alpha) {
  
  J <- toeplitz(c(0, (n-k)^(-alpha), rep(0, times = n-2)))
  H <- vector()
  for (j in 2:(k-1)) {
    for (i in 1:(k-2)){
      H <- append(H, -sum(J[j,i]*t(as.numeric(data[j,]))%*%as.numeric(data[j+1,])) - sum(h*as.numeric(data[j,])))
    }
  }
  
  pmf <- exp(-H)/sum(exp(-H))
  return(pmf)
  
}

# Initialize function output matrices
one <- matrix(NA, nrow=7, ncol=324)
two <- matrix(NA, nrow=7, ncol=324)
three <- matrix(NA, nrow=7, ncol=324)
exp <- matrix(NA, nrow=7, ncol=324)

# Create various initial beta values to input to each function
betas <- c(.25, .5, 1, 1.5, 2, 3, 5)

# Run Model 1
i <- 1
for (m in betas) {
  ising1 <- model1(h=1, beta=m)
  one[i,] <- ising1
  i <- i + 1
}

# Create various initial alpha values to input to the exponential function
alphas <- c(.25, .5, 1, 1.5, 2, 3, 5)

# Run exponential model
i <- 1
for (m in betas) {
  isingexp <- modelexp(h=1, alpha=m)
  exp[i,] <- isingexp
  i <- i + 1
}

# Create contour plots
contour(one, ylim=c(0,0.01), main="Contour Plot using Model 1")
contour(exp, ylim=c(-0.003,0.01), main="Contour Plot using Exponential Model")

# Run Model 2
i <- 1
for (m in betas) {
  ising2 <- model2(h=1, beta=c(m,m))
  two[i,] <- ising2
  i <- i + 1
}

# Run Model 3
i <- 1
for (m in betas) {
  ising3 <- model3(h=1, beta=c(m,m,m))
  three[i,] <- ising3
  i <- i + 1
}