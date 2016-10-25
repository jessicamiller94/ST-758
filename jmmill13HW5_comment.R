#comment: you did not do comparison, and I don't see any explanation about where you tried to improve -1.5
#comment: you did not try different block sizes or compare between them -1
#comment: residuals should take absolute value when you plot them, to make your plot more understandable -0.5

### Jessica Miller ###
###   ST 758 HW 5  ###
### Sept. 27, 2016 ###

## PROBLEM 1

library(mvtnorm) # Used to generate b from MVN dist'n

pcg <- function(coeff, precond, b, n) {
  A <- coeff
  M <- precond
  
  x <- matrix(NA, nrow=dim(A)[1], ncol=n)
  x[,1] <- rep(0, dim(A)[1])                     # set initial value to 0
  r <- matrix(NA, nrow=dim(A)[1], ncol=n)
  r[,1] <- b - A %*% x[,1]
  z <- matrix(NA, nrow=dim(A)[1], ncol=n)
  z[,1] <- solve(M) %*% r[,1]
  p <- matrix(NA, nrow=dim(A)[1], ncol=n)
  p[,1] <- z[,1]
  a <- vector(length=n)
  
  for (k in 2:n) { # indexed by 1, not by 0
    Ap_kminus1 <- A %*% p[,k-1]
    r[,k-1] <- b - A %*% x[,k-1]
    a[k-1] <- (t(r[,k-1]) %*% z[,k-1]) / (t(p[,k-1]) %*% Ap_kminus1)
    x[,k] <- x[,k-1] + a[k-1] * p[,k-1]
    r[,k] <- r[,k-1] - a[k-1] * Ap_kminus1
    z[,k] <- solve(M) %*% r[,k]
    p[,k] <- z[,k] + ((t(r[,k]) %*% z[,k])/(t(r[,k-1]) %*% z[,k-1])) %*% p[,k-1]
  }
  
  return(x)
}

## Test function using covariance matrices from HW 4:
# Generate 4000x4000 covariance matrices
cov1 <- matrix(NA, nrow=4000, ncol=4000)
cov2 <- matrix(NA, nrow=4000, ncol=4000)
cov3 <- matrix(NA, nrow=4000, ncol=4000)
cov4 <- matrix(NA, nrow=4000, ncol=4000)
for (i in 1:4000) {
  for (j in 1:4000) {
    h <- i-j
    cov1[i,j] <- (abs(h)/4)*besselK(abs(h)/4, nu=1)
    cov2[i,j] <- (1+(abs(h)/4))*exp(-abs(h)/4)
    cov3[i,j] <- exp(-((h/4)^2))
    if (h == 0) { cov4[i,j] <- 1 }
    if (h > 0) { cov4[i,j] <- (h-1+0.4)*cov4[i-1,j]/(h-0.4) }
  }
}
diag(cov1) <- 1             # Set the variance equal to 1
cov4 <- toeplitz(cov4[,1])  # Fix the 4th covariance matrix

# Define the preconditioner M as block diag of cov. matrix, block size = 1
M1 <- matrix(0, nrow=dim(cov1)[1], ncol=dim(cov1)[2])
diag(M1) <- diag(cov1)
M2 <- matrix(0, nrow=dim(cov2)[1], ncol=dim(cov2)[2])
diag(M2) <- diag(cov2)
M3 <- matrix(0, nrow=dim(cov3)[1], ncol=dim(cov3)[2])
diag(M3) <- diag(cov3)
M4 <- matrix(0, nrow=dim(cov4)[1], ncol=dim(cov4)[2])
diag(M4) <- diag(cov4)

# Define b, as in Ax=b
b1 <- rmvnorm(n = dim(cov1)[1], sigma=cov1, method="chol")
b2 <- rmvnorm(n = dim(cov2)[1], sigma=cov2, method="chol")
b3 <- rmvnorm(n = dim(cov3)[1], sigma=cov3, method="chol")
b4 <- rmvnorm(n = dim(cov4)[1], sigma=cov4, method="chol")

# Define n, the number of iterations
n <- 10   # Chose 10 iterations to cut down on run time

# Call the function for each covariance function
x1 <- pcg(coeff=cov1, precond=M1, b=b1[,1], n=n)
x2 <- pcg(coeff=cov2, precond=M2, b=b2[,1], n=n)
x3 <- pcg(coeff=cov3, precond=M3, b=b3[,1], n=n)
x4 <- pcg(coeff=cov4, precond=M4, b=b4[,1], n=n)

# Prepare the plot by calculating the residuals
avg1 <- apply(x1[,2:10], 1, mean)
avg2 <- apply(x2[,2:10], 1, mean)
avg3 <- apply(x3[,2:10], 1, mean)
avg4 <- apply(x4[,2:10], 1, mean)

resid1 <- matrix(NA, nrow=dim(cov1)[1], ncol=n)
resid2 <- matrix(NA, nrow=dim(cov2)[1], ncol=n)
resid3 <- matrix(NA, nrow=dim(cov3)[1], ncol=n)
resid4 <- matrix(NA, nrow=dim(cov4)[1], ncol=n)

resid1[,1] <- rep(0, length(avg1))
resid2[,1] <- rep(0, length(avg2))
resid3[,1] <- rep(0, length(avg3))
resid4[,1] <- rep(0, length(avg4))

for (i in 2:n) {
  resid1[,i] <- x1[,i] - avg1
  resid2[,i] <- x2[,i] - avg2
  resid3[,i] <- x3[,i] - avg3
  resid4[,i] <- x4[,i] - avg4
}

allresid1 <- apply(resid1, 2, mean)
allresid2 <- apply(resid2, 2, mean)
allresid3 <- apply(resid3, 2, mean)
allresid4 <- apply(resid4, 2, mean)

# Plot the averaged residuals
# Averaging the residuals simplifies the plot while still revealing trends
plot.new()
postscript(file="hw5p1.eps", horizontal=FALSE)
plot(1:10, allresid1, main="Residuals Averaged over All Iterations",
     xlab = "Iterations", ylab = "Average Residuals")
points(allresid2, col="red", pch=2)
points(allresid3, col="darkgreen", pch=3)
points(allresid4, col="blue", pch=4)
legend("bottomleft", legend=c("Sigma 1", "Sigma 2", "Sigma 3", "Sigma 4"),
       pch = c(1,2,3,4), col=c("black", "red", "darkgreen", "blue"), cex=.7)
dev.off()

## PROBLEM 2

# Adjust function from problem 1 so it runs more quickly
pcg2 <- function(coeff, precond, b, n) {
  A <- coeff
  M <- precond
  
  x <- matrix(NA, nrow=dim(A)[1], ncol=n)
  x[,1] <- rep(0, dim(A)[1])                     # set initial value to 0
  r <- matrix(NA, nrow=dim(A)[1], ncol=n)
  r[,1] <- b - A %*% x[,1]
  z <- matrix(NA, nrow=dim(A)[1], ncol=n)
  z[,1] <- solve(M) %*% r[,1]
  p <- matrix(NA, nrow=dim(A)[1], ncol=n)
  p[,1] <- z[,1]
  a <- vector(length=n)
  
  for (k in 2:n) { 
    Ap_kminus1 <- A %*% p[,k-1]
    r[,k-1] <- b - A %*% x[,k-1]
    a[k-1] <- (t(r[,k-1]) %*% z[,k-1]) / (t(p[,k-1]) %*% Ap_kminus1)
    x[,k] <- x[,k-1] + a[k-1] * p[,k-1]
    r[,k] <- r[,k-1] - a[k-1] * Ap_kminus1
    z[,k] <- M %*% r[,k]  # Since M is identity because A is cov. function, we don't have to compute inverse
    p[,k] <- z[,k] + ((t(r[,k]) %*% z[,k])/(t(r[,k-1]) %*% z[,k-1])) %*% p[,k-1]
  }
  
  return(x)
}
