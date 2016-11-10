# comment: 1. I can't ru  your code in a reasonable time and R session keeps breaking down. -1
#          2. you should summarize your result instead of throwing a bunch of profvis for me to check and compare. -1
# there are some ovious inefficient thing in your code:
# 1. cov matrix are toeplitz matrix, you never construct them by computing all elements. 
#    Instead you compute a row or column and construct using it and toeplitz() or similar functions
# 2. you don't need matrix multiplication in DL algorithm, the P matrix is just permutation and you can avoid using p by reverting r vector



### Jessica Miller ###
###  ST 758 HW 6   ###
### Oct. 4, 2016  ###

library(mvtnorm)  # Required for linear system
library(lava)     # Required for permutation matrix
library(profvis)  # Required for timing functions

# Calculate the covariance matrices from the previous homework assignments
cov1 <- matrix(NA, nrow=4000, ncol=4000)
cov2 <- matrix(NA, nrow=4000, ncol=4000)
cov3 <- matrix(NA, nrow=4000, ncol=4000)
cov4 <- matrix(NA, nrow=4000, ncol=4000)

for (i in 1:4000) {
  for (j in 1:4000) {
    h <- i-j
    cov1[i,j] <- (abs(h)/4)*besselK(abs(h)/4, nu=1)
    cov2[i,j] <- (1+(abs(h)/4))*exp(-abs(h)/4)
    cov3[i,j] <- exp(-((h/2)^2))
    if (h == 0) { cov4[i,j] <- 1 }
    if (h > 0) { cov4[i,j] <- (h-1+0.4)*cov4[i-1,j]/(h-0.4) }
  }
}
diag(cov1) <- 1             # Set the variance equal to 1
cov4 <- toeplitz(cov4[,1])  # Fix the 4th covariance matrix

# Define Durbin-Levinson algorithm
durblev <- function(coeff) {
  A <- coeff 
  
  # Initialize vectors and constants of system
  y <- rmvnorm(n = dim(A)[1], sigma=A, method="chol")
  y <- y[,1]
  
  n <- dim(A)[1]
  r <- vector(length=n)
  r[1] <- .95
  for (i in 2:n) { r[i] <- A[i,1]/A[i,i]}
  
  b <- vector(length=n)
  x <- vector(length=n)
  
  d_k <- 1
  b[1] <- -r[1]
  x[1] <- y[1]
  
  # Loop through the rest to recursively create the solution  
  for (k in 2:n) {
    
    # Create permutation matrix
    P <- matrix(0, nrow=(k-1), ncol=(k-1))
    revdiag(P) <- 1
    
    
    d_k <- 1 + t(r[1:(k-1)]) %*% b[1:(k-1)]
    e_k <- r[k] + t(b[1:(k-1)]) %*% P %*% r[1:(k-1)] 
    f_k <- y[k] - t(r[1:(k-1)]) %*% P %*% x[1:(k-1)] 
    
    b[k] <- e_k/d_k
    x[k] <- f_k/d_k
    b[2:k] <- b[1:(k-1)] + b[k] * P %*% b[1:(k-1)] 
    x[2:k] <- x[1:(k-1)] + x[k] * P %*% b[1:(k-1)] 
    
  }
  
  return(x)
  
}

# Call Durbin-Levinson algorithm and time it using profvis
profvis({
  dl1 <- durblev(coeff=cov1)
  dl2 <- durblev(coeff=cov2)
  dl3 <- durblev(coeff=cov3)
  dl4 <- durblev(coeff=cov4)
})

# Define pre-conditioners from HW 5
M1 <- matrix(0, nrow=dim(cov1)[1], ncol=dim(cov1)[2])
diag(M1) <- diag(cov1)
M2 <- matrix(0, nrow=dim(cov2)[1], ncol=dim(cov2)[2])
diag(M2) <- diag(cov2)
M3 <- matrix(0, nrow=dim(cov3)[1], ncol=dim(cov3)[2])
diag(M3) <- diag(cov3)
M4 <- matrix(0, nrow=dim(cov4)[1], ncol=dim(cov4)[2])
diag(M4) <- diag(cov4)

# Define HW 5's preconditioned conjugate gradient algorithm
pcg2 <- function(coeff, precond, b, n) {

  A <- coeff
  M <- precond
  
  x <- matrix(NA, nrow=dim(A)[1], ncol=n)
  x[,1] <- rep(0, dim(A)[1])            
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

# Define part of the linear system for preconditioned conj. grad.
b1 <- rmvnorm(n = dim(cov1)[1], sigma=cov1, method="chol")
b2 <- rmvnorm(n = dim(cov2)[1], sigma=cov2, method="chol")
b3 <- rmvnorm(n = dim(cov3)[1], sigma=cov3, method="chol")
b4 <- rmvnorm(n = dim(cov4)[1], sigma=cov4, method="chol")

# Call preconditioned conjugate gradient function and time it using profvis
profvis({
  x1 <- pcg2(coeff=cov1, precond=M1, b=b1[,1], n=10)
  x2 <- pcg2(coeff=cov2, precond=M2, b=b2[,1], n=10)
  x3 <- pcg2(coeff=cov3, precond=M3, b=b3[,1], n=10)
  x4 <- pcg2(coeff=cov4, precond=M4, b=b4[,1], n=10)
})

# Calculate the 2-norm between the two solutions
norm1 <- t(dl1-x1)%*%(dl1-x1)
norm2 <- t(dl2-x2)%*%(dl2-x2)
norm3 <- t(dl3-x3)%*%(dl3-x3)
norm4 <- t(dl4-x4)%*%(dl4-x4)

