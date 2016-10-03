### comments from TA: good job! 
###               But your baNded cholesky is too slow. Try profvis your function and see what happens
### Note: comments for specific question may be found, 
###       especially if anything wrong 

### Jessica Miller ###
###  ST 758 HW 4   ###
### Sept. 20, 2016  ###

## PROBLEM 2

# Pull the tridiagonal matrix function from homework 2.
tridiag <- function(top, middle, bottom){
  if (length(top)!=length(bottom)) {stop("Error: lengths are not equal!")}
  if (length(top)!=length(middle)-1) {stop("Error: length of top is not length(middle)-1!")}
  if (length(bottom)!=length(middle)-1) {stop("Error: length of bottom is not length(middle)-1!")}                  
  tri.mat <- structure(list(top,middle,bottom), class = "tridiag")
}

# Create a function that returns a banded Cholesky factor.
band_chol <- function(tridiag_mat){          # Insert tridiagonal matrix
  n <- length(tridiag_mat[[2]])
  A <- matrix(0, nrow=n, ncol=n)
  diag(A) <- tridiag_mat[[2]]                # Define A to be tridiagonal
  diag(A[-1,]) <- tridiag_mat[[3]]
  diag(A[,-1]) <- tridiag_mat[[1]]
  L <- matrix(0, nrow=n, ncol=n)
  L[1,1] <- sqrt(A[1,1])                     # Begin Cholesky factorization
  for (k in 2:n) {
    L[k-1,k] <- A[k-1,k]/L[k-1,k-1]
    L[k,k] <- sqrt(-(L[k-1,k])^2 + A[k,k])
  }
 return(L)                                   # Return the banded Cholesky factor
}

# Choose a parameter for the MA(1) process and create the tridiagonal matrices
sigma.sq <- 4
tridiagmat1 <- tridiag(rep(-sqrt(sigma.sq),99),rep(1+sigma.sq,100),rep(-sqrt(sigma.sq),99))
tridiagmat2 <- tridiag(rep(-sqrt(sigma.sq),999),rep(1+sigma.sq,1000),rep(-sqrt(sigma.sq),999))
tridiagmat3 <- tridiag(rep(-sqrt(sigma.sq),4999),rep(1+sigma.sq,5000),rep(-sqrt(sigma.sq),4999))
tridiagmat4 <- tridiag(rep(-sqrt(sigma.sq),9999),rep(1+sigma.sq,10000),rep(-sqrt(sigma.sq),9999))
tridiagmat5 <- tridiag(rep(-sqrt(sigma.sq),19999),rep(1+sigma.sq,20000),rep(-sqrt(sigma.sq),19999))

# Run each tridiagonal matrix through the banded Cholesky function
# Print out the time elapsed to run for each matrix size
ptm1 <- proc.time()
bandedchol1 <- band_chol(tridiag_mat=tridiagmat1)
print(proc.time() - ptm1)

ptm2 <- proc.time()
bandedchol2 <- band_chol(tridiag_mat=tridiagmat2)
print(proc.time() - ptm2)

ptm3 <- proc.time()
bandedchol3 <- band_chol(tridiag_mat=tridiagmat3)
print(proc.time() - ptm3)

ptm4 <- proc.time()
bandedchol4 <- band_chol(tridiag_mat=tridiagmat4)
print(proc.time() - ptm4)

ptm5 <- proc.time()
bandedchol5 <- band_chol(tridiag_mat=tridiagmat5)
print(proc.time() - ptm5)

## PROBLEM 3

# Define and fill covariance matrices for each given function.
cov1 <- matrix(NA, nrow=50, ncol=50)
cov2 <- matrix(NA, nrow=50, ncol=50)
cov3 <- matrix(NA, nrow=50, ncol=50)
cov4 <- matrix(NA, nrow=50, ncol=50)
for (i in 1:50) {
  for (j in 1:50) {
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
plot.new()
postscript(file="condvar.eps", horizontal=FALSE)
plot(diag(chol(cov3)), ylab="Conditional Variance", xlab="k", main="Conditional Variance of AR(p)", type="l")
lines(diag(chol(cov2)), lty=2)
lines(diag(chol(cov1)), lty=3)
lines(diag(chol(cov4)), lty=4)
text(40, 0.8,"Cov. 4", cex=.7)
text(40, 0.41, "Cov. 1", cex=.7)
text(40, 0.21, "Cov. 2", cex=.7)
text(40, 0.05, "Cov. 3", cex=.7)
dev.off()
# It is apparent (according to condvar.eps) that covariance
# function #2 converges the most quickly.