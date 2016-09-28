### comments from TA: good job!
#                     But you need to include checking with built-in function

### Note: comments for specific question may be found, 
###       especially if anything wrong 

### Jessica Miller ###
###  ST 758 HW 3   ###
### Sept. 13, 2016  ###

## PROBLEM 2

# Collect 1000x1000 matrix from HW 1
vec <- (1:1000)/1000
mat <- exp( - sqrt( outer( vec, -vec, "+" )^2 ) ) 

# Define matrix A for use in inverse Cholesky factor
A <- mat
n <- dim(A)[1]

# Define inverse Cholesky factor
Linv <- matrix(0, nrow=n, ncol=n)
Linv[1,1] <- 1/sqrt(A[1,1])   

# Begin inductive process
for (k in 2:n) {
  a_k <- A[k, 1:(k-1)]
  L_minuskminus1 <- Linv[1:(k-1), 1:(k-1)]
  l_k <- L_minuskminus1 %*% a_k
  L_kk <- sqrt(-t(l_k)%*%l_k + A[k,k])
  Linv[k,k] <- 1/L_kk
  Linv[k, 1:(k-1)] <- (-t(l_k) %*% L_minuskminus1)/as.vector(L_kk)
}