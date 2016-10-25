### Jessica Miller ###
###  ST 758 HW 7   ###
### Oct. 11, 2016  ###

## PROBLEM 1
weib <- function(sample, lambda, theta){
  n <- length(sample)
  for (i in 1:n){
    likeli <- (theta/lambda)^n * (sample[i]/lambda)^(theta-1)*exp(-(sum(sample[1:n])/lambda)^theta)
  }
 
}