### comments from TA: minor problems
### Note: comments for specific question may be found, 
###       especially if anything wrong 

# comment(Q1 and Q2): result not printed out because source() don't print variables themselves, 
#          so you should use print() -0.5

### Jessica Miller ###
###  ST 758 HW 1   ###
### Aug. 30, 2016  ###

# Homework 1, Due August 30, 2016 at the beginning of class

# write an R script to solve the problems in this assignment,
# save the R script as <unityid>HW1.R


# IMPORTANT: The homeworks will be graded by 
# me or the TA running source("<unityid>HWj.R")
# and looking at the printed output and any files that are output,
# so make sure you check that sourcing your file produces the output you expect
# We may also look at your code, so if you want to have the best chance
# of getting partial credit, make sure you provide informative comments.

#setwd("F:/ST 758/jmmill13ST758")

## PROBLEM 1

# Initializing vectors of length 1000
log <- vector(mode="logical", length=1000)
int <- vector(mode="integer", length=1000)
doub <- vector(mode="double", length=1000)
comp <- vector(mode="complex", length=1000)
char <- vector(mode="character", length=1000)
# Printing object sizes
object.size(log)
object.size(int)
object.size(doub)
object.size(comp)
object.size(char)

## PROBLEM 2

# The following code creates a vector and a matrix of size 1000 x 1000
vec <- (1:1000)/1000
mat <- exp( - sqrt( outer( vec, -vec, "+" )^2 ) ) 

# Double for loop multiplying mat by vec
result <- vector(length=1000)     # Initialize result vector
ptm <- proc.time()                # Start the clock: double for loop
for (j in 1:1000) {               # Loop over rows of mat
  mult <- vector(length=1000)     # Initialize mult, the vector of mat elements times vec elements
  for (i in 1:1000) {             # Loop over columns of mat
    mult[i] <- mat[j,i]*vec[i]    # Multiply each element of the ith column of mat by the ith element of vec
  }
  result[j] <- sum(mult, na.rm=T) # Sum the elements of mat, store in ith element of result
}
proc.time() - ptm                 # Stop the clock: double for loop

# Matrix multiplication
result2 <- vector(length=1000)    # Initialize second result vector
ptm2 <- proc.time()               # Start the clock: matrix mult
result2 <- mat %*% vec            # Multiply mat by vec, matrix style
proc.time() - ptm2                # Stop the clock: matrix mult

# Sum of squared differences
result3 <- vector(length=1000)    # Initialize vector to contain squared differences
result3 <- (result - result2)^2   # Calculate the squared difference
sum(result3)                      # Very small difference (likely rounding error)

## PROBLEM 3

# comment: figure different at edges. -0.5

# save the result as 'hw1p3.eps'
dev.off()
plot.new()
postscript(file="hw1p3.eps", width=8.4, height=4, horizontal=FALSE)

# Plot cosine function
par(fig=c(.55,.975,.15,.5225), oma=c(0,0,0,0), mar=c(0,0,0,0))            # Set figure location and margins
curve(cos, -.5, 2*pi+.5, xlim=c(0,2*pi), ylim=c(-1-.25,1+.25),            # Plot cosine curve
      main="", ylab="", xlab="", xaxt="n", yaxt="n", cex.axis=.8)
axis(1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), labels =                       # Set custom x-axis
      expression(0, pi/2, pi, 3*pi/2, 2*pi), cex.axis=.8)
axis(2, at = c(-1, 0, 1), labels = c(-1, 0, 1), cex.axis=.8)              # Set custom y-axis
text(pi,0.6, "cosine", cex=.75)                                           # Add "cosine" text

# Plot tangent function piece by piece
par(fig=c(.075,.45,.15,1), oma=c(0,0,0,0), mar=c(0,0,2,0), new=TRUE)      # Set figure location and margins
curve(tan, -pi-.5, -pi/2-.0001, xlim=c(-pi,pi), ylim=c(-4,4),             # Plot first part of tangent curve
      main="tangent", ylab="", xlab="", xaxt="n", cex.axis=.8,
      cex.main=.8)
curve(tan, -pi/2+.0001, pi/2-.0001 , add=TRUE)                            # Plot second part
curve(tan, pi/2+.0001, pi+.5, add=TRUE)                                   # Plot third part
abline(v=-pi/2, lty=2)                                                    # Add asymptotes
abline(v=pi/2, lty=2)
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi), labels =                         # Set custom x-axis
      expression(-pi, -pi/2, 0, pi/2, pi), cex.axis=.8)

# Plot sine function
par(fig=c(.55,.975,.5225,.895), oma=c(0,0,0,0), mar=c(0,0,0,0), new=TRUE) # Set figure location and margins
curve(sin, -.5, 2*pi+.5, xlim=c(0,2*pi), ylim=c(-1-.25,1+.25),            # Plot sine curve
      xaxt="n", yaxt="n", cex.axis=.8, lty=4)
axis(1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), labels=c("","","","",""))      # Set custom x-axis
text(3*pi/2,.5, "sine", cex=0.75)                                         # Add "sine" text
axis(2, at = c(-1, 0, 1), labels = c(-1, 0, 1), cex.axis=.8)              # Set custom y-axis

dev.off()     # Save plot as hw1p3.eps








