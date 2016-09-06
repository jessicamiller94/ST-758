### Jessica Miller ###
###  ST 758 HW 2   ###
### Sept. 6, 2016  ###

## PROBLEM 1

# Define function that creates an S3 tridiagonal matrix object of tridiag class.
# Include errors that stop the function if the inputs do not follow the tridiagonal rules.
tridiag <- function(top, middle, bottom){
  if (length(top)!=length(bottom)) {stop("Error: lengths are not equal!")}
  if (length(top)!=length(middle)-1) {stop("Error: length of top is not length(middle)-1!")}
  if (length(bottom)!=length(middle)-1) {stop("Error: length of bottom is not length(middle)-1!")}                  
  tri.mat <- structure(list(top,middle,bottom), class = "tridiag")
}
# Try an example to make sure function worked on small matrix.
tri <- tridiag(1:10,1:11,1:10)
# Check that the function produces an S3 object.
pryr::otype(tri)

## PROBLEM 2

# Define as.matrix method specifically for a tridiagonal matrix.
# Convert the object from previous function into a matrix class.
as.matrix.tridiag <- function(x) {
  tri.mat <- matrix(data=0, nrow=length(x[[2]]), ncol=length(x[[2]]))
  diag(tri.mat) <- x[[2]]
  for (i in 1:length(x[[1]])) {
    tri.mat[i, i+1] <- x[[1]][i] }
  for (j in 1:length(x[[3]])) {
    tri.mat[j+1, j] <- x[[3]][j] }
  return(tri.mat)
}
# Try the example function from Problem 1.
as.matrix.tridiag(tri)

## PROBLEM 3

# Define a 1000x1000 tridiagonal matrix
tri2 <- tridiag(1:999,1:1000,1:999)
tri2.matrix <- as.matrix.tridiag(tri2)
# Test the two methods to verify the first representation takes
# less space than the as.matrix method
object.size(tri2)
object.size(tri2.matrix)

## PROBLEM 4

# Create a plot named tridiag.eps for a matrix with
# sin( (1:99)/(2*pi) ) on the lower diagonal
# exp( - ( (1:100)-50 )^2/30^2 ) on the main diagonal
# (1:99) %% 20 on the upper diagonal
plot.tridiag <- function(top, middle, bottom) {
  plot.new()
  postscript(file="tridiag.eps", horizontal=FALSE)
  par(mfrow=c(3,1))     # Set plots on the same figure
  par(mar=c(2,4,1.5,1)) # Alter the margins of each plot
  plot(middle, type="l", main="Diagonals of Tridiagonal Matrix", ylab="Top", xlab="")
  plot(top, type="l", ylab="Middle", xlab="")
  plot(bottom, type="l", ylab="Bottom", xlab="Matrix Index")
  dev.off()
}
# Plug in the above expressions to the plot function.
plot.tridiag((1:99) %% 20,exp( - ( (1:100)-50 )^2/30^2 ),sin( (1:99)/(2*pi) ))


# Write a print method for the tridiagonal matrix class
# The print method should print the matrix in the same form as
# matrices are printed, except with blank spaces for zeros
# round(), sprintf(), and cat() may be helpful
# use it to print out this matrix
       [,1]  [,2]  [,3]  [,4]
[1,]   2.65  8.58 
[2,]   0.65  6.54  9.33 
[3,]         2.63 -4.59  8.15
[4,]               6.06  5.24

## PROBLEM 5

# Create a print method for tridiagonal matrices.
# I used the functions that I defined from Problems 1 and 2
# to be more efficient.
print.tridiag <- function(top, middle, bottom) {
  tridiag <- function(top, middle, bottom){
    if (length(top)!=length(bottom)) {stop("Error: lengths are not equal!")}
    if (length(top)!=length(middle)-1) {stop("Error: length of top is not length(middle)-1!")}
    if (length(bottom)!=length(middle)-1) {stop("Error: length of bottom is not length(middle)-1!")}                  
    tri.mat <- structure(list(top,middle,bottom), class = "tridiag")
  }
  as.matrix.tridiag <- function(x) {
    tri.mat <- matrix(data=NA, nrow=length(x[[2]]), ncol=length(x[[2]]))
    diag(tri.mat) <- x[[2]]
    for (i in 1:length(x[[1]])) { tri.mat[i, i+1] <- x[[1]][i] }
    for (j in 1:length(x[[3]])) { tri.mat[j+1, j] <- x[[3]][j] }
    return(tri.mat)
  }
  tri.1 <- tridiag(top, middle, bottom)
  tri.2 <- as.matrix.tridiag(tri.1)
  print(tri.2, na.print="")
}
# Define the diagonals going into the function
upper <- c(8.58, 9.33, 8.15)
main <- c(2.65, 6.54, -4.59, 5.24)
lower <- c(0.65, 2.63, 6.06)
# Call the tridiagonal print method
final.mat <- print.tridiag(upper, main, lower)
