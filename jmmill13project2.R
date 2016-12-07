### Jessica Miller ###
### Nov. 10, 2016 ###
### ST 758 Proj. 2 ###

#setwd("F:/ST 758/jmmill13ST758")
#library(fields)
#library(geoR)
#library(mvtnorm)

loc1024 <- read.csv("locations1024.csv", header=T)
loc2025 <- read.csv("locations2025.csv", header=T)
loc4096 <- read.csv("locations4096.csv", header=T)

## Plot locations on grid

plot(loc1024[,1],loc1024[,2])
plot(loc2025[,1],loc2025[,2])
plot(loc4096[,1],loc4096[,2])

# The locations are very similar to each other. Create grid.

newlocs <- function(locs.x, locs.y, sep) {
    x1 <- seq(min(locs.x), max(locs.x), by=sep)
    x2 <- seq(min(locs.y), max(locs.y), by=sep)
    s   <- expand.grid(x1, x2)
}

new1024 <- newlocs(loc1024[,1], loc1024[,2], .5)
new2025 <- newlocs(loc2025[,1], loc2025[,2], .5)
new4096 <- newlocs(loc4096[,1], loc4096[,2], .5)


plot(loc1024[,1],loc1024[,2])
points(new1024[,1], new1024[,2], col="red")
plot(loc2025[,1],loc2025[,2])
points(new2025[,1], new2025[,2], col="red")
plot(loc4096[,1],loc4096[,2])
points(new4096[,1], new4096[,2], col="red")

# Calculate covariance matrices for new grid.

dist1024 <- rdist(new1024)
dist2025 <- rdist(new2025)
dist4096 <- rdist(new4096)

covmat <- function(number, dist) {
    if (number == 1) {
        cov <- Matern(dist1024,1)
    }
    if (number == 2) {
        cov <- exp(-dist)
    }
    return(cov)
}

cov1024.mat <- covmat(1,dist1024)
cov2025.mat <- covmat(1,dist2025)
cov4096.mat <- covmat(1,dist4096)
cov1024.exp <- covmat(2,dist1024)
cov2025.exp <- covmat(2,dist2025)
cov4096.exp <- covmat(2,dist4096)

# Calculate Gaussian log likelihood two different ways:
# once using solve() to calculate inverse,
# once using chol2inv() to calculate inverse.
# Return the timing of each.

loglik <- function(cov) {
    y <- rmvnorm(n=nrow(cov), sigma=cov)
    timing1 <- vector()
    timing2 <- vector()
    for (i in 1:nrow(cov)){
        timing1 <- append(timing1, system.time(t(y[,i])%*%solve(cov)%*%y[,i])[3])
        timing2 <- append(timing2, system.time(t(y[,i])%*%chol2inv(chol(cov))%*%y[,i])[3])
    }
    return(list(as.numeric(timing1),as.numeric(timing2)))
}

ll1024.mat <- loglik(cov1024.mat)
ll1024.exp <- loglik(cov1024.exp)
ll2025.mat <- loglik(cov2025.mat)
ll2025.exp <- loglik(cov2025.exp)
ll4096.mat <- loglik(cov4096.mat)
ll4096.exp <- loglik(cov4096.exp)

mean(ll1024.mat[[1]])
mean(ll1024.exp[[1]])
mean(ll1024.mat[[2]])
mean(ll1024.exp[[2]])
mean(ll2025.mat[[1]])
mean(ll2025.exp[[1]])
mean(ll2025.mat[[2]])
mean(ll2025.exp[[2]])
mean(ll4096.mat[[1]])
mean(ll4096.exp[[1]])
mean(ll4096.mat[[2]])
mean(ll4096.exp[[2]])

# Looks like chol2inv() is the faster way.
# Create new grid for higher accuracy.

new2025.2 <- newlocs(loc2025[,1], loc2025[,2], .375)
new4096.2 <- newlocs(loc4096[,1], loc4096[,2], .25)

plot(loc2025[,1],loc2025[,2])
points(new2025.2[,1], new2025.2[,2], col="red")
plot(loc4096[,1],loc4096[,2])
points(new4096.2[,1], new4096.2[,2], col="red")

# Use new grids to investigate the speeds of the log-likelihoods.

dist2025.2 <- rdist(new2025.2)
dist4096.2 <- rdist(new4096.2)

cov2025.mat.2 <- covmat(1,dist2025.2)
cov4096.mat.2 <- covmat(1,dist4096.2)
cov2025.exp.2 <- covmat(2,dist2025.2)
cov4096.exp.2 <- covmat(2,dist4096.2)

ll2025.mat.2 <- loglik(cov2025.mat.2)
ll2025.exp.2 <- loglik(cov2025.exp.2)
ll4096.mat.2 <- loglik(cov4096.mat.2)
ll4096.exp.2 <- loglik(cov4096.exp.2) ## Takes a long time!

mean(ll2025.mat.2[[1]])
mean(ll2025.exp.2[[1]])
mean(ll2025.mat.2[[2]])
mean(ll2025.exp.2[[2]])
mean(ll4096.mat.2[[1]])
mean(ll4096.exp.2[[1]])
mean(ll4096.mat.2[[2]])
mean(ll4096.exp.2[[2]])

