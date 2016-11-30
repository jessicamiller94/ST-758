
# functions for simulating games and tournaments

# Simulate winner of the game based on the two seeds
simulateGame <- function(seeds, parameter){
    # seeds is a 2-vector of positive integers
    # model is Pr(seed[1] beats seed[2]) = g( b1*(sqrt(seeds[2]) - sqrt(seeds[1])) )
    # where g(x) = exp(x)/(1+exp(x))
    b1 <- parameter
    logitpr <- b1*(sqrt(seeds[2]) - sqrt(seeds[1]))
    pr <- exp(logitpr)/(1+exp(logitpr))
    if( runif(1) < pr ){ return(seeds[1])
    } else { return(seeds[2]) }
}


# Simulate a whole tournament

#' @export
simulateTourney <- function(nrounds,parameter){

    # this holds a vector of all the seeds
    # first 2^nrounds entries give the first round.
    # jth game is between teams bracket[2*j-1] and bracket[2*j]
    bracket <- rep(NA,2^(nrounds+1)-1)

    # this part fills in the first round games
    # it's a bit complicated to code, but the
    # general idea is that the best team plays the worst
    # second best plays the second worst, and so on.
    # Additionally, in the next round, the best team plays
    # the weakest first round favorite.
    # best team also plays the weakest second round
    # favorite in the third round
    inds <- 1:2
    if( nrounds == 1){
        bracket[1:(2^nrounds)] <- inds
    } else {
        for(r in 2:nrounds){
            matchups <- rbind( 1:(2^(r-1)), (2^r):((2^(r-1))+1) )
            inds <- c( matchups[,inds] )
        }
    }
    bracket[1:(2^nrounds)] <- inds

    # here is where we simulate the games. first game is between
    # the first two teams.
    teaminds <- 1:2
    # Result of this game goes in bracket[2^nrounds+1]
    gameind <- 2^nrounds+1
    # all results are filled in this way until the tournament is over
    for(j in 1:(2^nrounds-1)){
        bracket[gameind] <- simulateGame(bracket[teaminds],parameter)
        gameind <- gameind + 1
        teaminds <- teaminds + 2
    }

    # at the end, only report the winners of the games
    # i.e. no need to list the first round games, since
    # those are always the same
    bracket <- bracket[(2^nrounds + 1):(2^(nrounds+1)-1)]
    bracket
}
