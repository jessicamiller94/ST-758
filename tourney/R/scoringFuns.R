

# functions to score the brackets


#' Score a predicted bracket against an actual
#' bracket according to some scoring rule
#'
#' @param bracketPredicted a bracket object
#' @param bracketActual a bracket object
#' @param scoringRule a character string giving scoring rule
#' @return bracket score
#' @examples
#' score <- scoreBracket( c(1,3,1), c(1,2,1) )
#' score
#' @export
#' @import matrixStats
#' @useDynLib tourney
#' @importFrom Rcpp sourceCpp
scoreBracket <- function(bracketPredicted,bracketActual,scoringRule = "exponential"){

    stopifnot(length(bracketPredicted)==length(bracketActual))
    nrounds <- round(log2(length(bracketPredicted)+1))
    stopifnot(length(bracketPredicted)== 2^nrounds -1)

    if(scoringRule == "exponential"){
        valueVec <- rep(NA,2^(nrounds)-1)
    } else { error("unrecognized scoring rule") }

    startind <- 1
    for(j in 0:(nrounds-1)){
        endind <- startind + 2^(nrounds-j-1)-1
        valueVec[startind:endind] <- 2^j
        startind <- endind + 1
    }
    score <- sum( (bracketPredicted == bracketActual)*valueVec )
    score
}




# Check to see if a bracket is valid.
# Valid means that the bracket could potentially occur
# this means that the winner of each match is always
# one of the competitors in the match

#' @export
isBracketValid <- function(bracket){

    # number of rounds
    nrounds <- round(log2(length(bracket)+1))
    # throw error if the length of bracket is weird
    stopifnot(length(bracket)== 2^nrounds -1)

    # generate first round games for nrounds
    inds <- 1:2
    if( nrounds == 1){
        bracket[1:(2^nrounds)] <- inds
    } else {
        for(r in 2:nrounds){
            matchups <- rbind( 1:(2^(r-1)), (2^r):((2^(r-1))+1) )
            inds <- c( matchups[,inds] )
        }
    }
    # full bracket with starting games
    bracket <- c(inds,bracket)

    # check individual games to make sure that
    # winner of each game is a competitor from each
    isValid <- TRUE
    teaminds <- 1:2
    gameind <- 2^nrounds+1
    for(j in 1:(2^r)){
        if( !is.element(bracket[gameind],  bracket[teaminds])){
            isValid <- FALSE
        }
        gameind <- gameind + 1
        teaminds <- teaminds + 2
    }
    isValid
}


