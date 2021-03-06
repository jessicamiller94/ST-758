### Jessica Miller ###
### Nov. 30, 2016 ###
### ST 758 Proj. 3 ###

# Set up seeding
seeding <- c(1,64,32,33,17,48,16,49,9,56,24,41,25,40,8,57,
             4,61,29,36,20,45,13,52,12,53,21,44,28,37,5,60,
             2,63,31,34,18,47,15,50,10,55,23,42,26,39,7,58,
             3,62,30,35,19,46,14,51,11,54,22,43,27,38,6,59)

# Set up 63x1000 prediction matrix
brackets <- matrix(nrow=63, ncol=1000)

# Define win probability function, happily provided by Dr. G
probs <- function(seeds, parameter) {
    b1 <- parameter
    logitpr <- b1*(sqrt(seeds[2]) - sqrt(seeds[1]))
    pr <- exp(logitpr)/(1+exp(logitpr))
    if( runif(1) < pr ){ return(seeds[1])
    } else { return(seeds[2]) }
}

# Define general tournament function
tourney <- function(param) {
    round2 <- vector()
    for (i in seq(1, 63, by=2)) {
        winner <- probs(seeding[i:(i+1)], param)
        round2 <- append(round2, winner)
    }

    round3 <- round2
    for (i in seq(1, 31, by=2)) {
        winner <- probs(round2[i:(i+1)], param)
        round3 <- append(round3, winner)
    }

    round4 <- round3
    for (i in seq(1, 15, by=2)) {
        winner <- probs(round3[(i+32):(i+32+1)], param)
        round4 <- append(round4, winner)
    }

    round5 <- round4
    for (i in seq(1, 7, by=2)) {
        winner <- probs(round4[(i+48):(i+48+1)], param)
        round5 <- append(round5, winner)
    }

    round6 <- round5
    for (i in seq(1, 3, by=2)) {
        winner <- probs(round5[(i+56):(i+56+1)], param)
        round6 <- append(round6, winner)
    }

    # Round 7 is the final winner
    round7 <- round6
    for (i in 1:1) {
        winner <- probs(round6[(i+60):(i+60+1)], param)
        round7 <- append(round7, winner)
    }

    return(round7)
}

# First case (first 250 predictions): general 0.6 probability
for (i in 1:250) {
    brackets[,i] <- tourney(0.6)
}

# Second case (second 250 predictions): general 0.5 probability
for (i in 251:500) {
    brackets[,i] <- tourney(0.5)
}

# Third case (third 250 predictions): probability decreases with each round
tourney3 <- function(param) {
    round2 <- vector()
    for (i in seq(1, 63, by=2)) {
        winner <- probs(seeding[i:(i+1)], param)
        round2 <- append(round2, winner)
    }

    round3 <- round2
    for (i in seq(1, 31, by=2)) {
        winner <- probs(round2[i:(i+1)], param-0.025)
        round3 <- append(round3, winner)
    }

    round4 <- round3
    for (i in seq(1, 15, by=2)) {
        winner <- probs(round3[(i+32):(i+32+1)], param-0.05)
        round4 <- append(round4, winner)
    }

    round5 <- round4
    for (i in seq(1, 7, by=2)) {
        winner <- probs(round4[(i+48):(i+48+1)], param-0.075)
        round5 <- append(round5, winner)
    }

    round6 <- round5
    for (i in seq(1, 3, by=2)) {
        winner <- probs(round5[(i+56):(i+56+1)], param-0.1)
        round6 <- append(round6, winner)
    }

    # Round 7 is the final winner
    round7 <- round6
    for (i in 1:1) {
        winner <- probs(round6[(i+60):(i+60+1)], param-0.1)
        round7 <- append(round7, winner)
    }

    return(round7)
}

for (i in 501:750) {
    brackets[,i] <- tourney3(0.6)
}

# Fourth case: the "small underdog" effect
tourney4 <- function(param) {
    round2 <- vector()
    for (i in seq(1, 63, by=2)) {
        if ((seeding[i+1]-seeding[i]) <= 20) {
            winner <- probs(seeding[i:(i+1)], param+0.15)
            round2 <- append(round2, winner)
        }
        else {
            winner <- probs(seeding[i:(i+1)], param)
            round2 <- append(round2, winner)
        }

    }

    round3 <- round2
    for (i in seq(1, 31, by=2)) {
        if ((round2[i+1]-round2[i]) <= 20) {
            winner <- probs(round2[i:(i+1)], param+0.15)
            round3 <- append(round3, winner)
        }
        else {
            winner <- probs(round2[i:(i+1)], param)
            round3 <- append(round3, winner)
        }
    }

    round4 <- round3
    for (i in seq(1, 15, by=2)) {
        if ((round3[i+32+1]-round3[i+32]) <= 20) {
            winner <- probs(round3[(i+32):(i+32+1)], param+0.15)
            round4 <- append(round4, winner)
        }
        else {
            winner <- probs(round3[(i+32):(i+32+1)], param)
            round4 <- append(round4, winner)
        }
    }

    round5 <- round4
    for (i in seq(1, 7, by=2)) {
        if ((round4[i+48+1]-round4[i+48]) <= 20) {
            winner <- probs(round4[(i+48):(i+48+1)], param+0.15)
            round5 <- append(round5, winner)
        }
        else {
            winner <- probs(round4[(i+48):(i+48+1)], param)
            round5 <- append(round5, winner)
        }
    }

    round6 <- round5
    for (i in seq(1, 3, by=2)) {
        if ((round5[i+56+1]-round5[i+56]) <= 20) {
            winner <- probs(round5[(i+56):(i+56+1)], param+0.15)
            round6 <- append(round6, winner)
        }
        else {
            winner <- probs(round5[(i+56):(i+56+1)], param)
            round6 <- append(round6, winner)
        }
    }

    # Round 7 is the final winner
    round7 <- round6
    for (i in 1:1) {
        if ((seeding[i+60+1]-seeding[i+60]) <= 20) {
            winner <- probs(round6[(i+60):(i+60+1)], param+0.15)
            round7 <- append(round7, winner)
        }
        else {
            winner <- probs(round6[(i+60):(i+60+1)], param)
            round7 <- append(round7, winner)
        }
    }

    return(round7)
}

for (i in 751:1000) {
    brackets[,i] <- tourney4(0.6)
}

# Read brackets matrix out to a file
# setwd("F:/ST 758/jmmill13ST758")
save(brackets, file="jmmill13brackets.RData")
