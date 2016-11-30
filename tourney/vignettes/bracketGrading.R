


# create predictions of B brackets
set.seed(031985)
nrounds <- 6
B <- 100
brackets <- replicate(B, simulateTourney(nrounds,0.6) )
save(brackets,file="student1brackets.RData")
brackets <- replicate(B, simulateTourney(nrounds,0.6) )
save(brackets,file="student2brackets.RData")
brackets <- replicate(B, simulateTourney(nrounds,0.6) )
save(brackets,file="student3brackets.RData")
rm(list=ls())


# read in students' predictions
nrounds <- 6
B <- 100
studentids <- c("student1","student2","student3")
nstudents <- length(studentids)
brArray <- array(NA,c(2^nrounds-1,B,nstudents))
for(j in 1:nstudents){
    load( paste0(studentids[j],"brackets.RData") )
    brArray[,,j] <- brackets
}


# simulate T tournaments
nrounds <- 6
T <- 100
set.seed(48)
tourneyMat <- replicate(T, simulateTourney(nrounds,0.6) )

# score all of the students' brackets on all of the tournaments
scoreMat <- array(NA,c(B,T,nstudents))
for(s in 1:nstudents){
    for(t in 1:T){
        for(b in 1:B){
            scoreMat[b,t,s] <- scoreBracket(brArray[,b,s],tourneyMat[,t])
        }
    }
}

maxScores <- apply(scoreMat,c(2,3),max)
maxMaxScores <- apply(maxScores,1,max)

cbind( maxScores, maxScores == maxMaxScores )  # uses recycling of maxMaxScores



