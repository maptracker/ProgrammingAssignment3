# Coursera course assignment
# https://class.coursera.org/rprog-033/assignment/view?assignment_id=7

# Clear the workspace
rm(list=ls())
# Utility routines common to all three sub-assignments:
source("common.R")

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    oocm <- readOoCM()
    ## Check that state and outcome are valid
    foc <- fullOutcomeColumn( outcome )
    working <- oocm[ , c(foc, "Hospital.Name", "State")]
    # Long column names are irritating for debugging:
    names(working) <- c("outcome", "hospital", "state")

    
    byState   <- split(working, working$state)
    allStates <- names(byState)
    numStates <- length(allStates)
    initVec   <- vector("character", numStates)
    rv <- data.frame( hospital = initVec, state = initVec,
                     stringsAsFactors = FALSE, row.names = allStates )
    ## For each state, find the hospital of the given rank
    for (i in seq_len(numStates)) {
        state = allStates[i]
        rv$state[i] <- state
        stateFrame <- byState[[state]]
        stateSubset <- stateFrame[ !is.na(stateFrame[1]), 1:2 ]
        
        rankIndex <- normalizeIndex( stateSubset, num )
        rv$hospital[i] <- if (is.na(rankIndex)) {
            NA
        } else {
            indices <- order( stateSubset[[1]], stateSubset[[2]] )
            stateSubset[[2]][ indices[rankIndex] ]
        }
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    rv
}
