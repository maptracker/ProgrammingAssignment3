
# Coursera course assignment
# https://class.coursera.org/rprog-033/assignment/view?assignment_id=7

# Clear the workspace
rm(list=ls())
# Utility routines common to all three sub-assignments:
source("common.R")

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    oocm <- readOoCM()
    ## Check that state and outcome are valid
    validateState( oocm, state )
    foc <- fullOutcomeColumn( outcome )
    # Get a subset of the data for just that state
    # While we are here, also grab just the two columns we need
    stateSubset <- oocm[ oocm$State == state & !is.na(oocm[foc]),
                        c(foc, "Hospital.Name")]
    # Which one are we going to want to pick?
    rankIndex <- normalizeIndex( stateSubset, num )
    if (is.na(rankIndex)) return(rankIndex)
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    # Use the order() method to sort by outcome. Sort by the lowest
    # death rate, then by the hospital name.
    indices <- order( stateSubset[[1]], stateSubset[[2]] )
    
    # The first index out of the order is the row with the hospital we
    # want:
    stateSubset[[2]][ indices[rankIndex] ]
}
