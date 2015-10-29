# Coursera course assignment
# https://class.coursera.org/rprog-033/assignment/view?assignment_id=7

# Clear the workspace
rm(list=ls())
# Utility routines common to all three sub-assignments:
source("common.R")

best <- function(state, outcome) {
    ## Read outcome data
    oocm <- readOoCM()
    ## Check that state and outcome are valid
    validateState( oocm, state )
    foc <- fullOutcomeColumn( outcome )
    # Get a subset of the data for just that state
    # While we are here, also grab just the two columns we need
    stateSubset <- oocm[ oocm$State == state, c(foc, "Hospital.Name")]
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    # Use the order() method to sort by outcome. Sort by the lowest
    # death rate, then by the hospital name.
    indices <- order( stateSubset[[1]], stateSubset[[2]] )
    # The first index out of the order is the row with the hospital we
    # want:
    stateSubset[[2]][ indices[1] ]
}
