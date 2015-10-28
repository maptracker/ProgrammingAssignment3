# Coursera course assignment
# https://class.coursera.org/rprog-033/assignment/view?assignment_id=7

# Utility routines common to all three sub-assignments:
source("common.R")


best <- function(state, outcome) {
    ## Read outcome data
    oocm <- readOoCM()
    ## Check that state and outcome are valid
    validateState( oocm, state )
    foc <- fullOutcomeColumn( outcome )
    # Get a subset of the data for just that state
    stateSubset <- split(oocm, oocm$State)[[state]]
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    # Use the order method to sort by outcome
    # We recovered the full outcome column name above when we validated
    # the outcome code. Sub-order by hospital name
    indices <- order( stateSubset[[foc]], stateSubset[["Hospital.Name"]] )
    # The first index out of the order is the hospital we want:
    stateSubset[["Hospital.Name"]][ indices[1] ]
}
