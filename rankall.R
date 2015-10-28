                                        # Coursera course assignment
                                        # https://class.coursera.org/rprog-033/assignment/view?assignment_id=7

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    oocm <- read.csv("outcome-of-care-measures.csv",
                     colClasses = "character")
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}
