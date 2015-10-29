# Coursera course assignment
# https://class.coursera.org/rprog-033/assignment/view?assignment_id=7

# These are functions common to all three sub-assignments

readOoCM <- function() {
    # Build the column classes
    # These are names and addresses in the first columns:
    addressCols <- rep("character", 10)
    # The blocks of data columns, six per measured outcome:
    dataSet <- c("numeric", "character",
                 "numeric", "numeric", "numeric", "character")
    # number of different measurements:
    numBlocks <- 6
    dataCols <- rep(dataSet, 6)
    allCols <- c(addressCols, dataCols)

    oocm <- read.csv("outcome-of-care-measures.csv",
             na.strings = c("Not Available"),
             quote = "\"",
             colClasses = "character" )
    
    # ARRGGG If a CSV file quotes a value, then R *INSISTS* on
    # treating the values as strings, even if you indicate it is numeric:
    #    https://stackoverflow.com/a/6616047

    # So we have to coerce the columns after reading everything as strings
    for (i in seq_len(length(allCols))) {
        ctype = allCols[i];
        if (ctype == "numeric") {
            oocm[, i] <- as.numeric(oocm[, i])
        }
    }
    oocm
}

validateState <- function( oocm, state ) {
    # Directly check intersection of the request with the State column
    overlap <- intersect(oocm$State, state)
    if (length(overlap) == 0) stop("invalid state")
}

#validOutcomes <- c("heart attack", "heart failure", "pneumonia")
validateOutcome <- function( outcome ) {
    if (outcome == "heart attack") {
        return("Heart.Attack")
    } else if (outcome == "heart failure") {
        return("Heart.Failure")
    } else if (outcome == "pneumonia") {
        return("Pneumonia")
    }
    stop("invalid outcome")
    #overlap <- intersect(validOutcomes, outcome)
    #if (length(overlap) == 0) stop("invalid outcome");
}

fullOutcomeColumn <- function( outcome ) {
    valid <- validateOutcome( outcome )
    foc <- paste("Hospital.30.Day.Death..Mortality..Rates.from", valid,
                 sep = '.', collapse = "");
    # message("Using column ", foc)
    foc
}

normalizeIndex <- function( oocmPart, num ) {
    # Number of hospitals in subset:
    numHosp <- nrow(oocmPart)
    rankIndex <- num
    if (num == "best") {
        # First position
        rankIndex <- 1
    } else if (num == "worst") {
        # Last position
        rankIndex <- numHosp
    } else if (num > numHosp |  num < 1) {
        # out of bounds
        return(NA)
    }
    rankIndex
}
