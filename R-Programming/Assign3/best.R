#####
# Function for Assignment 3 of R Programming course
#
# Passes all submit script tests.
# By jcbrodie
####


best <- function(state, outcome){
    ## Read outcome data
    data <- read.csv("Assignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    states <- unique(data$State) #50 states plus Puerto Rico (PR), Guam (GU), Virgin Islands (VI), and DC
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if (state %in% states){
        # do I need to do anything?
    }
    else {
        stop("invalid state")
    }
    
    if (outcome %in% outcomes){
        if (outcome=="heart attack") {
            outcome.col = 11
        }
        if (outcome=="heart failure") {
            outcome.col = 17
        }
        if (outcome=="pneumonia") {
            outcome.col = 23
        }
    }
    else {
        stop("invalid outcome")
    }

    ## Return hospital name in given state with lowest 30-day death rate
    hospitals.in.state <- data[data$State == state,]
    hospitals.in.state[,outcome.col] <- as.numeric(hospitals.in.state[,outcome.col])
    hospitals.in.state.with.outcome <- hospitals.in.state[!is.na(hospitals.in.state[,outcome.col]),]
    best.outcome <- min(hospitals.in.state.with.outcome[,outcome.col])
    
    top.hospitals <- hospitals.in.state.with.outcome[hospitals.in.state.with.outcome[,outcome.col]==best.outcome,]
    hospital.name <- top.hospitals[1,2]
    
    if (nrow(top.hospitals) > 1){
        hospitals <- top.hospitals[order(top.hospitals[,2]),2]
        hospital.name <- hospitals[1]
    }
    
    hospital.name
}


# best('TX', "heart failure")
