#####
# Function for Assigment 3 of R Programming course
#
# Passes all submit script tests.
# By jcbrodie
####

rankhospital <- function(state, outcome, num = "best"){
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
    
    ## Return hospital name in given state with the given rank
    ## 30-day death rate
    hospitals.in.state <- data[data$State == state,]
    hospitals.in.state[,outcome.col] <- as.numeric(hospitals.in.state[,outcome.col])
    hospitals.in.state.with.outcome <- hospitals.in.state[!is.na(hospitals.in.state[,outcome.col]),]
    
    #determine which position we are actually looking for....
    # assign the rank
    if (num == "best"){
        desired.rank = 1
    }
    else if (num == "worst"){
        desired.rank = nrow(hospitals.in.state.with.outcome)
    }
    else if (is.numeric(num)){
        desired.rank = num
    }
    else {
        #not sure if I need this... maybe better just to go with default in this case?
        stop("invalid num")
    }
    
    ranked.hospitals <- hospitals.in.state.with.outcome[order(hospitals.in.state.with.outcome[,outcome.col], hospitals.in.state.with.outcome[,2]),]
    
    desired.hospital <- ranked.hospitals[desired.rank,]
    
    desired.hospital$Hospital.Name
}


rankhospital("NC", "heart attack", "worst")
