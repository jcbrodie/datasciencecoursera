#####
# Function for Assignment 3 of R Programming course
#
# Passes all submit script tests.
# By jcbrodie
####

rankall <- function(outcome, num = "best"){
    
    ## Read outcome data
    data <- read.csv("Assignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    states <- sort(unique(data$State)) #50 states plus Puerto Rico (PR), Guam (GU), Virgin Islands (VI), and DC
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that outcome is valid
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
    
    ## For each state, find the hospital of the given rank
    

    
    all.hospitals.of.rank <- matrix(rep(0, 54) , nrow = 54)
    row.names(all.hospitals.of.rank) <- states
    
    data[,outcome.col] <- as.numeric(data[,outcome.col])
    hospitals.with.outcome <- data[!is.na(data[,outcome.col]),]
    hospitals.ranked.by.state <- hospitals.with.outcome[order(hospitals.with.outcome$State,hospitals.with.outcome[,outcome.col], hospitals.with.outcome[,2]),]
    
    #determine which position we are actually looking for....
    # assign the rank
    
    if (num == "worst"){
        for (s in states){
            single.state <- hospitals.ranked.by.state[hospitals.ranked.by.state$State == s,]
            desired.rank = nrow(single.state)
            all.hospitals.of.rank[s,1] <- single.state[desired.rank, 2]
        }
        
        ## Return a data frame with the hospital names and the 
        ## (abbreviated) state name
        result <- data.frame(all.hospitals.of.rank, states)
        colnames(result) <- c("hospital", "state")

    }
    else {
        if (num == "best"){
            desired.rank = 1
        }
        else if (is.numeric(num)){
            desired.rank = num
        }
        else {
            #not sure if I need this... maybe better just to go with default in this case?
            stop("invalid num")
        }
        
        for (s in states){
            single.state <- hospitals.ranked.by.state[hospitals.ranked.by.state$State == s,]
            all.hospitals.of.rank[s,1] <- single.state[desired.rank, 2]
        }
        
        ## Return a data frame with the hospital names and the 
        ## (abbreviated) state name
        result <- data.frame(all.hospitals.of.rank, states)
        colnames(result) <- c("hospital", "state")
    }
    
    
    
    result
}
