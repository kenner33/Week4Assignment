best<- function(state, outcome){
    
    ## Read outcome data
    
    outcome1 <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character")
    
    ## Check that state and outcome are valid
    ## If state invalid, throw an error via stop function with "invalid state"
    ## If outcome invalid, throw an error via stop function with "invalid outcome"
    
    
    if(!any(state == outcome1$State)){
        stop("invalid state")}
    else if((outcome %in% c("heart attack", "heart failure",
                            "pneumonia")) == FALSE) {
        stop(print("invalid outcome"))
    }
    outcome2 <- subset(outcome1, State == state)
    
    ## Return hospital name in that state with lowest 30-day death rate
    
    if (outcome == "heart attack") {
        colnum <- 11
    }
    else if (outcome == "heart failure") {
        colnum <- 17
    }
    else {
        colnum <- 23
    }
    min_row <- which(as.numeric(outcome2[ ,colnum]) == 
                         min(as.numeric(outcome2[ ,colnum]), na.rm = TRUE))
    hospitals <- outcome2[min_row,2]
    hospitals <- sort(hospitals)
    return(hospitals[1])
    
}