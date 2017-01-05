rankhospital2 <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    possible_outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    outcome <- possible_outcomes[outcome]
    if (is.na(outcome))
        stop("invalid outcome")
    
    ## Check that state and outcome are valid
    df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    df <- df[,c(2, 7, outcome)]
    df <- df[complete.cases(df), ]
    names(df) <- c("hospital", "state", "outcome")
    
    state_hospitals <- df[df$state == state,]
    if (nrow(state_hospitals) == 0)
        stop("invalid state")
    
    
    ## Return hospital name in that state with the given rank 30-day death rate
    ranked_hospitals <- state_hospitals[order(state_hospitals$outcome, state_hospitals$hospital),]
    
    
    if (num == "best")
        num <- 1
    else if (num == "worst")
        num <- nrow(ranked_hospitals)
    
    ranked_hospitals[num,1]
    
}