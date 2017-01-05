rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    possible_outcomes <- c("heart attack" =11, "heart failure"=17, "pneumonia"=23)
    outcome <- possible_outcomes[outcome]
    if (is.na(outcome))
        stop("invalid outcome")
    
    df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    df <- df[,c(2, 7, outcome)]
    df <- df[complete.cases(df), ]
    names(df) <- c("hospital", "state", "outcome")
    df <- df[order(df$state, df$outcome, df$hospital),]
    
    l <- split(df, df$state)
    
    sel <- function(t){
        if (num == "best"){
            num <- 1
        }
        else if (num == "worst"){
            num <- nrow(t)
        }
            
        t[num, "hospital"]
    }
    
    l <- lapply(l, sel)
    
    data.frame(hospital=unlist(l), state=names(l), row.names=names(l))
    
}