best <- function(state, outcome) {
    
    possible_outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    outcome <- possible_outcomes[outcome]
    if (is.na(outcome))
        stop("invalid outcome")
    
    df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    state_hospitals <- df[df$State == state,]
    if (nrow(state_hospitals) == 0)
        stop("invalid state")

    min_val <- min(state_hospitals[, outcome], na.rm = TRUE)
    hospital.rows.numbers = which(state_hospitals[, outcome] == min_val)
    best.hospitals.names = state_hospitals[hospital.rows.numbers,]$Hospital.Name
    sort(best.hospitals.names)
    
    
}
