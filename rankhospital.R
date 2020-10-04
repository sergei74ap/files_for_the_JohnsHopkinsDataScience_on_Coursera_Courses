rankhospital <- function(state, outcome, num = "best") {
    
    ## check that state and outcome are valid
    if (! sum(state == state.abb)) {
        stop("invalid state")
    }
    if (outcome == "heart attack" )
        col_num <- 11
    else if (outcome == "heart failure")
        col_num <- 17
    else if (outcome == "pneumonia")
        col_num <- 23
    else {
        stop("invalid outcome")
    }
    
    ## read outcome data
    df <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    
    ## return hospital name in that state with the given rank 30-day death rate
    
    ## subset the dataframe by the state and selected outcome
    df <- df[df$State == state, c(2, 7, col_num)]
    
    df[, 3] <- as.numeric(df[, 3])           ## convert death.rate to numeric
    df <- df[!is.na(df[, 3]), ]              ## filter out the NA's
    df <- df[order(df[, 3], df[, 1]), ]      ## sort the records
    
    ## return the value
    if (num == "best")
        row_num <- 1
    else if (num == "worst")
        row_num <- nrow(df)
    else if (num > nrow(df))
        return(NA)
    else {
        row_num <- num
    }
    df$Hospital.Name[row_num]
}