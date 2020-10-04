rankall <- function(outcome, num = "best") {
    
    ## check that outcome is valid
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
    df <- df[, c(2, 7, col_num)]             ## subset by the selected outcome
    df[, 3] <- as.numeric(df[, 3])           ## convert death.rate to numeric
    df <- df[!is.na(df[, 3]), ]              ## filter out the NA's
    
    dfres <- data.frame()
    
    ## for each state, find the hospital of the given rank
    all.states <- unique(df$State)
    for (st in all.states) {
        
        df2 <- df[df$State == st, ]                 ## subset by state 
        df2 <- df2[order(df2[, 3], df2[, 1]), ]     ## sort the records

        if (num == "best")
            row_num <- 1
        else if (num == "worst")
            row_num <- nrow(df2)
        else if (num > nrow(df2))
            row_num <- (-1)
        else {
            row_num <- num
        }

        if (row_num > 0) 
            hospital.name <- df2$Hospital.Name[row_num]
        else
            hospital.name <- NA
        
        dfres <- rbind(dfres, list(hospital = hospital.name, state = st))
    }
    dfres <- dfres[order(dfres$state), ]
    dfres
}