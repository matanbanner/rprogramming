complete <- function(directory, id = 1:332) {
    oldwd <- getwd()
    setwd(directory)
    
    df <- data.frame(id=integer(), nobs=integer())
    
    for(i in id) {
        filename <- sprintf("%03i.csv", i)
        x <- read.csv(filename)
        df[nrow(df)+1,] <- c(i, nrow(x[complete.cases(x),]))
    }
    
    setwd(oldwd)
    
    df
    
    
}
