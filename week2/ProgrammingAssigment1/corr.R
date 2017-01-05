corr <- function(directory, threshold = 0) {
    oldwd <- getwd()
    setwd(directory)
    
    y <- numeric(0)
    
    for(i in 1:332) {
        filename <- sprintf("%03i.csv", i)
        x <- read.csv(filename)
        x <- x[complete.cases(x),]
        if (nrow(x) > threshold) {
            y[length(y)+1] <- cor(x[, "sulfate"], x[,"nitrate"])
        }
        
    }
    
    setwd(oldwd)
    
    y
    
}
