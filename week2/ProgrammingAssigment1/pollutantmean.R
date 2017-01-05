pollutantmean <- function(directory, pollutant, id = 1:332) {
    oldwd <- getwd()
    setwd(directory)
    sum_samples <- 0
    n_samples <- 0
    
    for(i in id) {
        filename <- sprintf("%03i.csv", i)
        x <- read.csv(filename)
        y <- x[, pollutant]
        y <- y[!is.na(y)]
        sum_samples <- sum_samples + sum(y)
        n_samples <- n_samples + length(y)
    }
    
    setwd(oldwd)
    
    sum_samples / n_samples
    
}

