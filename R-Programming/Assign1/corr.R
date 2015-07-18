## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
## NOTE: Do not round the result!

corr <- function(directory, threshold = 0) {
    complete.data = complete(directory)
    #only record ids where nobs>threshold
    ids = complete.data[complete.data["nobs"] > threshold, ]$id
    correl = numeric()
    for (i in ids) {
        new.data = read.csv(paste(directory, "/", sprintf("%03d", i), ".csv", sep = ""))
        final.data = new.data[complete.cases(new.data), ]
        correl = c(correl, cor(final.data$sulfate, final.data$nitrate))
    }
    #in R the last thing in the function is returned by default
    correl
}
