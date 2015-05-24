## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
## NOTE: Do not round the result!

## open all files in directory that fall in id range
## Then extract pollutant values

pollutantmean <- function(directory, pollutant, id = 1:332) {
    full.data = numeric()
    for (i in id) {
        new.data = read.csv(paste(directory, "/", sprintf("%03d", i), ".csv", sep = ""))
        full.data = c(full.data, new.data[[pollutant]])
    }
    mean(full.data, na.rm = TRUE)
}

