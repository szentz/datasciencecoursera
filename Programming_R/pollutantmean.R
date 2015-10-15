setwd("~/datasciencecoursera/Programming_R")

pollutantmean <- function(directory, pollutant, id = 1:332) {
      
    # Only read in data we need, so figure out files vector
    files <- sprintf("%s/%03d.csv", directory, id)
    
    # rbind the files together
    inData <-  do.call("rbind", lapply(files, function(x) read.csv(x)))
    
    # Calculate mean for given pollutant, remove NA... round to 3 digits to match examples
    round(mean(inData[[pollutant]], na.rm=TRUE),digits = 3)
      
}

# Mimic the examples given
# Expected: 4.064
#pollutantmean("specdata", "sulfate", 1:10)
#[1] 4.064

# Expected: 1.706
#pollutantmean("specdata", "nitrate", 70:72)
#[1] 1.706

# Expected: 1.281
#pollutantmean("specdata", "nitrate", 23)
#[1] 1.281