setwd("~/datasciencecoursera/Programming_R")

complete <- function(directory, id = 1:332) {
    
    # Define a sub function that read in csv and find id and sum
    sumCompletes <- function(x) {
        data <- read.csv(x)
        data.frame(id = data[["ID"]][1], nobs = sum(complete.cases(data)))
    }

    # Only read in data we need, so figure out files vector
    files <- sprintf("%s/%03d.csv", directory, id)
    
    # rbind the return from subCompletes for each file
    do.call("rbind", lapply(files, sumCompletes))
}
    
#Expected: 
#   id nobs
# 1  1  117
#complete("specdata", 1)
#  id nobs
#1  1  117

#Expected: 
#   id nobs
# 1  2 1041
# 2  4  474
# 3  8  192
# 4 10  148
# 5 12   96
#complete("specdata", c(2, 4, 8, 10, 12))
#  id nobs
#1  2 1041
#2  4  474
#3  8  192
#4 10  148
#5 12   96

#Expected:
#   id nobs
# 1 30  932
# 2 29  711
# 3 28  475
# 4 27  338
# 5 26  586
# 6 25  463
#complete("specdata", 30:25)
#  id nobs
#1 30  932
#2 29  711
#3 28  475
#4 27  338
#5 26  586
#6 25  463

#Expected:
#   id nobs
# 1  3  243
#complete("specdata", 3)
#  id nobs
#1  3  243