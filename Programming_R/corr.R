setwd("~/datasciencecoursera/Programming_R")

corr <- function(directory, threshold = 0) {
    
    # Init out vector
    outVector <- NULL
    
    # Loop through all files, compute complete sum, execute cor when sum > threshold
    for (file in list.files(directory, full = TRUE)) {
        data <- read.csv(file);
        
        completeCases = complete.cases(data);
        if (sum(completeCases) > threshold) {
            outVector <- c(outVector,cor(data[["sulfate"]][completeCases], 
                                    data[["nitrate"]][completeCases]))
        }
    }
    
    outVector
}

# Expected:
# [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
# source("corr.R")
# cr <- corr("specdata", 150)
# head(cr)
# [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21060 -0.04999  0.09463  0.12530  0.26840  0.76310

