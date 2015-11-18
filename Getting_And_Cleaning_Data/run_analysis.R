library(reshape2)

fileName = "dataset.zip"


# Get file, download to working directory and unzip
fetchData <- function () {
        dataUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(dataUrl, fileName, method="curl")
        unzip(fileName)
}

# Ingest features, filter only mean or stddev
getFeatures <- function() {
    features <- read.table("./UCI HAR Dataset/features.txt")
    features[,2] <- as.character(features[,2])
    grep("mean|std", features[,2])
}

getActivityLabels <- function() {
    # Ingest activity labels
    activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
    activityLabels[,2] <- as.character(activityLabels[,2])
    activityLabels
}


getFullData <- function() {
    # Load test and training data
    trainData <- read.table("UCI HAR Dataset/train/X_train.txt")[desiredFeatures]
    testData <- read.table("UCI HAR Dataset/test/X_test.txt")[desiredFeatures]
    names(trainData) <- desiredFeatures;
    names(testData) <- desiredFeatures;
    
    trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
    testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
    names(trainSubjects) <- "Subject";
    names(testSubjects) <- "Subject";
    
    trainActivities <- read.table("UCI HAR Dataset/train/y_train.txt")
    testActivities <- read.table("UCI HAR Dataset/test/y_test.txt")
    names(trainActivities) <- "Activity"
    names(testActivities) <- "Activity"
    
    trainComplete <- cbind(trainSubjects, trainActivities, trainData)
    testComplete <- cbind(testSubjects, testActivities, testData)
    
    # Merge test and train data
    rbind(trainComplete, testComplete)
}

if (!file.exists(filename))
    fetchData()

activityLabels <- getActivityLabels();
fullData <- getFullData();

# Setup factors for melt... melt comes from reshape package
fullData$Subject <- as.factor(fullData$Subject)
fullData$Activity <- factor(fullData$Activity, levels = activityLabels[,1], labels = activityLabels[,2])

fullDataMelt <- melt(fullData, id = c("Subject", "Activity"))
fullDataMean <- dcast(fullDataMelt, Subject + Activity ~ variable, mean)

# Output to text file.  Output table to verify readability
write.table(fullDataMean, "tidy.txt", row.names = FALSE)

