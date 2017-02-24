# clearing global environment for a clean slate
rm(list = ls())
packages <- c("data.table", "reshape2", "dplyr", "plyr")
sapply(packages, require, character.only = T, quietly = T)

getTidyData <- function(directory) {
    setwd(directory)
    
    features <- fread('features.txt')
    names(features) <- c('rowIndex', 'featureName')
    # cleaning and subsetting colNames that contain mean and std
    featuresIndices <- grep(".*[Mm]ean.*|.*std.*", features$featureName)
    featuresWanted <- features[featuresIndices]
    featuresWanted$featureName = gsub('-mean', 'Mean', featuresWanted$featureName)
    featuresWanted$featureName = gsub('-std', 'Std', featuresWanted$featureName)
    featuresWanted$featureName <- gsub('[-()]', '', featuresWanted$featureName)
    
    train.path <- 'train'
    xTrain <- fread(file.path(train.path, 'X_train.txt'), select = featuresIndices)
    yTrain <- fread(file.path(train.path, 'Y_train.txt'))
    subTrain <- fread(file.path(train.path, 'subject_train.txt'))
    
    test.path <- 'test'
    xTest <- fread(file.path(test.path, 'X_test.txt'), select = featuresIndices)
    yTest <- fread(file.path(test.path, 'Y_test.txt'))
    subTest <- fread(file.path(test.path, 'subject_test.txt'))
    
    activities <- fread('activity_labels.txt')
    
    # merging/ combining datasets
    subMerged <- rbind(subTrain, subTest)
    xMerged <- rbind(xTrain, xTest)
    yMerged <- rbind(yTrain, yTest)
    
    mergedDf <- cbind(subMerged, yMerged, xMerged)
    colnames(mergedDf) <- c('subject', 'activity_label', featuresWanted$featureName)
    
    # providing proper labels for activities
    mergedDf$activity_label <- as.factor(mergedDf$activity_label)
    levels(mergedDf$activity_label) <- activities$V2
    return(mergedDf)
}

getAggData <- function(df){
    sumDf <- aggregate(df, list(subject = df$subject, activity = df$activity), mean)
    sumDf <- sumDf[,-(3:4)]
    return(sumDf)
}

# a function for writing back the resulting DFs
writeFile <- function(){
    response <- readline(prompt="Would you like to write the files to your current diretory? (y/n): ")
    if (response == 'y'){
        print("Your files have been saved as tidyDF.txt and aggDf.txt")
        write.table(tidyDf, 'tidyDf.txt')
        write.table(aggDf, 'aggDf.txt')
    }
}

dir.path <- "~/dev/datasciencecoursera/datasciencecoursera/Course_3/UCI HAR Dataset/"
tidyDf <- getTidyData(dir.path)
aggDf <- getAggData(tidyDf)
writeFile()

