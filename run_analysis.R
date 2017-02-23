rm(list = ls())
packages <- c("data.table", "reshape2", "dplyr", "plyr")
sapply(packages, require, character.only = T, quietly = T)

getTidyData <- function(directory) {
    setwd(directory)
    
    train.path <- 'train'
    xTrain <- fread(file.path(train.path, 'X_train.txt'))
    yTrain <- fread(file.path(train.path, 'Y_train.txt'))
    subTrain <- fread(file.path(train.path, 'subject_train.txt'))
    
    test.path <- 'test'
    xTest <- fread(file.path(test.path, 'X_test.txt'))
    yTest <- fread(file.path(test.path, 'Y_test.txt'))
    subTest <- fread(file.path(test.path, 'subject_test.txt'))
    
    features <- fread('features.txt')
    activities <- fread('activity_labels.txt')
    
    subMerged <- rbind(subTrain, subTest)
    xMerged <- rbind(xTrain, xTest)
    yMerged <- rbind(yTrain, yTest)
    
    names(xMerged) <- features$V2
    names(yMerged) <- 'activity_label'
    names(subMerged) <- 'subject'
    
    mergedDf <- cbind(subMerged, yMerged, xMerged)
    names(mergedDf) <- tolower(names(mergedDf))
    mergedDf$activity_label <- as.factor(mergedDf$activity_label)
    levels(mergedDf$activity_label) <- activities$V2
    return(mergedDf)
}

getMeanAndSd <- function(df){
    means <- unlist(lapply(df[,-(1:2)], mean))
    sdevs <- unlist(lapply(df[,-(1:2)], sd))
    
    sumStats <- list(means, sdevs)
    return(sumStats)
}

getAggData <- function(df){
    sumDf <- aggregate(df, list(subject = df$subject, activity = df$activity), mean)
    sumDf <- sumDf[,-(3:4)]
    return(sumDf)
}

dir.path <- "~/dev/datasciencecoursera/datasciencecoursera/Course_3/UCI HAR Dataset/"
tidyDf <- getTidyData(dir.path)
sumStats <- getMeanAndSd(tidyDf)
aggDf <- getAggData(tidyDf)
write.table(df, 'tidyDf.txt')
