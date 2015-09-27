# Set working directory and load required libraries.

setwd("~/Documents/Coursera/Getting_Cleaning_Data/Course_Project/UCI HAR Dataset")
library(dplyr)


# 1.- Merges the training and the test sets to create one data set.

# Read all the tables needed for this assignment.

Features <- read.table(file = "features.txt", header = F)
Activity <- read.table(file = "activity_labels.txt", header = F)
SubTest <- read.table(file = "test/subject_test.txt", header = F)
XTest <- read.table(file = "test/X_test.txt", header = F)
YTest <- read.table(file = "test/y_test.txt", header = F)
SubTrain <- read.table(file = "train/subject_train.txt", header = F)
XTrain <- read.table(file = "train/X_train.txt", header = F)
YTrain <- read.table(file = "train/y_train.txt", header = F)

# Assign column labels to files

colnames(Activity) <- c("activityId", "activityType")
colnames(SubTest) <- "subjectId"
colnames(XTest) <- Features[, 2]
colnames(YTest) <- "activityId"
colnames(SubTrain) <- "subjectId"
colnames(XTrain) <- Features[, 2]
colnames(YTrain) <- "activityId"

# Merge files from the test and train sets.

Test <- cbind(YTest, SubTest, XTest)
Train <- cbind(YTrain, SubTrain, XTrain)

# Combine test and train datasets into the final set to use

FinalSet <- rbind(Test, Train)

# 2.- Extracts only the measurements on the mean and standard deviation for each measurement.
# Use the colnames to search all the columns containing mean and standard deviation values
# plus the activityId and subjectId

colNames <- colnames(FinalSet)

colstokeep <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames)
 & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

FinalData <- FinalSet[colstokeep == T]

# 3.- Uses descriptive activity names to name the activities in the data set
# Merge the FinalData together with the activity_labels.txt file, to include
# the activity descriptive names

FinalData <- merge(FinalData, Activity, by = "activityId")

# Update the colNames vector to include the new column name after merging
colNames <- colnames(FinalData)

# 4. Appropriately label the data set with descriptive activity names.

# Clarify the variable names
for (i in 1:length(colNames)) 
{
        colNames[i] <- gsub("\\()","",colNames[i])
        colNames[i] <- gsub("-std$","StdDev",colNames[i])
        colNames[i] <- gsub("-mean","Mean",colNames[i])
        colNames[i] <- gsub("^(t)","time",colNames[i])
        colNames[i] <- gsub("^(f)","freq",colNames[i])
        colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Reassign the new descriptive column names to the FinalData set

colnames(FinalData) <- colNames

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
# Create a new table without the activityType column

finalDataNoActivityType <- FinalData[,names(FinalData) != 'activityType']

# Summarize the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject

tidyData <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

# Merge the tidyData with activityType to include descriptive acitvity names

tidyData <- merge(tidyData,activityType,by='activityId',all.x=TRUE)

# Export the tidyData set 

write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')
