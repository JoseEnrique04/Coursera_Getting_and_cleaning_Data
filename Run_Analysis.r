# Step1
# 
trainData <- read.table("C:/Archivos de programa/R/data/X_train.txt")
head(trainData)
trainLabel <- read.table("C:/Archivos de programa/R/data/y_train.txt")
table(trainLabel)
trainSubject <- read.table("C:/Archivos de programa/R/data/subject_train.txt")
testData <- read.table("C:/Archivos de programa/R/data/X_test.txt")
testLabel <- read.table("C:/Archivos de programa/R/data/y_test.txt") 
table(testLabel) 
testSubject <- read.table("C:/Archivos de programa/R/data/subject_test.txt")
joinData <- rbind(trainData, testData)
joinLabel <- rbind(trainLabel, testLabel)
joinSubject <- rbind(trainSubject, testSubject)

# Step2. Extracts only the measurements on the mean and standard 
# deviation for each measurement. 
features <- read.table("C:/Archivos de programa/R/data/features.txt")
dim(features)  # 561*2
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) # 66
joinData <- joinData[, meanStdIndices]
dim(joinData) # 10299*66
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData))

# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("C:/Archivos de programa/R/data/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
write.table(cleanedData, "C:/Archivos de programa/R/data/merged_data.txt") # write out the 1st dataset

# Step5. Creates a second, independent tidy data set with the average of 

subjectLen <- length(table(joinSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)
write.table(result, "C:/Archivos de programa/R/data/data_with_means.txt") # write out the 2nd dataset
data <- read.table("C:/Archivos de programa/R/data/data_with_means.txt")
data[1:12, 1:3]
