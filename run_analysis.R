#download, unzip and read files
setwd("C:/Tahera/Personal/Coursera/Getting and cleaning data/Project1")

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "dataSet.zip")
dateDownloaded <- date()
unzip("dataSet.zip", files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)

# 1. Merge the training and the test sets to create one data set

library(data.table)
library(plyr)

features     <- read.table('./UCI HAR Dataset/features.txt',header=FALSE)
activityType <- read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE)
subjectTrain <- read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE)
xTrain       <- read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE)
yTrain       <- read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE)

colnames(activityType)  = c('ActivityId','Activity')
colnames(subjectTrain)  = "Subject"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "ActivityId"

dataTraining <- cbind(yTrain,subjectTrain,xTrain)

subjectTest <- read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE)
xTest       <- read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE)
yTest       <- read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE)

colnames(subjectTest) = "Subject"
colnames(xTest)       = features[,2] 
colnames(yTest)       = "ActivityId"

dataTest <- cbind(yTest,subjectTest,xTest)

dataFinal <- rbind(dataTraining, dataTest)

# 2. Extract only the measurements on the mean and standard deviation for each measurement

colNames  = colnames(dataFinal)

dataFinalMeanStd <- dataFinal[, grepl("mean|std|Subject|ActivityId", names(dataFinal))]

# 3. Use descriptive activity names to name the activities in the data set

dataFinalMeanStd = merge(dataFinalMeanStd, activityType, by.x = "ActivityId", by.y = "ActivityId", all = TRUE)

colNames = colnames(dataFinalMeanStd)

# 4. Appropriately labels the data set with descriptive name

names(dataFinalMeanStd) <- gsub("\\()","",names(dataFinalMeanStd))
names(dataFinalMeanStd) <- gsub("-std$","StdDev",names(dataFinalMeanStd))
names(dataFinalMeanStd) <- gsub("-mean","Mean",names(dataFinalMeanStd))
names(dataFinalMeanStd) <- gsub("^(t)","time",names(dataFinalMeanStd))
names(dataFinalMeanStd) <- gsub("^(f)","freq",names(dataFinalMeanStd))
names(dataFinalMeanStd) <- gsub("([Gg]ravity)","Gravity",names(dataFinalMeanStd))
names(dataFinalMeanStd) <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",names(dataFinalMeanStd))
names(dataFinalMeanStd) <- gsub("[Gg]yro","Gyro",names(dataFinalMeanStd))
names(dataFinalMeanStd) <- gsub("AccMag","AccMagnitude", names(dataFinalMeanStd))
names(dataFinalMeanStd) <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",names(dataFinalMeanStd))
names(dataFinalMeanStd) <- gsub("JerkMag","JerkMagnitude",names(dataFinalMeanStd))
names(dataFinalMeanStd) <- gsub("GyroMag","GyroMagnitude",names(dataFinalMeanStd))

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject

dataFinalAvg_by_act_sub <- ddply(dataFinalMeanStd, c("Subject","Activity"), numcolwise(mean))
write.table(dataFinalAvg_by_act_sub, file = "final_data_average.txt", row.name=FALSE)