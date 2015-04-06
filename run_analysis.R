### Install packages that are necessary to run the script

library(dplyr)
library(tidyr)





###Download files with the data that are in your current directory, unzip them if needed

currentdir<-getwd()

if (!file.exists("currentdir")) {
    download.file(
        url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
        method="curl",
        destfile="./data/UCI HAR Dataset.zip")
    unzip("./data/UCI HAR Dataset.zip", exdir="./data")
}


###Load the data
namesfuture  <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)[[2]]
labelsactivty <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names=c("activityNum", "activity"))


xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names=c("activityNum"))
subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names=c("subjectId"))

xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names=c("activityNum"))
subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names=c("subjectId"))


###Merge the training and the test sets to create one data set (step 1)

fulldata <- rbind(xtrain, xtest)
colnames(fulldata) <- namesfuture
rm(list=c("xtrain", "xtest"))

### Extract only the measurements on the mean and standard deviation for each measurement Step 2). 

chosenmeasures <- namesfuture[grepl("mean\\()|std\\()", namesfuture)]
fulldata <- fulldata[,chosenmeasures]
rm(chosenmeasures)



###Use descriptive activity names to name the activities in the data set (STep 3)

activity_dat <- merge(rbind(ytrain, ytest), labelsactivty, by="activityNum", sort=FALSE)[["activity"]]
fulldata["activty"] <- sub("_", " ", tolower(activity_dat))
rm(list=c("ytrain", "ytest", "activity_dat"))


###Add subject id (Step 4)

fulldata["subjectId"] <- rbind(subjecttrain, subjecttest)
rm(list=c("subjecttrain", "subjecttest"))


###create a second, independent tidy data set with the average of each variable for each activity and each subject


cleannames <- function(name) {
    # Better variable names
    name <- sub("^f", "meanfrequency", name)
    name <- sub("^t", "meantime", name)
    name <- sub("Acc", "acceleration", name)
    name <- sub("Mag", "magnitude", name)
    name <- sub("Gyro", "gyroscope", name)
    name <- sub("-mean\\(\\)", "mean", name)
    name <- sub("-std\\(\\)", "standarddev", name)
    name
}


cleandata <- fulldata %>%
    gather(variable, value, -activty, -subjectId) %>%
    mutate(variable=cleannames(variable)) %>%
    group_by(activty, subjectId, variable) %>%
    summarise(value=mean(value)) %>%
    arrange(activty, subjectId, variable)

####Save a file with your output in a current directory

write.table(cleandata, "./assignment.txt", row.names=FALSE)

