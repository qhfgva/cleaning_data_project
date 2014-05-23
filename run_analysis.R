
## You will be required to submit:

## 1) a tidy data set as described below,
## 2) a link to a Github repository with your script for performing the
##    analysis, and
## 3) a code book that describes the variables, the data, and any
##    transformations or work that you performed to clean up the data called
##    CodeBook.md. You should also include a README.md in the repo with your
##    scripts. This repo explains how all of the scripts work and how they
##    are connected.


setwd("/home/dlee/projects/r/coursera-datascience/cleaning_data/project")
getwd()


## (1) Merges the training and the test sets to create one data set.

# training data
train <- read.csv("UCI HAR Dataset/train/X_train.txt", sep="")
y.train <- read.csv("UCI HAR Dataset/train/y_train.txt", sep="")
column.names <- read.csv("UCI HAR Dataset/features.txt", sep="")
subject.train <- read.csv("UCI HAR Dataset/train/subject_train.txt")

names(train) <- make.names(column.names[,2])
train$subject <- subject.train[,1]
train$activity <- y.train[,1]

# test data
test <- read.csv("UCI HAR Dataset/test/X_test.txt", sep="")
y.test <- read.csv("UCI HAR Dataset/test/y_test.txt", sep="")
column.names <- read.csv("UCI HAR Dataset/features.txt", sep="")
subject.test <- read.csv("UCI HAR Dataset/test/subject_test.txt")

names(test) <- make.names(column.names[,2])
test$subject <- subject.test[,1]
test$activity <- y.test[,1]

# merge
full <- rbind(train, test)


## (2) Extracts only the measurements on the mean and standard deviation for each measurement. 

# get only columns that measure mean/std
keepers <- grepl("mean|std", names(full))
keepers <- c(keepers[1:(length(keepers)-2)], TRUE, TRUE)
mean.and.std <- full[, keepers]


## (3) Uses descriptive activity names to name the activities in the data set

# add a column with the activity name (based on the activity code)
names.list <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
activity.names <- names.list[mean.and.std[,"activity"]]
mean.and.std$activity.names <- activity.names


## (4) Appropriately labels the data set with descriptive activity names. 

# rename columns with appropiate names (clean, easy to access)
# - remove multi "." sequences from the make.names function
# - remove trailing "."
fixed.names <- gsub("\\.{2,3}",".",names(mean.and.std))
fixed.names <- gsub("\\.$","",fixed.names)
names(mean.and.std) <- fixed.names

write.table(final.means, file="tidy_full.txt")


## (5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# melt data to have average of all data by user/subject
library(reshape2)
final.summary = melt(mean.and.std[,!(names(mean.and.std) %in% "activity")], id.var = c("subject", "activity.names"))
final.means = dcast(final.summary, subject + activity.names ~ variable, mean)

write.table(final.means, file="tidy_means.txt")
