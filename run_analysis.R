library(data.table)
#1-Merges the training and the test sets to create one data set.

# A-Read the activity rows  

# Read test data
activity_test<-fread("./test/y_test.txt", header=F)
# Read  train data
activity_train<-fread("./train/y_train.txt", header=F)
# Add the test and train data together
activitydata<-rbind(activity_test, activity_train)
 
colnames(activitydata)<-  c("activityid") 
 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B-Read the subject rows  

# Read test data
subject_test<-fread("./test/subject_test.txt", header=F)
# Read  train data
subject_train<-fread("./train/subject_train.txt", header=F)
# Add the test and train data together
subjectdata<-rbind(subject_test, subject_train)
 
colnames(subjectdata)<- c("subjectid")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# c-Read the featurelabels values
featurelabels<-fread("features.txt", header=F, sep=" ")
 
colnames(featurelabels)<-c("featureno", "featurename")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#D-Read the fetures data

# Read the x-test  data, fread makes an error and forced the Rstudio to restart
features_test<-read.table("./test/x_test.txt", header=F)
# Read the x-train  data
features_train<-read.table("./train/x_train.txt", header=F)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Add  data together
featuresdata<-rbind(features_test, features_train)

colnames(featuresdata) <- make.names(featurelabels$featurename, unique=TRUE)
#instead of using unique(mergeddata$subjectID) later

alldata<-cbind(activitydata, subjectdata, featuresdata)
 
#====================================================================================================

#2-Extracts only the measurements on the mean and standard deviation for each measurement
library(dplyr)   
 
dfmeanstd <- select(alldata,matches("subjectid|activityid|mean|std"))
#=======================================================================================================
#3. Uses descriptive activity names to name the activities in the data set

#A-# Read activity labels
activitylabels<-fread("activity_labels.txt", header=F, sep=" ")

# Create column names  
colnames(activitylabels)<- c("activityid","activitydata")

#merge activity label to the dataset  on activityid
alldata <- merge(x=dfmeanstd, y=activitylabels, by="activityid")
unique(alldata[,c("activitydata")])
cleandata<-arrange(alldata, subjectid, activityid)
 
#===============================================================================================
#4-Appropriately labels the data set with descriptive variable names. 
colnames <-colnames(cleandata)
colnames <- make.names(colnames, unique=TRUE)

colnamesclean<-gsub("-", " ", colnames) #Replace - with a space
colnamesclean<-gsub("\\.", " ", colnamesclean) #Replace . with a space
colnames(cleandata) <- colnamesclean
#===============================================================================================
#5-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# i use this libraray because it is esear to me since i am from the database background
# Create second, independent tidy data 
tidydata <- tbl_df(cleandata)

# make sure columns are unique
colnames(tidydata) <- make.names(colnames(tidydata) , unique=TRUE)

# Group  by subject and activity
tidydatagroup <-group_by(tidydata, subjectid, activityid)

# Calculate the mean for all  
tidydatamean <- summarise_each(tidydatagroup, funs(mean))

# Reapply the clean column names
colnames(tidydatamean) <- colnamesclean

write.table(tidydatamean, file="tidydata.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=TRUE)