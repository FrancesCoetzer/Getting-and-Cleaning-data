# This script will do the following:
#    1) Merges the training and the test sets to create one data set.
#    2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#    3) Uses descriptive activity names to name the activities in the data set
#    4) Appropriately labels the data set with descriptive variable names. 
#    5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#load libraries
library(tidyr)
library(dplyr)
library(reshape2)

# First read in the data:

activity_labels<-read.table(".\\activity_labels.txt", header=FALSE)
features<-read.table(".\\features.txt", header=FALSE)



X_train<-read.table(".\\train\\X_train.txt", header=F)
y_train<-read.table(".\\train\\y_train.txt",, header=F)
subject_train<-read.table(".\\train\\subject_train.txt", header=F)

X_test<-read.table(".\\test\\X_test.txt", header=F)
y_test<-read.table(".\\test\\y_test.txt", header=F)
subject_test<-read.table(".\\test\\subject_test.txt",header=F)

#Assign column names to the data
names(activity_labels)<-c("id.activity", "type.activity")
names(y_train)<-"id.activity"
names(y_test)<-"id.activity"
names(subject_train)<-"id.subject"
names(subject_test)<-"id.subject"
names(X_train)<-features[,2]
names(X_test)<-features[,2]

#Create the training and test data sets by combining all the data relevant to training and test
train.data<-cbind(subject_train, y_train, X_train)
test.data<-cbind(subject_test, y_test, X_test)

#Create the merged data set
Big.data<-rbind(train.data, test.data)

#This finishes step 1

#Step 2:
#Create data set with just mean and standard deviation 
unique.names<-unique(names(Big.data))
use.data<-Big.data[,unique.names]
mean.std.data<-select(use.data, contains("-mean"), contains("-std"))
mean.std.data<-cbind(Big.data[,1:2], mean.std.data)


#Step 3
Step3.data<- merge(mean.std.data, activity_labels, by="id.activity", all.x=TRUE)

#Step 4
names(Step3.data[,3:81])<-gsub("^t", "time", names(Step3.data[,3:81]))
names(Step3.data[,3:81])<-gsub("^f", "frequency", names(Step3.data[,3:81]))
names(Step3.data[,3:81])<-gsub("Acc", "Accelerometer", names(Step3.data[,3:81]))
names(Step3.data[,3:81])<-gsub("Gyro", "Gyroscope", names(Step3.data[,3:81]))
names(Step3.data[,3:81])<-gsub("Mag", "Magnitude", names(Step3.data[,3:81]))
names(Step3.data[,3:81])<-gsub("BodyBody", "Body", names(Step3.data[,3:81]))
Step4.data<-Step3.data
Step4.data[,1]<-Step4.data[,82]
Step4.data<-Step4.data[,-82]

#Step 5
variable<-names(Step4.data[,3:81])
Final.data<-dcast(Step3.data, id.subject+type.activity~variable,mean)

Final.data<-group_by(Step3.data, type.activity, id.subject)
Final.data<-arrange(Final.data,type.activity,id.subject)

write.table(Final.data, "tidy.txt", row.names = FALSE, quote = FALSE)


summarize(Final.data)



