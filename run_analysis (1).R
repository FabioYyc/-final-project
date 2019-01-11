install.packages("stringr")
library(stringr)
install.packages("dplyr")
library(dplyr)
###load test and train data into R
download.file(url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
              destfile = "./assignment", method ="curl")
unzip("./assignment")
##read multiple txt files in the folder, combine them into two lists
testfilelist<-list.files("./assignment/UCI HAR Dataset/test", pattern = ".txt")
testdatalist<-lapply(paste("./assignment/UCI HAR Dataset/test/",testfilelist, sep=""), read.table)

trainfilelist<-list.files(path ="./assignment/UCI HAR Dataset/train",pattern = ".txt")
traindatalist<-lapply(paste("./assignment/UCI HAR Dataset/train/",trainfilelist, sep=""), read.table)
###combine the lists of train and test data sets into 2 datasets(train and test)
testdata <-do.call(cbind, testdatalist)
traindata<-do.call(cbind, traindatalist)
##combine test and train data by rows
merged <-rbind2(testdata, traindata)


###change colnnames of merged data
colnames(merged)[1]<-"subject"
colnames(merged)[563] <- "y"
###reorder the column, make y as the second column
merged<-merged[,c(1,563,2:562)]

###as features are the variable names (columns) used originally in X_train and X_test
###transpose the features dataframe then make it a vector for rename the train and test dataset
features<- read.table("./assignment/UCI HAR Dataset/features.txt", sep = "")
features1<-as.data.frame(t(features))
features_names<-as.vector(unlist(features1[2,]))
colnames(merged)[3:563]<-features_names

###extract measurement with mean and std
names(merged)<-as.character(names(merged))
extracted_data<-merged[grep(pattern ="(-mean()|-std())",names(merged)[3:563])]
matched_measurement<-c(grep("(mean()|std())", names(extracted_data)))
length(matched_measurement)
length(names(extracted_data))
length(extracted_data)==length(matched_measurement)
###the answer is false, hence the extractation was not clean, use matched_measurement vector to
###extract again
matched_measurement_data<-extracted_data[,matched_measurement]
extracted_data<-cbind(extracted_data[,1:2], matched_measurement_data)

###rename the subject and activity columns
colnames(extracted_data)[1:2]<-c("subject","Activity")
activity_labels<-read.table("./assignment/UCI HAR Dataset/activity_labels.txt")
###creating look up vector, use the vector to change the column
activity_labels_vector<-as.vector.factor(activity_labels[,2])
extracted_data[,2]<-activity_labels_vector[extracted_data[,2]]

###the data set with descriptive variable names
names(extracted_data)<-gsub(pattern="[^a-z]", replacement = " ", x=tolower(names(extracted_data)))
names(extracted_data)<-as.character(names(extracted_data))


###group the datasets by subject and activity
tidy_data<-extracted_data%>%
  group_by(subject,activity) %>%
  summarise_each(funs(mean), names(extracted_data)[3:37])

###export tidy data set into txt
write.table(tidy_data,file = "./tidy_data_set.txt", row.name=FALSE)
