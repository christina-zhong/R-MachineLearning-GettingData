# 1. Merges the training and the test sets to create one data set named "bodyData".
testData<-read.fwf(file="UCIHARDataset/test/X_test.txt",skip=0,width=rep(16,561));
trainData<-read.fwf(file="UCIHARDataset/train/X_train.txt",skip=0,width=rep(16,561));
bodyData<-rbind(trainData,testData);
any(is.na(bodyData))
features<-read.table(file="UCIHARDataset/features.txt", sep=" ", header=FALSE,stringsAsFactors=FALSE);
colnames(bodyData)<-features[,2];

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
colSelected<-grep("mean\\(|std",features[,2]);
bodySelected<-bodyData[,colSelected];

# 3. Uses descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names with column "activity" and "activityName". 
testActivity<-read.fwf(file="UCIHARDataset/test/Y_test.txt",skip=0,width=c(1));
trainActivity<-read.fwf(file="UCIHARDataset/train/Y_train.txt",skip=0,width=c(1));
activityNames<-read.table(file="UCIHARDataset/activity_labels.txt", sep=" ",header=FALSE,stringsAsFactors=FALSE);
activity<-c(trainActivity[,1],testActivity[,1]);
activityLabels<-data.frame(activity=activity,activityName=rep(activityNames[1,2],length(activity)),stringsAsFactors=FALSE);
for(i in 2:6){
  activityLabels[activityLabels$activity == i,]$activityName=activityNames[i,2]
}
bodyData<-cbind(bodyData,activityLabels);

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
meanCol<-grep("mean\\(",features[,2]);
meanBodyData<-aggregate(bodyData[,meanCol],list(bodyData$activity),mean);
colnames(meanBodyData)[1]<-"activity"
rownames(meanBodyData)<-activityNames[,2];
write.table(meanBodyData,file="averagebody.txt",row.name=FALSE);
