#Verify Location
if (!file.exists("UCI HAR Dataset")) {message("'UCI HAR Dataset' Folder Not Found!")}

Path = "UCI HAR Dataset/"

# Load Data
x_Train = read.table(paste(Path, "train/X_train.txt", sep=""))
y_Train = read.table(paste(Path, "train/y_train.txt", sep=""))
x_Test = read.table(paste(Path, "test/X_test.txt", sep=""))
y_Test = read.table(paste(Path, "test/y_test.txt", sep=""))


# Load Column Names
ColNames = read.table(paste(Path, "features.txt", sep=""))
ColNames = ColNames[,2];
ActivityLabels = read.table(paste(Path, "activity_labels.txt", sep=""))


# Add Column Names
names(x_Train) <- ColNames
names(x_Test)  <- ColNames

## Merge Test and Train
x_Merge = rbind(x_Test,x_Train) 
y_Merge = rbind(y_Test,y_Train) 

# Identify Mean-StdDev Columns 
ColMean = grep("mean", ColNames)
ColStd  = grep("std", ColNames)
z <- as.vector(rbind(ColMean, ColStd))
ColMeanStd <- unique(z)

# Subset
xSubset= x_Merge[,ColMeanStd]

# write into file:
write.csv(xSubset,file=paste(Path, "xSubset.csv", sep=""))


# Labels
yVectorLabels=vector(mode="character", length=length(y_Merge))

# for each activity label, actually put in a factor variable with the name of the activity
for(a in 1:6){
  inds = which(y_Merge==a)
  yVectorLabels[inds] <- as.character(activityLabels[a,2])
}

# loading subject IDs
subjectTrain = read.table(paste(Path, "train/subject_train.txt", sep=""))
subjectTest = read.table(paste(Path, "test/subject_test.txt", sep=""))

## renaming IDs to more appropriate 
# first create a function to add the word "Subject" to the number ID
subjectify = function(x){return(paste("Subject", as.character(x), sep=""))}
# apply said function to every element 
subjectNamesTest= sapply(subjectTest, FUN=subjectify)
subjectNamesTrain= sapply(subjectTrain, FUN=subjectify)

# also merge the test and train subject names
subjectNamesVector = rbind(subjectNamesTrain, subjectNamesTest)


### creating a second dataset that has the means of each variable for each subject
newData = matrix(ncol=length(names(xSubset)), nrow=length(unique(subjectNamesVector)))
rownames(newData) = unique(subjectNamesVector); 
colnames(newData) = names(xSubset);

# for each subject calculate the mean of all variables
for(s in unique(subjectNamesVector)){
    w = which(subjectNamesVector == s)
    cm = colMeans(xSubset[w,])
    newData[s,] = cm
}
write.csv(newData,"UCI HAR Dataset/tidy.csv",row.names=FALSE)
