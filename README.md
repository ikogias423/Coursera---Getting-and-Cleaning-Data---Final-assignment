#Course Assignment
rm(list=ls(all=TRUE))
setwd("C:/Users/Karakatsougkas/Dropbox/Data Analysis material/Getting and Cleaning Data/Project/UCI HAR Dataset")

TrainLabels <- read.table("./train/y_train.txt")
TrainSet <- read.table("./train/X_train.txt")
TrainSub <- read.table("./train/subject_train.txt")

TestLabels <-  read.table("./test/y_test.txt")
TestSet <- read.table("./test/X_test.txt")
TestSub <- read.table("./test/subject_test.txt")


####Step 1: Merges the training and the test sets to create one data set.

#Adds following vars in main datasets: ID, Subject, Activity.
TrainSet$ID <- 1:nrow(TrainSet)
TrainSet$Subject <- TrainSub[["V1"]]  #We use the notation [[]] to extract a vector/column from the TrainSub data frame. Otherwise, a data frame will be stored as a columb in TrainSet.
TrainSet$Activity <- TrainLabels[["V1"]]
TrainSet <- TrainSet[,c(562,563,564, 1:561)]  #Brings ID, Subject, Activity col from the rightmost to leftmost.

TestSet$ID <- 1:nrow(TestSet)
TestSet$Subject <- TestSub[["V1"]]
TestSet$Activity <- TestLabels[["V1"]]
TestSet <- TestSet[,c(562,563,564, 1:561)]  #Brings ID, Subject, Activity col from the rightmost to leftmost.

#Merge test and train 
merged <- merge(TestSet, TrainSet, all = T)  # "all = T" asks to merge all columns with same name. 
merged$ID <- c(1:nrow(merged)) #renaming the ID index values


#####Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.  #(8*3 + 9)*17 all vars
varlist <- read.table("./features.txt")
#grep("mean[()]|std[()]", varlist$V2, value = T)   #verify that only mean() and std() are chosen.
mergedmeanstd <- merged[,c(T, T, T, grepl("mean[()]|std[()]", varlist$V2))]


#####Step 3: Uses descriptive activity names to name the activities in the data set
mergedmeanstd$Activity <- as.factor(mergedmeanstd$Activity)  #First convert it to factors, then below change factor's level names:
levels(mergedmeanstd$Activity)[] <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

#####Step 4: Appropriately labels the data set with descriptive variable names.
names(mergedmeanstd)[4:NCOL(mergedmeanstd)] <- grep("mean[()]|std[()]", varlist$V2, value = TRUE)  #starts from '4' because first two columns are ID, ACTIVITY.
names(mergedmeanstd)[4:NCOL(mergedmeanstd)] <- gsub("`","",names(mergedmeanstd)[4:NCOL(mergedmeanstd)]) #removes the ` at the start & end of each var. E.g. it was `fBodyBodyGyroMag-std()` instead of fBodyBodyGyroMag-std().
names(mergedmeanstd)[4:NCOL(mergedmeanstd)] <- gsub("[()]","", names(mergedmeanstd)[4:NCOL(mergedmeanstd)]) #removes () from names
names(mergedmeanstd)[4:NCOL(mergedmeanstd)] <- gsub("-","", names(mergedmeanstd)[4:NCOL(mergedmeanstd)]) #removes "-" from names



#####Step 5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Split <- split(mergedmeanstd[ ,1:NCOL(mergedmeanstd)], mergedmeanstd$Activity)
Y <- list()
final <- data.frame()
count <- 1
for (activity in as.character(levels(mergedmeanstd$Activity))){
        for(subj in 1:30){
        X <- subset(Split[[activity]], Split[[activity]][["Subject"]] == subj)
        XX <- as.data.frame(X)[,4:NCOL(as.data.frame(X))]
        XXX <- data.frame( colMeans(XX, dims = 1) )
        names(XXX) <- paste0("Subject_",subj, "_", activity)
        Y[[subj]] <- XXX
                if(count == 1 & subj == 1){
                        final <- Y[[1]]
                } else final <- cbind(final, Y[[subj]]) 
        }
count <- count + 1
}



#The data frame 'final' would be our final answer, however there is a slight problem:
#the measured vars are the columns' names while the subject's ID and activity are the rows' names.
#We want the opposite, as we want to measured vars to be the names of the columns (since they are the variables).
#We achieve this 'trasposition' of the data frame using a combination of the melt and dcast function of the 
#'reshape2' package.

final$VAR <- rownames(final)  #adds the rownames in a column in order to be used in the melt and cast command below, whic will set them as column names.
final[,181,1:180]
final <- final[,c(181,1:180)]
library(reshape2)
final2 <- dcast( melt(final, id.vars = "VAR"), variable ~ VAR)

rm(list=ls()[-4]) #removes all saved in the glbal environment data except the final data.frame 'final2'.


final2
#FINAL ANSWER: 'final2' is the tidy data set that contains the average of each activity and each subject.

#write.table(final2, "C:/Users/Karakatsougkas/Dropbox/Data Analysis material/Getting and Cleaning Data/Project/run_analysis.txt", row.names = FALSE)

