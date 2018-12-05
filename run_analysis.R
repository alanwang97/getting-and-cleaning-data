library(stringr)
library(dplyr)

match_feature <- function(x){
        if (str_trim(x)==1){
                x <- "WALKING"
        }
        if (str_trim(x)==2){
                x <- "WALKING_UPSTAIRS"
        }
        if (str_trim(x)==3){
                x <- "WALKING_DOWNSTAIRS"
        }
        if (str_trim(x)==4){
                x <- "SITTING"
        }
        if (str_trim(x)==5){
                x <- "STANDING"
        }
        if (str_trim(x)==6){
                x <- "LAYING"
        }
        return(x)
}
#this function can match the activities with their labels.
y_match <- function(t){
        for (i in 1:nrow(t)){
                
                t[[i,1]] <- match_feature(t[[i,1]])
                
        }
        return(t)
}
#this function call the match_function to replace the content in a file which only has the labels of the activities.

mean_col <- function(x){
        output <- vector()
        for (i in 1 : length(names(x)) ){
                if (length(unique(x[,i])) == 1){
                        output <- cbind(output,unique(x[,i]))
                }
                else{
                        output <- cbind(output,mean(x[,i],rm.na = TRUE))
                        
                }
        }
        return(as.vector(output))
}
#this function can produce the mean of each col in a dataframe,and if the content of a column is all the same, it will not process the column.

train_x <- read.table("train/X_train.txt")
train_y <- read.table("train/y_train.txt")
train_subject <- read.table("train/subject_train.txt")
test_subject <- read.table("test/subject_test.txt")
test_x <- read.table("test/X_test.txt")
test_y <- read.table("test/y_test.txt")
features <- read.table("features.txt")
#read the fiels

test_y_1 <- y_match(test_y)
train_y_1 <- y_match(train_y)
#replace labels with the real activities

whole_test <- cbind(test_subject,test_y_1,test_x)
names(whole_test) <- c("subject","active",as.character(features$V2))
whole_train <- cbind(train_subject,train_y_1,train_x)
names(whole_train) <- c("subject","active",as.character(features$V2))
#use activities, subject, and features to generate dataframes which have all the data needed. 


selected_train <- whole_train[,c(1,2,(grep("mean()|std()",as.character(features[,2])))+2)]
selected_test <- whole_test[,c(1,2,(grep("mean()|std()",as.character(features[,2])))+2)]
#Extracts only the measurements on the mean and standard deviation for each measurement.

selected_whole_data <- merge(selected_train,selected_test,all = TRUE)
#merge the data of train and of the test

# 5.From the data set in step 4, creates a second, independent tidy data set with the average 
#   of each variable for each activity and each subject.

answer<- data.frame()
for (i in 1:length(unique(selected_whole_data$subject))){
        splited1_data <- split(selected_whole_data,selected_whole_data$subject)[[i]]
        
        for (i_ in 1:length(unique(splited1_data$active))) {
                splited2_data <- split(splited1_data,splited1_data$active)[[i_]]
                answer <- rbind(answer,mean_col(splited2_data),stringsAsFactors = FALSE)
        }
}

