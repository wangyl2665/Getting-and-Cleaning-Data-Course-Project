#===============================================================================
# load package
library(readr)
library(tidyr)
library(dplyr)


#===============================================================================
# 1. Merges the training and the test sets to create one data set.
features <- read_table2("./features.txt", col_names = c('ID', 'Name'))

X_train <- read_table2("./train/X_train.txt", col_names = features$Name)
subject_train <- read_table2("./train/subject_train.txt", col_names = 'subjectid')
y_train <- read_csv("./train/y_train.txt", col_names = 'activity', col_types = 'c')

X_test <- read_table2("./test/X_test.txt", col_names = features$Name)
subject_test <- read_table2("./test/subject_test.txt", col_names = 'subjectid')
y_test <- read_csv("./test/y_test.txt", col_names = 'activity', col_types = 'c')

trainset <- bind_cols(subject_train, y_train, X_train)
testset <- bind_cols(subject_test, y_test, X_test)

dataset <- bind_rows(trainset, testset)


#===============================================================================
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
meanstddata <- dataset %>% 
        select(subjectid, activity, matches('mean()|std()'))


#===============================================================================
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately activitys the data set with descriptive variable names.
activity_activitys <- read_table2("./activity_labels.txt", 
                                  col_names = c('Id', 'activity'))
meanstddata <- meanstddata %>% 
        mutate(activity = case_when(
                activity == '1' ~ 'WALKING',
                activity == '2' ~ 'WALKING_UPSTAIRS',
                activity == '3' ~ 'WALKING_DOWNSTAIRS',
                activity == '4' ~ 'SITTING',
                activity == '5' ~ 'STANDING',
                activity == '6' ~ 'LAYING'
        )) 


#===============================================================================
# 5. From the data set in step 4, 
#    creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidyset <- meanstddata %>% 
        group_by(subjectid, activity) %>% 
        summarise_all(funs(mean)) %>% 
        write_csv('./tidy.csv')




