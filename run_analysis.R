library(dplyr)

transformation <- function(x) {
    splited_value <- strsplit(as.character(x), ' ')
    splited_value <- as.numeric(splited_value[[1]])
    splited_value <- na.omit(splited_value)
    mean <- mean(splited_value)
    st_d <- sd(splited_value)
    c(mean, st_d)
}

#Download and save file

if(!file.exists('./data/Smartphones.zip')) {
    if(!dir.exists('./data')) {dir.create('./data')}
    fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
    download.file(fileURL, './data/Smartphones.zip')
    dir.create('./data/Smartphones')
    unzip('./data/Smartphones.zip', exdir = './data/Smartphones')
}

#Reading and merging datasets

test_files <- list.files(path = './data/Smartphones/UCI HAR Dataset/test', pattern = 'txt$')
test_files <- paste('./data/Smartphones/UCI HAR Dataset/test/' ,test_files, sep='')
test_data <- sapply(test_files, read.delim)
test <- data.frame(test_data[1], test_data[2], test_data[3])
test[4] <- 0
colnames(test) <- c('subject', 'set', 'activity', 'train')
for (i in 1:length(test[, 2])) {
    test[i, 5] <- transformation(test[i, 2])[1]
    test[i, 6] <- transformation(test[i, 2])[2]
}
colnames(test)[5] <- 'mean'
colnames(test)[6] <- 'st.deviation'
test <- select(test, -2)

test_intertial_signals <- list.files(path = './data/Smartphones/UCI HAR Dataset/test/Inertial Signals', pattern = 'txt$')
test_intertial_signals <- paste('./data/Smartphones/UCI HAR Dataset/test/Inertial Signals/' ,test_intertial_signals, sep='')
test_intertial_signals_data <- sapply(test_intertial_signals, read.delim)
test_intertial <- bind_cols(test_intertial_signals_data)

test_intertial <- as.data.frame(test_intertial)
test_signals <- data.frame(matrix(NA ,nrow = 2946, ncol = 18))
options(scipen = 999)
for (i in 1:9){
    for(j in 1:length(test_intertial[,i])) {
        test_signals[j, 2*i-1] <- transformation(test_intertial[j, i])[1]
        test_signals[j, 2*i] <- transformation(test_intertial[j, i])[2]
    }
}
test_signals[19] <- 0
colnames(test_signals) <- c('mean.body.acc.x', 'st.body.acc.x', 'mean.body.acc.y',
                            'st.body.acc.y','mean.body.acc.z', 'st.body.acc.z', 'mean.body.gyro.x',
                            'st.body.gyro.x', 'mean.body.gyro.y', 'st.body.gyro.y' , 'mean.body.gyro.z',
                            'st.body.gyro.z', 'mean.total.acc.x', 'st.total.acc.x', 'mean.total.acc.y',
                            'st.total.acc.y', 'mean.total.acc.z', 'st.total.acc.z', 'train')

train_intertial_signals <- list.files(path = './data/Smartphones/UCI HAR Dataset/train/Inertial Signals', pattern = 'txt$')
train_intertial_signals <- paste('./data/Smartphones/UCI HAR Dataset/train/Inertial Signals/' ,train_intertial_signals, sep='')
train_intertial_signals_data <- sapply(train_intertial_signals, read.delim)
train_intertial <- bind_cols(train_intertial_signals_data)

train_intertial <- as.data.frame(train_intertial)
train_signals <- data.frame(matrix(NA ,nrow = 2946, ncol = 18))
options(scipen = 999)
for (i in 1:9){
    for(j in 1:length(train_intertial[,i])) {
        train_signals[j, 2*i-1] <- transformation(train_intertial[j, i])[1]
        train_signals[j, 2*i] <- transformation(train_intertial[j, i])[2]
    }
}
train_signals[19] <- 1
colnames(train_signals) <- c('mean.body.acc.x', 'st.body.acc.x', 'mean.body.acc.y',
                            'st.body.acc.y','mean.body.acc.z', 'st.body.acc.z', 'mean.body.gyro.x',
                            'st.body.gyro.x', 'mean.body.gyro.y', 'st.body.gyro.y' , 'mean.body.gyro.z',
                            'st.body.gyro.z', 'mean.total.acc.x', 'st.total.acc.x', 'mean.total.acc.y',
                            'st.total.acc.y', 'mean.total.acc.z', 'st.total.acc.z', 'train')

train_files <- list.files(path = './data/Smartphones/UCI HAR Dataset/train', pattern = 'txt$')
train_files <- paste('./data/Smartphones/UCI HAR Dataset/train/' ,train_files, sep='')
train_data <- sapply(train_files, read.delim)
train <- data.frame(train_data[1], train_data[2], train_data[3])
train[4] <- 1
colnames(train) <- c('subject', 'set', 'activity', 'train')
for (i in 1:length(train[, 2])) {
    train[i, 5] <- transformation(train[i, 2])[1]
    train[i, 6] <- transformation(train[i, 2])[2]
}
colnames(train)[5] <- 'mean'
colnames(train)[6] <- 'st.deviation'
train <- select(train, -2)

data <- rbind(test, train)
data[6] <- 1:10297
colnames(data)[6] <- 'id'
data_signals <- rbind(test_signals, train_signals)
data_signals[20] <- 1:10297
colnames(data_signals)[20] <- 'id'

merged_data <- merge(data, data_signals, by = c('id', 'train'))
merged_data <- merged_data[order(merged_data$id), ]

activity_names <- c('WALKING', 'WALKING_UPSTAIRS', 'WALKING_DOWNSTAIRS', 'SITTING', 'STANDING', 'LAYING')
for (l in 1:6) {
    index <- merged_data$activity == l
    merged_data$activity[index] <- activity_names[l]
}

#Creating dataset for averages

average_data <- merged_data %>% group_by(subject, activity) %>% summarise_all('mean') %>% select(-'id')

