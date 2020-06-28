library(dplyr)

#Download and save file

if(!file.exists('./data/Smartphones.zip')) {
  if(!dir.exists('./data')) {dir.create('./data')}
  fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
  download.file(fileURL, './data/Smartphones.zip')
  dir.create('./data/Smartphones')
  unzip('./data/Smartphones.zip', exdir = './data/Smartphones')
}

#Reading datasets

features <- read.table('./data/Smartphones/UCI HAR Dataset/features.txt', col.names = c("n","functions"))
activities <- read.table('./data/Smartphones/UCI HAR Dataset/activity_labels.txt', col.names = c("id", "activity"))
subject_test <- read.table('./data/Smartphones/UCI HAR Dataset/test/subject_test.txt', col.names = "subject")
x_test <- read.table('./data/Smartphones/UCI HAR Dataset/test/X_test.txt', col.names = features$functions)
y_test <- read.table('./data/Smartphones/UCI HAR Dataset/test/y_test.txt', col.names = "id")
subject_train <- read.table('./data/Smartphones/UCI HAR Dataset/train/subject_train.txt', col.names = "subject")
x_train <- read.table('./data/Smartphones/UCI HAR Dataset/train/X_train.txt', col.names = features$functions)
y_train <- read.table('./data/Smartphones/UCI HAR Dataset/train/y_train.txt', col.names = "id")

#Merging test and train

x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
merged_data <- cbind(subject, y_data, x_data)

data <- merged_data %>% select(subject, id, contains('mean'), contains('std'))
data$id <- activities[data$id, 2]

#Renaming columns

names(data)[2] = 'activity'

names(data) <- gsub('\\.+', '_', names(data), ignore.case = FALSE)
names(data) <- gsub('Acc', 'Accelerometer', names(data))
names(data) <- gsub('Gyro', 'Gyroscope', names(data))
names(data) <- gsub('BodyBody', 'Body', names(data))
names(data) <- gsub('Mag', 'Magnitude', names(data))
names(data) <- gsub('^t', 'Time', names(data))
names(data) <- gsub('^f', 'Frequency', names(data))
names(data) <- gsub('tBody', 'TimeBody', names(data))
names(data) <- gsub('-mean()_', 'Mean', names(data), ignore.case = FALSE)
names(data) <- gsub('-std()', 'St.deviation', names(data), ignore.case = FALSE)
names(data) <- gsub('-freq()', 'Frequency', names(data), ignore.case = FALSE)
names(data) <- gsub('gravity', 'Gravity', names(data))
names(data) <- gsub('angle', 'Angle', names(data))
names(data) <- gsub('.mean_', 'Mean', names(data), ignore.case = FALSE)
names(data) <- gsub('Gravity_', 'Gravity', names(data))
names(data) <- gsub('Freq_', 'Freq', names(data))
names(data) <- gsub('Mean_', 'Mean', names(data))
names(data) <- gsub('Magnitude_', 'Magnitude', names(data))
names(data) <- gsub('Angle_', 'Angle', names(data))

tidy_grouped_data <- data %>% group_by(subject, activity) %>% summarise_all(funs(mean))