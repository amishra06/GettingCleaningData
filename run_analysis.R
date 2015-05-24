##Load packages.
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

##Set path
path <- getwd()

##Get the data
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "project.zip"

#if (!file.exists(path)) {dir.create(path)}
#download.file(url, file.path(path, f))

#unzip ("project.zip", exdir = "./")

pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive=TRUE)

##Read the files
#Read the subject files.
datSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
datSubjectTest  <- fread(file.path(pathIn, "test" , "subject_test.txt" ))

#Read the activity files.
datActivityTrain <- fread(file.path(pathIn, "train", "y_train.txt"))
datActivityTest  <- fread(file.path(pathIn, "test" , "y_test.txt" ))

#Read the data files

fileToDataTable <- function (f) {
    df <- read.table(f)
    dat <- data.table(df)
}
datTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
datTest  <- fileToDataTable(file.path(pathIn, "test" , "X_test.txt" ))


##Merge the training and the test sets

#Concatenate the data tables.
datSubject <- rbind(datSubjectTrain, datSubjectTest)
setnames(datSubject, "V1", "subject")
datActivity <- rbind(datActivityTrain, datActivityTest)
setnames(datActivity, "V1", "activityNum")
dat <- rbind(datTrain, datTest)

#Merge columns.
datSubject <- cbind(datSubject, datActivity)
dat <- cbind(datSubject, dat)

#Set key.
setkey(dat, subject, activityNum)

#Extract only the mean and standard deviation
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

select <- c(key(dat), dtFeatures$featureCode)
dat <- dat[, select, with=FALSE]

##Use descriptive activity names
dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

##Merge activity labels.

dat <- merge(dat, dtActivityNames, by="activityNum", all.x=TRUE)
setkey(dat, subject, activityNum, activityName)
dat <- data.table(melt(dat, key(dat), variable.name="featureCode"))
dat <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
dat$activity <- factor(dat$activityName)
dat$feature <- factor(dat$featureName)

grepthis <- function (regex) {
  grepl(regex, dt$feature)
}

## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dat$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dat$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dat$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dat$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dat$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dat$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dat$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))


r1 <- nrow(dat[, .N, by=c("feature")])
r2 <- nrow(dat[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

##Create a tidy data set
setkey(dat, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
datTidy <- dat[, list(count = .N, average = mean(value)), by=key(dat)]

##Save to file
f <- file.path(path, "HumanActivityRecognitionUsingSmartphones.txt")
write.table(datTidy, f, quote = FALSE, sep = "\t", row.names = FALSE)
