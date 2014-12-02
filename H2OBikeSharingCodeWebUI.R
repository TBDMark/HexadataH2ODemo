# Load Data

BikeTrain <- read.csv("~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/train.csv")
BikeTest <- read.csv("~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/test.csv")
Sample <- read.csv("~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/samplesubmission.csv")

str(BikeTrain)

#update fields properly to factors

train <- BikeTrain

train$season <- factor(train$season)
train$weather <- factor(train$weather)
train$holiday <- factor(train$holiday)
train$workingday <- factor(train$workingday)

str(train)

test <- BikeTest

test$season <- factor(test$season)
test$weather <- factor(test$weather)
test$holiday <- factor(test$holiday)
test$workingday <- factor(test$workingday)

str(test)

#Feature engineering

#pull out time and convert to factor
train$time <- substring(train$datetime,12,20)
train$time <- factor(train$time)

test$time <- substring(test$datetime,12,20)
test$time <- factor(test$time)

str(train)
str(test)

#pull out day

train$day <- weekdays(as.Date(train$datetime))
test$day <- weekdays(as.Date(test$datetime))

train$day <- factor(train$day)
test$day <- factor(test$day)

#count by day
sqldf("SELECT SUM(count), day FROM train GROUP BY day ORDER BY SUM(count) ASC")

#convert temp to Farenheit

train$ftemp <- (train$temp * (9/5)) + 32
train$fatemp <- (train$atemp * (9/5)) + 32
train$fatempcalc <- (35.74+ (0.6215 * train$ftemp)) - (35.75*(train$windspeed^0.16)) + (0.4275 * train$ftemp*(train$windspeed^0.16))

test$ftemp <- (test$temp * (9/5)) + 32
test$fatemp <- (test$atemp * (9/5)) + 32
test$fatempcalc <- (35.74+ (0.6215 * test$ftemp)) - (35.75*(test$windspeed^0.16)) + (0.4275 * test$ftemp*(test$windspeed^0.16))

#time of day

#convert time and create $hour as integer to evaluate for daypart
train$hour<- as.numeric(substr(train$time,1,2))
test$hour<- as.numeric(substr(test$time,1,2))

#create daypart column, default to 4 to make things easier for ourselves
train$daypart <- "4"
test$daypart <- "4"


#4AM - 10AM = 1
train$daypart[(train$hour < 10) & (train$hour > 3)] <- 1
test$daypart[(test$hour < 10) & (test$hour > 3)] <- 1


#11AM - 3PM = 2
train$daypart[(train$hour < 16) & (train$hour > 9)] <- 2
test$daypart[(test$hour < 16) & (test$hour > 9)] <- 2


#4PM - 9PM = 3
train$daypart[(train$hour < 22) & (train$hour > 15)] <- 3
test$daypart[(test$hour < 22) & (test$hour > 15)] <- 3

#convert daypart to factor
train$daypart <- as.factor(train$daypart)
test$daypart <- as.factor(test$daypart)

#convert hour back to factor
train$hour <- as.factor(train$hour)
test$hour <- as.factor(test$hour)

#write data for H2O processing in web UI

write.csv(train, file = "~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/trainh2o.csv")
write.csv(test, file = "~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/testh2o.csv")

#run brandon harris models

library(party)

formula <- count ~ season + holiday + workingday + weather + humidity + windspeed + day + ftemp + fatemp + hour + daypart

fit.ctree <- ctree(formula, data=train)
predict.ctree <- predict(fit.ctree,test)
submit.ctree <- data.frame(datetime = test$datetime, count=predict.ctree)

write.csv(submit.ctree, file="~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/20141121_ctree_submission1.csv",row.names=FALSE)

#rpart models
library(rpart)

fit.rpart <- rpart(formula, data=train)
plot(fit.rpart)

predict.rpart <- predict(fit.rpart,test)
submit.rpart <- data.frame(datetime = test$datetime, count=predict.rpart)

write.csv(submit.rpart, file="~/Two Bytes Data/Kaggle/Kaggle_BikeSharing/20141121_rpart_submission1.csv",row.names=FALSE)

#GLM
fit.GLM <- glm(formula, data=train)
