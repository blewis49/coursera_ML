library(tidyverse)
library(caret)
fileurl1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(fileurl1, destfile = "training.csv", method = "curl")
fileurl2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fileurl2, destfile = "testing.csv", method = "curl")

training <- read.csv("training.csv", na.strings = c("", "NA"), stringsAsFactors = TRUE)
testing <- read.csv("testing.csv", na.strings = c("", "NA"), stringsAsFactors = TRUE)

# ---- Dealing with Missing Data --------------
sapply(training, function(x) sum(is.na(x)))
sapply(testing, function(x) sum(is.na(x)))

# Let's look at the distribution of features that are missing data.  There are several features that are missing 19,216 values.  Some of these are multi-modal
histogram(training$max_roll_belt, na.rm = T)
ggplot(final_training) + geom_density(aes(max_roll_belt))
ggplot(training) + geom_density(aes(avg_yaw_dumbbell))
ggplot(training) + geom_boxplot(aes(x=classe, y=max_roll_belt))

# how can we remove the columns that are missing so much data?
# use the apply function to iterate over each column and return those that do not have any NAs
training <- training[,apply(training, 2, function(x) !any(is.na(x)))]
testing <- testing[,apply(testing, 2, function(x) !any(is.na(x)))]

training <- training[,-c(1,3:5)]
testing <- testing[,-c(1,3:5)]

# Another option using randomForest::randomForest
library(randomForest)
set.seed(1234)
trainBuild <- createDataPartition(y = training$classe, p = 0.75, list = FALSE)
train <- training[trainBuild,]
test <- training[-trainBuild,]
modRF <- randomForest(classe ~., data = train)
predRF <- predict(modRF, test)
modRF$confusion
accuracy <- sum(predRF == test$classe) / length(predRF)

# Original model build
modelRF <- randomForest(classe ~., data = training)
levels(testing$new_window) <- levels(training$new_window)
predictRF <- predict(modelRF, testing)

# Answers
#1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
#B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
#http://nthturn.com/2015/02/22/prediction-using-random-forests-in-r-an-example/