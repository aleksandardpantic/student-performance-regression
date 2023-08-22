rm(list = ls())

data <- read.csv('Student_Performance.csv')

library(e1071)
library(caret)
set.seed(100)
ind <- createDataPartition(data$Performance.Index, p=.8, list=F)
train.data <- data[ind,]
test.data <- data[-ind,]

trc <- trainControl(method = 'cv', number = 5)
model <- train(Performance.Index ~., 
               data = train.data, 
               method = 'svmRadial', 
               preProcess = c("center", "scale"),
               trControl = trc)
model$bestTune$sigma


regressor <- svm(formula = Performance.Index ~.,data = train.data, kernel = 'radial',cost = model$bestTune$C)

predictions <- predict(regressor,newdata = test.data)
actual <- test.data$Performance.Index

rss <- sum((predictions-actual)^2)
tss <- sum((actual - mean(actual))^2)

r.squared <- 1 - rss/tss


rows <- nrow(test.data)
cols <- length(test.data)-1
adjusted.r.squared <- 1 - (1-r.squared)*(rows-1)/(rows-cols-1)
