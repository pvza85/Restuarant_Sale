rawtrain <- read.csv("./data/train.csv")
rawtest <- read.csv("./data/test.csv")
cities <- read.csv("./data/cities.csv")




#keep untouched data just in case
train <- rawtrain
test <- rawtest




#calculating dates
train$Open.Date <- as.numeric(Sys.Date() - as.Date(train$Open.Date, format="%m/%d/%Y"))
test$Open.Date <- as.numeric(Sys.Date() - as.Date(test$Open.Date, format="%m/%d/%Y"))





#augmenting city information
train <- merge(train, cities)
test <- merge(test, cities)




#rm(cities)

#[optional]merge function ruin the arrange 
library(dplyr)
train <- arrange(train, Id)#
test <- arrange(test, Id)




#separate labels 
train <- cbind(train[,2], train[,1], train[,3:45])
test <- cbind(test[,2], test[,1], test[,3:44])
labels <- as.matrix(train[,43])
train <- train[,-43]
names(train) <- c("Id", "City", names(train[,3:44]))
names(test) <- c("Id", "City", names(test[,3:44]))



#convert all to nummeric
train <- data.frame(lapply(train,as.numeric))
test <- data.frame(lapply(test,as.numeric))




#exclude data indeces
train <- train[,-1]
test <- test[,-1]

traina <- train
testa <- test

#exclude population [you can exclude city name also]
train <- train[,-c(42)]
test <- test[,-c(42)]





#-------------------------preprocess----------------------------
#I did not used this part, but just in case if you want it, every where you need to replace "train" by "ptrain" and "test" by "ptest"
library(caret)
preProcValues <- preProcess(train, method = c("center", "scale"))
ptrain <- predict(preProcValues, train)
ptest <- predict(preProcValues, test)

#cross validation
library(caret)
ctrl <- trainControl(method = "cv", savePred=T, classProb=T)





#-----------------training------------------------------------ 

library(caret)
rfModel <- train( train, labels, method="rf", prox=TRUE, trControl=ctrl, ntree=1000)




svmModel <- train(train, labels, method="svmRadial", trControl=ctrl, tuneGrid = data.frame(.C = c(.125,.25, .5, 1,2,4),.sigma = c(.0625,.125,.25,.05, 1,2)))

gModel <- train(train, labels, method="gaussprRadial", trControl=ctrl)

boostModel <- train(train, labels, method="bstTree", trControl=ctrl)

#it is not working now, I changed the parameters and "ridam" will fix it and send you best nn model
set.seed(2258)
nnModel <- train(train, labels, method = "nnet", trControl=ctrl, algorithm = "backprop", hidden=86,trace = T, linout = 1)





#--------------------prediction---------------------------------
pred1 <- predict(rfModel, test)
pred2 <- predict(nnModel, test)
pred3 <- predict(svmModel, test)
pred4 <- predict(boostModel, test)
pred5 <- predict(gModel, test)
pred <- (pred1 + pred3 + pred4 + pred5 )/4#bagging


#[optional] I just want to see the result of training over train set
tpred1 <- predict(rfModel, train)
tpred2 <- predict(nnModel, train)
tpred3 <- predict(svmModel, train)
tpred4 <- predict(boostModel, train)
tpred5 <- predict(gModel, train)
tpred <- (tpred1  + tpred3 + tpred4 + tpred5 )/4
tpred <- as.matrix(tpred)
tpred1 <- as.matrix(tpred1)
tpred2 <- as.matrix(tpred2)
tpred3 <- as.matrix(tpred3)
tpred4 <- as.matrix(tpred4)
tpred5 <- as.matrix(tpred5)
plot(labels, tpred)
x <- tpred
er <- sum(sqrt((x - labels)^2))/137
er



#------------finilize prediction and write them file-------------
pred <- as.matrix(pred)
pred <- cbind(1:nrow(pred), pred)
colnames(pred) <- c("Id", "Prediction")
pred[,1] <- pred[,1]-1

#[optional] 
l <- rawtest$Type != "MB"
pred[,2] <- pred[,2] * 1.2


write.csv(pred , "combineres8.csv", row.names = FALSE, quote=FALSE)
