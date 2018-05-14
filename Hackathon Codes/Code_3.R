sum(is.na(train_raw))
sum(is.na(test_raw))

#imputing by missforest package
library(missForest)
train_raw_miss<-train_raw
train_raw_miss$CustomerID<-NULL
train_raw_miss<-missForest(train_raw_miss)
test_raw_miss<-test_raw
test_raw_miss$CustomerID<-NULL
test_raw_miss<-missForest(test_raw_miss)

#Creating the imputed dataframe
train_raw_miss<-train_raw_miss$ximp
str(train_raw_miss)
sum(is.na(train_raw_miss))

#Creating imputed test df
test_raw_miss<-test_raw_miss$ximp
str(test_raw_miss)
sum(is.na(test_raw_miss))

###################### Splitting for validation
set.seed(1002)
split = sample.split(train_raw_miss$Churn, SplitRatio = 0.80)
train_rf_miss = subset(train_raw_miss, split==TRUE)
val_rf_miss = subset(train_raw_miss, split==FALSE)
val_result<-data.frame(val_rf_miss$Churn)
val_rf_miss$Churn<-NULL

################## Model for validation
library(h2o)
h2o.init(nthreads = -1)

colnames(train_rf_miss)
colnames(test_raw_miss)
y.dep<-1
x.indep<-c(2:10,13:22)

train.h2o<-as.h2o(train_rf_miss)
test.h2o<-as.h2o(val_rf_miss)

rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, mtries = 3,max_depth = 4, seed = 1122)
h2o.performance(rforest.model)
predict.rforest <- as.data.frame(h2o.predict(rforest.model, real.h2o))
sub_rf <- data.frame(CustomerID = test_raw$CustomerID,Churn =  predict.rforest$predict)
h2o.shutdown()
y
sub_rf$Churn
library(caret)
confusionMatrix(sub_rf$Churn,val_result$val_rf_miss.Churn)

########################### Model for test
trainreal.h2o<-as.h2o(train_raw_miss)
real.h2o<-as.h2o(test_raw_miss)
rforest.model2 <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = trainreal.h2o, ntrees = 1000, mtries = 3,max_depth = 4, seed = 1122)
predict.rforest2 <- as.data.frame(h2o.predict(rforest.model, real.h2o))
sub_rf2 <- data.frame(CustomerID = test_raw$CustomerID,Churn =  predict.rforest2$predict)
write.csv(sub_rf,file='sub_rf2.csv',row.names = F)
