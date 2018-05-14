str(train_raw_miss)
str(test_raw_miss)

################## Smoting
install.packages('ROSE')
library(ROSE)
train_rose<-ROSE(Churn ~ ., data = train_raw_miss, seed = 1)$data

##################### Splitting for validation
set.seed(300)
split = sample.split(train_rose$Churn, SplitRatio = 0.80)
train_rose_spl = subset(train_rose, split==TRUE)
val_rose_spl = subset(train_rose, split==FALSE)
val_result<-data.frame(val_rose_spl$Churn)
val_rose_spl$Churn<-NULL

###################### Model for validation
library(h2o)
h2o.init(nthreads = -1)

colnames(train_rose_spl)
colnames(val_rose_spl)
y.dep<-1
x.indep<-c(2:10,13:22)

train.h2o<-as.h2o(train_rose_spl)
test.h2o<-as.h2o(val_rose_spl)

rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, mtries = 3,max_depth = 4, seed = 1122)
h2o.performance(rforest.model)
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
sub_rf <- data.frame(CustomerID = val_rf$CustomerID,Churn =  predict.rforest$predict)
#write.csv(sub_rf,file='sub_rf.csv',row.names = F)
h2o.shutdown()
y
sub_rf$Churn
library(caret)
confusionMatrix(sub_rf$Churn,val_result$val_rose_spl.Churn)

h2o.gainsLift(val_result$val_rose_spl.Churn, predict.rforest$No, percents = TRUE)
h2o.gainsLift(rforest.model)


############################ for test

trainreal.h2o<-as.h2o(train_rose)
real.h2o<-as.h2o(test_raw_miss)
rforest.model2 <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = trainreal.h2o, ntrees = 5000, mtries = 3,max_depth = 4, seed = 1200)
predict.rforest2 <- as.data.frame(h2o.predict(rforest.model2, real.h2o))
sub_rf3 <- data.frame(CustomerID = test_raw$CustomerID,Churn =  predict.rforest2$predict)
write.csv(sub_rf3,file='sub_rose.csv',row.names = F)
h2o.shutdown()
y

######### best results so far