############ instead of imputing, removing missing values
sum(is.na(train_raw))
sum(is.na(test_raw))

#omitting
train_raw_omit<-na.omit(train_raw)

#smoting
train_rose_omit<-ROSE(Churn ~ ., data = train_raw_omit, seed = 15)$data

############# Straight to test

h2o.init(nthreads = -1)

colnames(train_raw_omit)
colnames(test_raw)
y.dep<-1
x.indep<-c(2:10,14:23)

trainreal.h2o<-as.h2o(train_raw_omit)
real.h2o<-as.h2o(test_raw)

rforest.model3 <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = trainreal.h2o, ntrees = 5000, mtries = 3,max_depth = 4, seed = 1210)
predict.rforest3 <- as.data.frame(h2o.predict(rforest.model3, real.h2o))
sub_rf4 <- data.frame(CustomerID = test_raw$CustomerID,Churn =  predict.rforest3$predict)
write.csv(sub_rf4,file='sub_omit.csv',row.names = F)
h2o.shutdown()
y
