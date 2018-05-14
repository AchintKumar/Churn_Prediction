#Reading file path
setwd('F:/PHD/Hack/Data')
getwd()

############# AGGREGATION

#Reading the files
data1<-read.csv('Train.csv')
data2<-read.csv('Train_AccountInfo.csv')
data3<-read.csv('Train_Demographics.csv',na.strings = c('?','',' ','MISSINGVAL'))
data4<-read.csv('Train_ServicesOptedFor.csv')

#Reshaping data4
library(reshape2)
data4_reshaped<-dcast(data4,CustomerID~data4$TypeOfService)

#Merging all the files
data3$HouseholdID<-NULL
data4_reshaped$CustomerID<-NULL
data1$CustomerID<-NULL
train_new<-cbind(data2,data3,data4_reshaped,data1)

#Reading test files
test1<-read.csv('Test.csv')
test2<-read.csv('Test_AccountInfo.csv')
test3<-read.csv('Test_Demographics.csv',na.strings = c('?','',' ','MISSINGVAL'))
test4<-read.csv('Test_ServicesOptedFor.csv')

#Reshaping test4
test4_reshaped<-dcast(test4,CustomerID~test4$TypeOfService)

#Merging all test files
test3$HouseholdID<-NULL
test4_reshaped$CustomerID<-NULL
test1$CustomerID<-NULL
test_new<-cbind(test2,test3,test4_reshaped,test1)

rm(data1,data2,data3,data4,data4_reshaped,test1,test2,test3,test4,test4_reshaped)

############### ANALYSIS

#changing date column from factors to dates
train_new$DOC<-as.Date(train_new$DOC, format="%d/%m/%Y")
train_new$DOE<-as.Date(as.character(train_new$DOE), format='%d-%b-%y')

test_new$DOC<-as.Date(test_new$DOC, format="%d/%m/%Y")
test_new$DOE<-as.Date(as.character(test_new$DOE), format='%d-%b-%y')

str(train_new)
str(test_new)

#making new variable explaining the number of days the customer had the network
train_new$diff<-train_new$DOC-train_new$DOE
train_new$diff<-as.numeric(train_new$diff)

test_new$diff<-test_new$DOC-test_new$DOE
test_new$diff<-as.numeric(test_new$diff)

#deleting country and state (no variance)
train_new$Country<-NULL
train_new$State<-NULL

test_new$Country<-NULL
test_new$State<-NULL

#Changing various columns to factor
colnames(train_new)
colnames(test_new)
x<-colnames(train_new[14:22])
x<-append(x,colnames(train_new[9:11]))

train_new[x]<-lapply(train_new[x], factor)
test_new[x]<-lapply(test_new[x], factor)

#Changing total charges to numeric
train_new$TotalCharges<-as.numeric(as.character(train_new$TotalCharges))
test_new$TotalCharges<-as.numeric(as.character(test_new$TotalCharges))

################ MODEL 1
#Using h2o for faster computation
library(h2o)
#Initializing h2o and allowing all cores to run parallely
h2o.init(nthreads = -1)

#Selecting the independent and target variable(s)
colnames(train_new)
colnames(test_new)
y.dep<-23
x.indep<-c(2,4,6:22,24)

#Reading datasets as h2o frames
train.h2o<-as.h2o(train_new)
test.h2o<-as.h2o(test_new)

#Training the model
rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, mtries = 3, max_depth = 4, seed = 1122)

#Checking the performance of the model
h2o.performance(rforest.model)

#Predicting the values
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))

#Creating a submission 
improved_1 <- data.frame(CustomerID = test_new$CustomerID,Churn =  predict.rforest$predict)
write.csv(improved_1,file='improved_1.csv',row.names = F)

#Shutting down h2o
h2o.shutdown()
y

####################### MODEL 2
library(h2o)
h2o.init(nthreads = -1)

colnames(train_new)
colnames(test_new)
y.dep<-23
x.indep<-c(2,4,6:22,24)

train.h2o<-as.h2o(train_new)
test.h2o<-as.h2o(test_new)


rforest.model2 <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 2500, mtries = 5, max_depth = 4, seed = 1124)
#h2o.performance(rforest.model2)
predict.rforest2 <- as.data.frame(h2o.predict(rforest.model2, test.h2o))
improved_2 <- data.frame(CustomerID = test_new$CustomerID,Churn =  predict.rforest2$predict)
write.csv(improved_2,file='improved_2.csv',row.names = F)
h2o.shutdown()
y

####################### MODEL 3
library(h2o)
h2o.init(nthreads = -1)

colnames(train_new)
colnames(test_new)
y.dep<-23
x.indep<-c(6:22,24)

train.h2o<-as.h2o(train_new)
test.h2o<-as.h2o(test_new)


naive_model <- h2o.naiveBayes(x=x.indep, y=y.dep, train.h2o)
predict.naive <- as.data.frame(h2o.predict(naive_model, test.h2o))
improved_3 <- data.frame(CustomerID = test_new$CustomerID,Churn =  predict.naive$predict)
write.csv(improved_3,file='improved_3.csv',row.names = F)
h2o.shutdown()
y

###################### MODEL 4
#Making a new dataframe
train_naive<-train_new
str(train_naive)

#Removing numeric columns
train_naive$BaseCharges<-NULL
train_naive$TotalCharges<-NULL
train_naive$DOC<-NULL
train_naive$DOE<-NULL

#Checking histogram of new variable created earlier
hist(train_naive$diff)

#Binning into equal frequencies
library(infotheo)
diff<-discretize(train_naive$diff,'equalfreq',nbins = 4)
train_naive$diff<-diff$X
train_naive$diff<-as.factor(train_naive$diff)

#Doing same to test
test_naive<-test_new
str(test_naive)
test_naive$BaseCharges<-NULL
test_naive$TotalCharges<-NULL
test_naive$DOC<-NULL
test_naive$DOE<-NULL
hist(test_naive$diff)
library(infotheo)
diff<-discretize(test_naive$diff,'equalfreq',nbins = 4)
test_naive$diff<-diff$X
test_naive$diff<-as.factor(test_naive$diff)

#implementing naive bayes
h2o.init(nthreads = -1)

colnames(train_naive)
colnames(test_naive)
y.dep<-19
x.indep<-c(2:18,20)

train.h2o<-as.h2o(train_naive)
test.h2o<-as.h2o(test_naive)


naive_model2 <- h2o.naiveBayes(x=x.indep, y=y.dep, train.h2o)
predict.naive2 <- as.data.frame(h2o.predict(naive_model2, test.h2o))
improved_4 <- data.frame(CustomerID = test_new$CustomerID,Churn =  predict.naive2$predict)
write.csv(improved_4,file='improved_4.csv',row.names = F)
h2o.shutdown()
y

############################# MODEL 5
#Reinitializing dataframe
train_naive<-train_new
str(train_naive)

#Removing date columnms
train_naive$DOC<-NULL
train_naive$DOE<-NULL

#Binning base and total charges also

#Binning diff column
library(infotheo)
diff<-discretize(train_naive$diff,'equalfreq',nbins = 4)
train_naive$diff<-diff$X
train_naive$diff<-as.factor(train_naive$diff)

#Binning base charges
base<-discretize(train_naive$BaseCharges,'equalfreq',nbins = 4)
train_naive$BaseCharges<-base$X
train_naive$BaseCharges<-as.factor(train_naive$BaseCharges)

#Binning total charges
tot<-discretize(train_naive$TotalCharges,'equalfreq',nbins = 4)
train_naive$TotalCharges<-tot$X
train_naive$TotalCharges<-as.factor(train_naive$TotalCharges)

#Doing same with test
test_naive<-test_new
str(test_naive)

test_naive$DOC<-NULL
test_naive$DOE<-NULL

diff<-discretize(test_naive$diff,'equalfreq',nbins = 4)
test_naive$diff<-diff$X
test_naive$diff<-as.factor(test_naive$diff)

base<-discretize(test_naive$BaseCharges,'equalfreq',nbins = 4)
test_naive$BaseCharges<-base$X
test_naive$BaseCharges<-as.factor(test_naive$BaseCharges)

tot<-discretize(test_naive$TotalCharges,'equalfreq',nbins = 4)
test_naive$TotalCharges<-tot$X
test_naive$TotalCharges<-as.factor(test_naive$TotalCharges)

#implementing naive bayes model
h2o.init(nthreads = -1)

colnames(train_naive)
colnames(test_naive)
y.dep<-21
x.indep<-c(2:20,22)

train.h2o<-as.h2o(train_naive)
test.h2o<-as.h2o(test_naive)


naive_model3 <- h2o.naiveBayes(x=x.indep, y=y.dep, train.h2o)
predict.naive3 <- as.data.frame(h2o.predict(naive_model3, test.h2o))
improved_5 <- data.frame(CustomerID = test_new$CustomerID,Churn =  predict.naive3$predict)
write.csv(improved_5,file='improved_5.csv',row.names = F)
h2o.shutdown()
y

######################### MODEL 6
h2o.init(nthreads = -1)

colnames(train_naive)
colnames(test_naive)
y.dep<-21
x.indep<-c(2:20,22)

train.h2o<-as.h2o(train_naive)
test.h2o<-as.h2o(test_naive)

#implementing random forest model
rforest.model3 <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, mtries = 5, max_depth = 4, seed = 1127)
h2o.performance(rforest.model3)
h2o.varimp_plot(rforest.model3)
predict.rforest3 <- as.data.frame(h2o.predict(rforest.model3, test.h2o))
improved_6 <- data.frame(CustomerID = test_new$CustomerID,Churn =  predict.rforest3$predict)
write.csv(improved_6,file='improved_6.csv',row.names = F)
h2o.shutdown()
y

########################### MODEL 7
#Reinitializing dataframe
train_naive<-train_new
str(train_naive)

#Removing date columnms
train_naive$DOC<-NULL
train_naive$DOE<-NULL

#Making a new variable charge as difference of base and total charges
train_naive$Charge<-train_naive$TotalCharges-train_naive$BaseCharges

#Removing base and total charges
train_naive$BaseCharges<-NULL
train_naive$TotalCharges<-NULL

diff<-discretize(train_naive$diff,'equalfreq',nbins = 4)
train_naive$diff<-diff$X
train_naive$diff<-as.factor(train_naive$diff)

#Binning base charges
charge<-discretize(train_naive$Charge,'equalfreq',nbins = 4)
train_naive$Charge<-charge$X
train_naive$Charge<-as.factor(train_naive$Charge)

#Doing same for test
test_naive<-test_new
str(test_naive)

#Removing date columnms
test_naive$DOC<-NULL
test_naive$DOE<-NULL

#Making a new variable charge as difference of base and total charges
test_naive$Charge<-test_naive$TotalCharges-test_naive$BaseCharges

#Removing base and total charges
test_naive$BaseCharges<-NULL
test_naive$TotalCharges<-NULL

diff<-discretize(test_naive$diff,'equalfreq',nbins = 4)
test_naive$diff<-diff$X
test_naive$diff<-as.factor(test_naive$diff)

#Binning base charges
charge<-discretize(test_naive$Charge,'equalfreq',nbins = 4)
test_naive$Charge<-charge$X
test_naive$Charge<-as.factor(test_naive$Charge)

#implementing naive bayes model
h2o.init(nthreads = -1)

colnames(train_naive)
colnames(test_naive)
y.dep<-19
x.indep<-c(2:18,20,21)

train.h2o<-as.h2o(train_naive)
test.h2o<-as.h2o(test_naive)


naive_model4 <- h2o.naiveBayes(x=x.indep, y=y.dep, train.h2o)
predict.naive4 <- as.data.frame(h2o.predict(naive_model4, test.h2o))
improved_7 <- data.frame(CustomerID = test_new$CustomerID,Churn =  predict.naive4$predict)
write.csv(improved_7,file='improved_7.csv',row.names = F)
h2o.shutdown()
y

########################### MODEL 8
#random forest model
colnames(train_naive)
colnames(test_naive)
y.dep<-19
x.indep<-c(2:18,20,21)

train.h2o<-as.h2o(train_naive)
test.h2o<-as.h2o(test_naive)


rforest.model4 <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, mtries = 5, max_depth = 4, seed = 1127)
h2o.performance(rforest.model4)
h2o.varimp_plot(rforest.model4)
predict.rforest4 <- as.data.frame(h2o.predict(rforest.model4, test.h2o))
improved_8 <- data.frame(CustomerID = test_new$CustomerID,Churn =  predict.rforest4$predict)
write.csv(improved_8,file='improved_8.csv',row.names = F)
h2o.shutdown()
y

######################### MODEL 9
#naive nayes model
h2o.init(nthreads = -1)

colnames(train_naive)
colnames(test_naive)
y.dep<-19
x.indep<-c(3,4,8,10,12,14,15,18,20,21)

train.h2o<-as.h2o(train_naive)
test.h2o<-as.h2o(test_naive)


naive_model5 <- h2o.naiveBayes(x=x.indep, y=y.dep, train.h2o)
predict.naive5 <- as.data.frame(h2o.predict(naive_model5, test.h2o))
improved_9 <- data.frame(CustomerID = test_new$CustomerID,Churn =  predict.naive5$predict)
write.csv(improved_9,file='improved_9.csv',row.names = F)
h2o.shutdown()
y
