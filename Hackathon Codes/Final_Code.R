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

################ MODEL 
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
