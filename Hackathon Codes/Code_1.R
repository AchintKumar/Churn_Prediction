#Setting path
setwd('F:/PHD/Hack/Data')
getwd()

#Reading data
train_service<-read.csv('Train_ServicesOptedFor.csv')
train_pred<-read.csv('Train.csv')
train_accountinfo<-read.csv('Train_AccountInfo.csv')
train_Demo<-read.csv('Train_Demographics.csv')

############# AGGREGATION ############################
#Making new dataframe
train_new<-c(1:5298)
train_new<-as.data.frame(train_new)
train_new$CustomerID<-train_pred$CustomerID
train_new$train_new<-NULL

#Making columns for each service type in new data frame
train_new$OnlineBackup<-train_service[train_service$TypeOfService=='OnlineBackup',]
train_new$StreamingMovies<-train_service[train_service$TypeOfService=='StreamingMovies',]
train_new$InternetServiceCategory<-train_service[train_service$TypeOfService=='InternetServiceCategory',]
train_new$HasPhoneService<-train_service[train_service$TypeOfService=='HasPhoneService',]
train_new$TechnicalSupport<-train_service[train_service$TypeOfService=='TechnicalSupport',]
train_new$OnlineSecurity<-train_service[train_service$TypeOfService=='OnlineSecurity',]
train_new$StreamingTelevision<-train_service[train_service$TypeOfService=='StreamingTelevision',]
train_new$DeviceProtection<-train_service[train_service$TypeOfService=='DeviceProtection',]
train_new$MultipleLines<-train_service[train_service$TypeOfService=='MultipleLines',]

#For loop giving error,so doing manually

#copying into new dataframe
a<-train_new$StreamingMovies
#removing customerid and type
a$CustomerID<-NULL
a$TypeOfService<-NULL
#adding detail of service in prediction frame
train_pred$StreamMovies<-a$SeviceDetails

#doing for rest of the columns
a<-train_new$InternetServiceCategory
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$Internet<-a$SeviceDetails

a<-train_new$HasPhoneService
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$Phone<-a$SeviceDetails

a<-train_new$TechnicalSupport
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$TechSupport<-a$SeviceDetails

a<-train_new$OnlineSecurity
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$OnlineSecurity<-a$SeviceDetails

a<-train_new$StreamingTelevision
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$Tele<-a$SeviceDetails

a<-train_new$DeviceProtection
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$DeviceProtect<-a$SeviceDetails

a<-train_new$MultipleLines
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$MultiLines<-a$SeviceDetails

a<-train_new$OnlineBackup
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$OnlineBackup<-a$SeviceDetails

rm(a)

#binding to make actual train set
train_raw<-cbind(train_pred,train_accountinfo,train_Demo)
train_raw$CustomerID<-NULL
train_raw$HouseholdID<-NULL

rm(train_accountinfo,train_Demo,train_new,train_pred,train_service)

#doing the same for test set

#Reading data
train_service<-read.csv('Test_ServicesOptedFor.csv')
train_pred<-read.csv('Test.csv')
train_accountinfo<-read.csv('Test_AccountInfo.csv')
train_Demo<-read.csv('Test_Demographics.csv')

#Making new dataframe
train_new<-c(1:1769)
train_new<-as.data.frame(train_new)
train_new$CustomerID<-train_pred$CustomerID
train_new$train_new<-NULL

#Making columns for each service type in new data frame
train_new$OnlineBackup<-train_service[train_service$TypeOfService=='OnlineBackup',]
train_new$StreamingMovies<-train_service[train_service$TypeOfService=='StreamingMovies',]
train_new$InternetServiceCategory<-train_service[train_service$TypeOfService=='InternetServiceCategory',]
train_new$HasPhoneService<-train_service[train_service$TypeOfService=='HasPhoneService',]
train_new$TechnicalSupport<-train_service[train_service$TypeOfService=='TechnicalSupport',]
train_new$OnlineSecurity<-train_service[train_service$TypeOfService=='OnlineSecurity',]
train_new$StreamingTelevision<-train_service[train_service$TypeOfService=='StreamingTelevision',]
train_new$DeviceProtection<-train_service[train_service$TypeOfService=='DeviceProtection',]
train_new$MultipleLines<-train_service[train_service$TypeOfService=='MultipleLines',]

#For loop giving error,so doing manually

#copying into new dataframe
a<-train_new$StreamingMovies
#removing customerid and type
a$CustomerID<-NULL
a$TypeOfService<-NULL
#adding detail of service in prediction frame
train_pred$StreamMovies<-a$SeviceDetails

#doing for rest of the columns
a<-train_new$InternetServiceCategory
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$Internet<-a$SeviceDetails

a<-train_new$HasPhoneService
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$Phone<-a$SeviceDetails

a<-train_new$TechnicalSupport
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$TechSupport<-a$SeviceDetails

a<-train_new$OnlineSecurity
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$OnlineSecurity<-a$SeviceDetails

a<-train_new$StreamingTelevision
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$Tele<-a$SeviceDetails

a<-train_new$DeviceProtection
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$DeviceProtect<-a$SeviceDetails

a<-train_new$MultipleLines
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$MultiLines<-a$SeviceDetails

a<-train_new$OnlineBackup
a$CustomerID<-NULL
a$TypeOfService<-NULL
train_pred$OnlineBackup<-a$SeviceDetails

rm(a)

#binding to make actual test set
test_raw<-cbind(train_pred,train_accountinfo,train_Demo)
test_raw$CustomerID<-NULL
test_raw$HouseholdID<-NULL

rm(train_accountinfo,train_Demo,train_new,train_pred,train_service)

############# ANALYSIS #################

summary(train_raw)

#Making new variable number of days between original date and recorded date 
train_raw$DOC<-as.Date(train_raw$DOC, format="%d/%m/%Y")
train_raw$DOE<-as.Date(as.character(train_raw$DOE), format='%d-%b-%y')
str(train_raw$DOE)
str(train_raw$DOC)
train_raw$diff<-train_raw$DOC-train_raw$DOE
train_raw$diff<-as.numeric(train_raw$diff)
str(train_raw)
#in test
test_raw$DOC<-as.Date(test_raw$DOC, format="%d/%m/%Y")
test_raw$DOE<-as.Date(as.character(test_raw$DOE), format='%d-%b-%y')
str(test_raw$DOE)
str(test_raw$DOC)
test_raw$diff<-test_raw$DOC-test_raw$DOE
test_raw$diff<-as.numeric(test_raw$diff)

#removing irrelevant date values
train_raw$DOC<-NULL
test_raw$DOC<-NULL
train_raw$DOE<-NULL
test_raw$DOE<-NULL

#checking structure
str(test_raw)
str(train_raw)

#Country has no variance
table(train_raw$Country)
train_raw$Country<-NULL
table(test_raw$Country)
test_raw$Country<-NULL

#State has no variance
table(train_raw$State)
train_raw$State<-NULL
table(test_raw$State)
test_raw$State<-NULL

# Removing all unused levels in factors
train_raw<-lapply(train_raw, function(x) if(is.factor(x)) factor(x) else x)
train_raw<-as.data.frame(train_raw)
str(train_raw)
test_raw<-lapply(test_raw, function(x) if(is.factor(x)) factor(x) else x)
test_raw<-as.data.frame(test_raw)
str(test_raw)

#StreamMovies
train_raw$StreamMovies
train_raw$Internet

#TotalCharges
train_raw$TotalCharges<-as.character(train_raw$TotalCharges)
train_raw$TotalCharges<-as.numeric(train_raw$TotalCharges)
test_raw$TotalCharges<-as.character(test_raw$TotalCharges)
test_raw$TotalCharges<-as.numeric(test_raw$TotalCharges)

#checking class imbalance
prop.table(table(train_raw$Churn))
View(train_raw)
prop.table(table(train_raw$Retired))
prop.table(table(train_raw$HasPartner))
prop.table(table(train_raw$HasDependents))

############# VISUALIZATION #######################
install.packages('JGR')
library(JGR)
JGR()

write.csv(train_raw,file='train_complete.csv',row.names = F)
write.csv(test_raw,file='test_complete.csv',row.names = F)




############# BASE MODEL #####################
library(h2o)
h2o.init(nthreads = -1)

y.dep<-1
x.indep<-c(2:10,11:26)

train.h2o<-as.h2o(train_raw)
test.h2o<-as.h2o(test_raw)


rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, mtries = 3, max_depth = 4, seed = 1122)
h2o.performance(rforest.model)
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
sub_rf <- data.frame(CustomerID = test_raw$CustomerID,Churn =  predict.rforest$predict)
write.csv(sub_rf,file='sub_rf.csv',row.names = F)
h2o.shutdown()

