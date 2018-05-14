#Setting path
setwd('F:/PHD/Hack/Data')
getwd()

#Reading data
train_raw<-read.csv('train_complete.csv',na.strings=c(""," ","NA"))
test_raw<-read.csv('test_complete.csv',na.strings=c(""," ","NA"))


############### ANALYSIS ########################
str(train_raw)

#in TotalCharges
sum(is.na(train_raw$TotalCharges)) #10
View(train_raw[is.na(train_raw$TotalCharges),])

#in BaseCharges
sum(is.na(train_raw$BaseCharges))

#in Number Of Days
sum(is.na(train_raw$diff))

#in contract type
levels(train_raw$ContractType)
sum(is.na(train_raw$ContractType)) #5

#in education
train_raw$Education[train_raw$Education == '""']<-NA
levels(train_raw$Education)
sum(is.na(train_raw$Education)) #10
table(train_raw$Education)

#in gender
levels(train_raw$Gender)
sum(is.na(train_raw$Gender)) #4

str(train_raw)
str(test_raw)

#removing unused levels in test
lapply(test_raw, function(x){sum(is.na(x))})
levels(test_raw$Education)
levels(test_raw$Gender)
levels(test_raw$ContractType)

#Changing into factors
train_raw$Retired<-as.factor(train_raw$Retired)
train_raw$HasPartner<-as.factor(train_raw$HasPartner)
train_raw$HasDependents<-as.factor(train_raw$HasDependents)
train_raw$Phone<-as.factor(train_raw$Phone)
str(train_raw)

#in test
test_raw$Phone<-as.factor(test_raw$Phone)
test_raw$Retired<-as.factor(test_raw$Retired)
test_raw$HasPartner<-as.factor(test_raw$HasPartner)
test_raw$HasDependents<-as.factor(test_raw$HasDependents)
str(test_raw)

str(train_raw)
str(test_raw)

#making new variable by taking difference of base and total charges
train_raw$diffcharges<-train_raw$TotalCharges-train_raw$BaseCharges
test_raw$diffcharges<-test_raw$TotalCharges-test_raw$BaseCharges
train_raw$Churn

sum(is.na(train_raw))
sum(is.na(test_raw))

#################### Splitting ##############################
library(caTools)
set.seed(1000)
split = sample.split(train_raw$Churn, SplitRatio = 0.80)
train_rf = subset(train_raw, split==TRUE)
val_rf = subset(train_raw, split==FALSE)
val_result<-data.frame(val_rf$Churn)
val_rf$Churn<-NULL

############### MODEL VALIDATION ###############

library(h2o)
h2o.init(nthreads = -1)

colnames(train_rf)
y.dep<-1
x.indep<-c(2:10,14:23)

train.h2o<-as.h2o(train_rf)
test.h2o<-as.h2o(val_rf)


rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, mtries = 3,max_depth = 4, seed = 1122)
h2o.performance(rforest.model)
predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))
sub_rf <- data.frame(CustomerID = val_rf$CustomerID,Churn =  predict.rforest$predict)
write.csv(sub_rf,file='sub_rf.csv',row.names = F)
h2o.shutdown()
y
sub_rf$Churn
library(caret)
confusionMatrix(sub_rf$Churn,val_result$val_rf.Churn)
