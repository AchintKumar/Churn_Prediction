################# Pattern Generation (Basic Association Rules)

str(train_raw)
pattern_raw<-train_raw
str(pattern_raw)
pattern_raw$BaseCharges<-NULL
pattern_raw$TotalCharges<-NULL

#important numerical columns
hist(pattern_raw$diff)
hist(pattern_raw$diffcharges)

library(infotheo)

#binning with equal frequency
diffbin<-discretize(pattern_raw$diff,"equalfreq",3)
pattern_raw$diffbin<-diffbin$X
hist(pattern_raw$diffbin)
pattern_raw$diffbin<-as.factor(pattern_raw$diffbin)

diffchargesbin<-discretize(pattern_raw$diffcharges,'equalfreq',3)
pattern_raw$diffbincharges<-diffchargesbin$X
hist(pattern_raw$diffbincharges)
pattern_raw$diffbincharges<-as.factor(pattern_raw$diffbincharges)

#removed the rest of numerical columns
pattern_raw$diff<-NULL
pattern_raw$diffcharges<-NULL
str(pattern_raw)

################### Association Rules
library(arules)
pattern_transactions<- as(pattern_raw, "transactions")
inspect(pattern_transactions)
pattern_rules<-apriori(pattern_transactions,parameter=list(support = 0.1, confidence = 0.5))

pattern_yes<-as(subset(pattern_rules,subset=rhs %in% "Churn=Yes"),'data.frame')
pattern_no<-as(subset(pattern_rules,subset=rhs %in% "Churn=No"),"data.frame")

View(pattern_yes)
rules<-subset(pattern_rules,subset = rhs %in% "Churn=Yes" & lift > 1)
rules

write.csv(pattern_yes,"apriori_yes.csv")
write.csv(pattern_no,"apriori_no.csv")

###################### C5.0
library(C50)
model_c50<-C5.0(Churn~.,data = pattern_raw, rules= TRUE)
summary(model_c50)
model_c50$tree

write(capture.output(summary(model_c50)),"c50model.txt")
