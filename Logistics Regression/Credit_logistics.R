library(csvread)
credit<-read.csv("C:/Users/hp/Desktop/Analytics/Machine-Learning/credit.csv")
head(credit)
library(Deducer)
library(ResourceSelection)
library(gmodels)
library(Hmisc)
library(xtable)
library(data.table)
library(caret)
library(ROCR)
library(vif)
library(smbinning)
library(reshape2)
View(credit)
str(credit)
table(credit$months_loan_duration)
table(credit$installment_rate)
credit$installment_rate<-as.factor(credit$installment_rate)
table(credit$residence_history)
credit$residence_history<-as.factor(credit$residence_history)
table(credit$existing_credits)
credit$existing_credits<-as.factor(credit$existing_credits)
credit$dependents<-as.factor(credit$dependents)
credit$default<-as.factor(credit$default)
str(credit)
bins<-4
cutpoints<-quantile(credit[,"months_loan_duration"],(0:bins)/bins)
cutpoints
library(caret)
set.seed(1001)
index<-createDataPartition(credit$default,p=.7,list = FALSE)
head(index)
train<-credit[index,]
test<-credit[-index,]
head(test)
head(train)

credit_model<-glm(default~.,data = train,family = binomial(link='logit'))
summary(credit_model)
exp(credit_model$coefficients)
library(ResourceSelection)
hoslem.test(credit_model$y,fitted(credit_model))

pred_train<-predict(credit_model,train,type = "response")
pred_train

library(ROCR)
rocpred<-prediction(pred_train,train$default)
rocrperf<-performance(rocpred,"tpr","fpr")
plot(rocrperf,colorize=TRUE,print.cutoffs.at=seq(.1,by=.1))
# from ROC curve we take 0.3 as the optimum cutoff

pred<-predict(credit_model,test,type = "response")
pred
test$pred<-pred
head(test)
test$pred<-ifelse(test$pred>=.3,2,1)
head(test)
mytable<-table(test$default,test$pred)
mytable
library(gmodels)
confusionMatrix(test$default,test$pred)




