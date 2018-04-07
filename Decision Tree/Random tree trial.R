library(csvread)
credit<-read.csv("C:/Users/hp/Desktop/Analytics/Machine-Learning/credit.csv")
head(credit)
str(credit)
library(dplyr)
credit<-credit%>%filter(employment_length!="unemployed")%>%filter(job!="unempolyed non-resident")
str(credit)
set.seed(100)
credit_random<-credit[order(runif(938)),]
head(credit_random)
summary(credit$amount)
summary(credit_random$amount)
credit_train<-credit_random[(1:838),]
credit_test<-credit_random[(839:938),]
str(credit_test)
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
library(C50)
credit_model<-C5.0(credit_train[c(-15,-21)],as.factor(credit_train$default))
credit_model
summary(credit_model)
credit_predict<-predict(credit_model,credit_test)
credit_predict
credit_test$predict<-credit_predict
View(credit_test)
library(gmodels)
CrossTable(credit_test$default,credit_predict,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,
           dnn = c("ACTUAL DEFAULT","PREDICTED DEFAULT"))









