# Required Library 
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(csvread)

#Data using Credit Data
credit<-read.csv("C:/Users/hp/Desktop/Analytics/Machine-Learning/credit.csv")
head(credit)
str(credit)
table(credit$months_loan_duration)
table(credit$installment_rate)
credit$installment_rate<-as.factor(credit$installment_rate)
table(credit$residence_history)
credit$residence_history<-as.factor(credit$residence_history)
table(credit$existing_credits)
credit$existing_credits<-as.factor(credit$existing_credits)
credit$dependents<-as.factor(credit$dependents)
str(credit)
credit$default<-ifelse(credit$default==1,0,1)
head(credit)
#Data partition
set.seed(1000)
index<-sample(2,nrow(credit),replace = TRUE,prob = c(.7,.3))
head(index)
train<-credit[index==1,]
head(train)
str(train)
test<-credit[index==2,]
str(test)
head(test)
ncol(credit)
#Create Matrix-One-hot encoding for factor variables
#Factor variables will be converted to dummy variables.
#Numerical variable will be kept as it is
trainm<-sparse.model.matrix(default~.-1,data = train)
head(trainm)
trainm
train_label<-train[,"default"]
head(train_label)
train_matrix<-xgb.DMatrix(data=as.matrix(trainm),label=train_label)
train_matrix

testm<-sparse.model.matrix(default~.-1,data =test)
head(testm)
test_label<-test[,"default"]
test_matrix<-xgb.DMatrix(data = as.matrix(testm),label=test_label)
test_matrix
#Parameters
#nc-number of classes
nc<-length(unique(train_label))
nc
#xgb.parameters-xgb_parms
#eval_metric is evaluation matrix...different types of objectives are there
# we are using multi:softprob now.
xgb_parms<-list("objective"="multi:softprob",
                "eval_metric"="mlogloss",
                 "num_class"=nc)

watchlist<-list(train=train_matrix,test=test_matrix)
watchlist

#eXtreme Gradient Boosting Model
bst_model<-xgb.train(params = xgb_parms,
                     data=train_matrix,
                     nrounds = 500,
                     watchlist = watchlist,
                     eta=.01,
                     max.depth=3,
                     gamma=1)


#subsample=1
#colsample_bytree=1
#missing=NA
#seed=333
#Training and Test data error plot
e<-data.frame(bst_model$evaluation_log)
plot(e$iter,e$train_mlogloss,col='blue')
lines(e$iter,e$test_mlogloss,col='red')
min(e$test_mlogloss)
e[e$test_mlogloss==0.55703,]

#we have over fitting in the train and test data so we apply
#eta whcich ranges from 0 t0 1.
# small value of eta gives a model which is more good for over fitting

# Finding Important features(attributes)
imp<-xgb.importance(colnames(train_matrix),model = bst_model)
print(imp)
#ploting important features
xgb.plot.importance(imp)

#prediction and confusion matrix using test data
p<-predict(bst_model,newdata = test_matrix)
head(p)
#storing the predict value into a proper format
#ecause in predict we get probabiliy as a pair of p(0) and p(1)
library(dplyr)
pred<-matrix(p,nrow = nc,ncol = length(p)/nc) %>% t() %>%
      data.frame() %>% mutate(label=test_label,max_prob=max.col(.,"last")-1)
head(pred)
table(prediction=pred$max_prob,actual=pred$label)

#max.depth=6 where 6 is the default value, we have to chage the value up and down
#to see model accuracy change...and it can take value from 1 to infinity.
#gamma can take value from 0 to infinity, larger value of gamma makes more conservative algorithm
#if we see too much of over fitting we can increase the value of gamma and test it.
#subsample it can take value from 0 to 1.we can give lower value to prevent over fitting.
# if we give .5 then it means it will take 50% of the data to grow trees to avoid over fitting.
#missing=NA to avoid missing value.
#seed=333 is used to avoid randomisation and it will repeat the result
#











