install.packages("data.table")
install.packages("smbinning")
install.packages("xtable")
install.packages("Hmisc")
library(Deducer)
library(ResourceSelection)
library(gmodels)
library(xtable)
library(Hmisc)

library(data.table)
library(caret)
library(VIF)
library(ROCR)
library(smbinning)
library(reshape2)

#########Reading and convering dat file ######
readLines("C:/Users/hp/Desktop/Analytics/04-02-2018/adult/adult.dat",n=25)  # checking the lines upto 25 to see the details and data.
adult<-read.table("C:/Users/hp/Desktop/Analytics/04-02-2018/adult/adult.dat",header = FALSE, skip =19,sep = ",")
adult
names<-c("Age","Workclass","Fnlwgt","Education","Education-num","Marital-status","Occupation","Relationship","Race","Sex","Capital-gain","Capital-loss","Hours-per-week","Native-country","class")
write.csv(adult,"C:/Users/hp/Desktop/Analytics/04-02-2018/adult/adult.csv",row.names = FALSE)
colnames(adult)<-names
head(adult)
rawData <- adult
adult <- rawData

## Checking how many complete.cases we have

incompleteRows <- nrow(adult[!complete.cases(adult) , ])
totalMissing <- incompleteRows/nrow(adult)*100

nrow(adult)
print(incompleteRows)
print(totalMissing)

# Running chi-Square for "Workclass" variables
levels(adult$`Workclass `)
table(adult$`Workclass `, useNA = "ifany")

mytable <- table(adult$Class, adult$`Workclass `)
chisq.test(mytable)

# Running chi-Square for "Native-country" variables
levels(adult$Native-country)
table(adult$`Native-country` , useNA = "ifany")

mytable <- table(adult$Class, adult$`Workclass `)
chisq.test(mytable)

## Clubbing Different factor levels in one group
adult[, "Workclass "] <- as.factor(ifelse(adult[,"Workclass "] == "Private","Private_Job","Other" ))
adult[, "Native-country"] <- as.factor(ifelse(adult[,"Native-country"] == "United-States","UnitedStates","OtherCountries" ))

str(adult)

# After closely looking the variable checking the histogram and quntiles.
# I decided to convert this variable into three different categories

# Checking distribution based on bins

bins<-4
cutpoints<-quantile(adult[, "Hours-per-week "],(0:bins)/bins)
#binned <-cut(adult[, "Hours-per-week "],cutpoints,include.lowest=TRUE)
#summary(binned)

# Using describe function
describe(adult[, "Hours-per-week "])

# Building histogram
hist(adult[, "Hours-per-week "])

# Converting "Hours-per-week" into categories with three different levels.
adult[,"Hours-per-week "] <- as.factor(ifelse(adult[,"Hours-per-week "] <= 35, "Under",
                                    ifelse(adult[,"Hours-per-week "] == 40, "OnTime","Over")))



## Running Logistic Model with the above three variables

newData <- adult[, c("Hours-per-week ", "Workclass ", "Native-country", "Class")]

library(caret)
index <- createDataPartition(newData$Class , p =0.7,list = FALSE)
Train <- newData[index,]
Test <- newData[-index,]

## # Building my first model. including all the variables
adult_model <- glm(Class ~ . , data = Train[1:1000,] , family=binomial(link='logit'))
summary(adult_model)
exp(adult_model$cofficients)
 
library(ResourceSelection)
hoslem.test(adult_model$y, fitted(adult_model))

# Run the function code first
bruteforce(adult_model)

## Using rocplot function to draw the roc curv
library(Deducer)
rocplot(adult_model)


# Validating Training Results
# Looking at classificaton table
Train$rankP <- predict(adult_model, newdata = Train, type = "response")
Train$rankV <- ifelse(Train$rankP >= 0.23 ,1,0)
mytable<- table(Train$Class, Train$rankV)

# Looking at auc and ROC curve
## Using functions from AUC package to calculate auc and draw ROC curve
library(ROCR)
pr <- prediction(Train$rankP, Train$Class)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Validating Testing Results
Test$rankP <- predict(adult_model, newdata = Test, type = "response")
Test$rankV <- ifelse(Test$rankP >= 0.23 ,1,0)
mytable<- table(Test$Class, Test$rankV)

library(ROCR)
pr <- prediction(Test$rankP, Test$Class)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

