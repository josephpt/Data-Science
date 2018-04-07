library(csvread)
data<-read.csv("C:/Users/hp/Desktop/Analytics/Machine-Learning/wisc_bc_data.csv")
head(data)
str(data)
table(data$diagnosis)
View(data)
data1<-data[,-1]
names(data1)
set.seed(1001)
index<-sample(2,nrow(data1),replace = TRUE,
              prob = c(.8,.2))
train<-data1[index==1,]
head(train)
test<-data1[index==2,]
str(test)
#scatter plot and correlations
library(psych)
#pairs.panels(train[,-1],gap=0,
             bg=c("red","blue")[train$diagnosis],
             pch=21)
#bg is to colour the types of result in diagnosis
#pch is to give the type of symbol we want.

#as the graph is difficult to read due to lot of variables let us try for few variables.
pairs.panels(train[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)],gap=0,
             bg=c("red","blue")[train$diagnosis],
             pch=21)
# radius mean, are mean all are having high value of correlation so there is multi collinearity

#Principle component Analysis
# centre is to conver the variables in such a way tat the average become zero
#scale is to scale the value or to normalize the values

pc<-prcomp(train[,-1],
           center = TRUE,
           scale. = TRUE)

attributes(pc)  #attributes stored in pc
options(scipen = 9999)
pc$center   # shows mean
pc$scale    # shows the standard deviation
print(pc)
summary(pc)

#Orthogonality of PCs
pairs.panels(pc$x,gap=0,
             bg=c("red","blue"[train$diagnosis]))
cor(pc$x)

#BI plot
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
g<-ggbiplot(pc,
            obs.scale = 1,
            var.scale = 1,
            groups = train$diagnosis,
            ellipse = TRUE,
            circle = TRUE,
            ellipse.prob = 0.75)
#ellipse.prob captures the probability within the given range
g<-g+scale_color_discrete(name='')
g<-g+theme(legend.direction = 'horizontal',
           legend.position = 'top')
print(g)
#note:if arrows of variables in bi plot are closer then they have high correlation
#length of arrows are based on principle component values which we got from print(pc)

#Prediction with principle components
train_pred<-predict(pc,train)
train_pred<-data.frame(train_pred,train[1])
head(train_pred)
test_pred<-predict(pc,test)
test_pred<-data.frame(test_pred,test[1])
class(train_pred)
names(train_pred)

#######Random forest using PCs........
#finding the number of predictor variable using tuneRF
#step factor is to mention the step after each iteration
#improve=0.1 means the iteration stops when it reaches an improvemnet factor of 0.1
library(randomForest)
#Taking PCs 1:18 as those are enough to predict the complete behaviour
bestmtry<-tuneRF(train_pred[,1:18],train_pred$diagnosis,stepFactor = 1.2,improve = 0.01,
                 trace = TRUE,plot = TRUE)
diag_forest<-randomForest(diagnosis~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+
                            PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18,data=train_pred)
diag_forest

pred_test<-predict(diag_forest,newdata = test_pred)

pred_test
library(caret)
confusionMatrix(pred_test,test_pred$diagnosis)


