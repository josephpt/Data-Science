library(csvread)
A=read.csv("C:/Users/hp/Desktop/Analytics/datasetsforhypothesistopics/bulb.csv")
A
summary(A)
View(A)
Test=t.test(A$Lifetime_Yrs,mu=15)
Test
options(scipen = 9999)
set.seed(10)
Test=t.test(A$Lifetime_Yrs,mu=15)
Test
?t.test
Cities=read.csv("C:/Users/hp/Desktop/Analytics/datasetsforhypothesistopics/city_ratings.csv")
summary(Cities)
View(Cities)
Cities$City=as.factor(Cities$City)
summary(Cities)
Result=t.test(Cities$Rating~Cities$City)
# or Result=t.test(Cities$Rating~as.factor(Cities$City)
Result

Kitkat=read.csv("C:/Users/hp/Desktop/Analytics/datasetsforhypothesistopics/kitkatsales_anova.csv")
View(Kitkat)
result=aov(Kitkat$Sales~as.factor(Kitkat$Place),data = Kitkat)
result=aov(Sales~as.factor(Place),data = Kitkat)
result
summary(result)
TukeyHSD(result)
########Paired T test ###########
test=read.csv("C:/Users/hp/Desktop/Analytics/datasetsforhypothesistopics/students_scores.csv")
head(test)
result=t.test(test$Before,test$After,paired = TRUE)
result

sprouts=read.csv("C:/Users/hp/Desktop/Analytics/datasetsforhypothesistopics/sprouts_test.csv")
View(sprouts)
result1=t.test(sprouts$Number~as.factor(sprouts$Env))
result1





























