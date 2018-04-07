library(csvread)
A=read.csv("C:/Users/josephpt/Desktop/New folder/bulb.csv")
View(A)
summary(A)
One_sample_result=t.test(A$Lifetime_Yrs,mu=15)   #mu is the average given in the test
One_sample_result
options(scipen=9999)   # to show exponential in normal form
One_sample_result=t.test(A$Lifetime_Yrs,mu=15)   #mu is the average given in the test
One_sample_result

#p<alpha we reject the null hypothesis.as we are rejecting null hypothesis and mean value is 9.3 we say value 9.3 is a significant value not 15
#lesser value is 8.639... max. value is 9.9602...
#95% of the test resulted within that range
#max. time we got the value 9.3
#?t.test= to read about t..test

result=read.csv("C:/Users/josephpt/Desktop/New folder/city_ratings.csv")
result
summary(result)
str(result)
result$city=as.factor(result$City)
str(result$city)
ans=t.test(result$Rating~result$city)
ans
#############################################################################

result=read.csv("C:/Users/josephpt/Desktop/New folder/kitkatsales_anova(1 way).csv")
result
summary(result)
?aggregate

aov1=aov(Sales~as.factor(Place),result) #do the analysis of varience
aov1
options(scipen=9999) 
summary(aov1) #gives the summary
#when we reject null hypothesis in Anova test we ahve to do Post Adhock test
####LSD  Lease squared deviation
####Tukey test
### Duncan test

#Tukey

TukeyHSD(aov1)
#################################################################
#chi sqr test (check)
##################################################################

X=read.csv("C:/Users/josephpt/Desktop/New folder/students_scores.csv")
X
str(X)
X$Before
class(X$Before)
str(X$Before)
Y=t.test(X$After,X$Before)
Y

H=read.csv("C:/Users/josephpt/Desktop/New folder/sprouts_test.csv")
H
H$Env=as.factor(H$Env)
I=t.test(H$Number~H$Env)
I



 



































