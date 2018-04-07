library(csvread)
credit<-read.csv("C:/Users/hp/Desktop/Analytics/Machine-Learning/credit.csv")
head(credit)
names(credit)
View(credit)
library(ggplot2)
p1<-ggplot(credit,aes(age))+geom_histogram()
p1
library(dplyr)
a<-arrange(credit,age)
a
View(a)
credit$age<-ifelse(credit$age>=19 & credit$age<35,"young",ifelse(credit$age>35 & credit$age<=55,"middle","old"))
head(credit)                    
table(credit$age)
credit$age<-as.factor(credit$age)
credit$default<-as.factor(credit$default)
credit$credit_history<-as.factor(credit$credit_history)
#Credit history based on age
p2<-ggplot(credit,aes(age,amount))+facet_grid(.~default)+geom_point(aes(color=default))
p2

table(credit$employment_length)
credit$employment_length<-as.factor(credit$employment_length)
# credit history based on employment_length
p3<-ggplot(credit,aes(employment_length,amount))+facet_wrap(~credit_history,ncol = 2)+geom_point(aes(color=default))
p3
p13<-ggplot(credit,aes(employment_length))+geom_histogram(stat = "count",aes(fill=default))
p13

# Distribution of defaulters and non defaulters are of same kind
p4<-ggplot(credit,aes(amount))+geom_histogram(aes(color=default,fill=default))
p4
summary(credit$amount)

# Box plot based on age

p5<-ggplot(credit,aes(x=age,y=amount))+geom_boxplot(fill=I("light green"))
p5

credit$personal_status<-as.factor(credit$personal_status)
# loan amount based on personal_status
p6<-ggplot(credit,aes(x=personal_status,y=amount))+geom_boxplot(fill=I("orange"))
p6

p7<-p4<-ggplot(credit,aes(credit_history,amount))+facet_wrap(~personal_status)+geom_point(aes(color=personal_status))
p7

p14<-p4<-ggplot(credit,aes(credit_history,amount))+geom_point(aes(color=personal_status))
p14


table(credit$savings_balance)
credit$savings_balance<-as.factor(credit$savings_balance)

# max people are of category with less than 100dm amount and they are the main defaulters
p8<-ggplot(credit,aes(savings_balance))+geom_histogram(stat="count",aes(fill=default))
p8

table(credit$housing)
credit$housing<-as.factor(credit$housing)

p9<-p4<-ggplot(credit,aes(savings_balance,amount))+facet_wrap(~housing)+geom_point(aes(color=default))
p9

table(credit$job)

p10<-ggplot(credit,aes(job,amount))+geom_boxplot(fill="skyblue")
p10

p11<-ggplot(credit,aes(job))+geom_histogram(stat = "count",aes(fill=default))
p11

credit$telephone<-as.factor(credit$telephone)

p12<-ggplot(credit,aes(job))+facet_grid(.~telephone)+geom_histogram(stat = "count",aes(fill=default))
p12





###########################################################################














