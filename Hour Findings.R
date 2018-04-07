Hour<-read.csv("C:/Users/hp/Desktop/Analytics/Important Codes/hour.csv")
head(Hour)
names(Hour)
summary(Hour)
str(Hour)
View(Hour)
library(ggplot2)
p1<-ggplot(Hour,aes(hr,cnt))+geom_point(color="red")+geom_smooth()
p1
p2<-ggplot(Hour,aes(hr,cnt))+geom_point(color="red")+facet_grid(.~yr)+geom_smooth()
p2
p3<-ggplot(Hour,aes(hr,cnt))+geom_point(aes(color=season))+facet_grid(.~yr)+geom_smooth()
p3

p4<-ggplot(Hour,aes(yr,cnt))+geom_boxplot(fill="light green")                                        
p4
# month wise count is similar
p5<-ggplot(Hour,aes(mnth,cnt))+geom_bar(aes(fill=season))  # no data mixed according to season
p5

p6<-ggplot(Hour,aes(mnth,cnt))+facet_grid(.~yr)+geom_point(aes(color=season))
p6

p7<-ggplot(Hour,aes(mnth,temp))+geom_point(aes(color=season))
p7

Hour$holiday<-as.factor(Hour$holiday)
Hour$weekday<-as.factor(Hour$weekday)

p8<-ggplot(Hour,aes(weekday))+geom_histogram(stat = "count",aes(fill=holiday))
p8 # not a day is shown as holiday

p9<-ggplot(Hour,aes(atemp,hum))+geom_point()+geom_smooth(method="lm")
p9

p10<-ggplot(Hour,aes(windspeed,cnt))+geom_point(aes(color=season))
p10








