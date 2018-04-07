Hour<-read.csv("C:/Users/hp/Desktop/Analytics/Important Codes/hour.csv")
head(Hour)
names(Hour)
summary(Hour)
str(Hour)
View(Hour)
library(ggplot2)
qplot(hr,cnt,data = Hour,color=season)
qplot(hr,cnt,data = Hour,geom = c("point","smooth")) #smmoth is used to smoot
#and "point" keeps the old points also.
#Histogram can be drawn with only one variable.
######Going to mpg data set#####################################################################
View(mpg)
qplot(displ,hwy,data = mpg,color=drv)
qplot(displ,hwy,data = mpg,color=drv,geom = c("point","smooth"))
qplot(displ,hwy,data = mpg,geom = c("point","smooth"))
qplot(hwy,data = mpg,fill=drv) ##Histogram with colour
#applying Facets
qplot(displ,hwy,data = mpg,facets = .~drv)
qplot(hwy,data = mpg,facets = drv~.,binwidth=2)
qplot(temp,data=Hour,geom = "density")  #Density Smoothing

qplot(temp,data=Hour,color=yr)

qplot(temp,hum,data=Hour,shape=season)
Hour$season<-as.factor(Hour$season)
Hour$yr<-as.factor(Hour$yr)
qplot(hr,cnt,data = Hour,color=season,shape=yr) #Giving shape to year wise category
qplot(hum,cnt,data = Hour,facets=.~season,color=season)
qplot(hum,cnt,data = Hour,facets=.~season,geom=c("point","smooth"),color=season)

####GGPLOT########
library(ggplot2)
g=ggplot(Hour,aes(temp,cnt))
p=g+geom_point()
p
p1=p+geom_smooth(method = "lm") #Linear smoothing is done
p1
p2=p+facet_grid(.~yr)+geom_smooth(method = "lm") #applying Facets based on Year
p2
# coloured the points with size=3 and less dark
p3<-g+geom_point(color="steelblue",size=3,alpha=1/2)
p3
# Giving colour based on Season
p4<-g+geom_point(aes(color=season),size=3,alpha=1/2)
p4

p4<-g+facet_grid(.~season)+geom_point(aes(color=season),size=3,alpha=1/2)
p4

# Naming the Graph and Axises
p5<-g+facet_grid(.~season)+geom_point(aes(color=season),size=3,alpha=1/2)+
    labs(title="Tem and Count")+labs(x="Temperature",y="Count")
p5
# Check how to name with expression
#p5<-g+facet_grid(.~season)+geom_point(aes(color=season),size=3,alpha=1/2)+
  labs(title="Tem and Count")+labs(x=expression("log"*PM[2.5]),y="Count")

#Applying Theme to our plot ( check more on Themes in Google)
p6<-g+facet_grid(.~season)+geom_point(aes(color=season))+theme_bw(base_family = "times")
p6











