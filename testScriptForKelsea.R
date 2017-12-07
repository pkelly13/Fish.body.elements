#Perfom model fitting on fish body chemistry data - fit michaelis menten models with x axis as % maxiimum size and raw body size - check to see if parameters are significantly different
#Patrick Kelly 10 November 2017

library(mgcv)
library(bbmle)

#load data
setwd('~/Documents/Miami U/Downs_fishBodyChem')
data<-read.csv('HatcheryData_bodyChem.csv')

#remove NA rows
data<-data[!is.na(data$Weight..g.),]

#change % data to numeric
percent.adult<-c()
for(i in 1:nrow(data)){
	percent.adult[i]<-as.numeric(strsplit(data$X._Adult_Size,split='%')[[i]])
}

data$percent.adult<-percent.adult

#look at data
ggplot(data=data,aes(x=percent.adult,y=P.relmax))+geom_point()

mm<-function(a,b,sd,x,y){

	yhat<-a*x/(b+x)
	
	-sum(dnorm(y,yhat,sd=sd,log=T))
}



#go through each species and get parameters
spp<-unique(data$Species)
species<-data[data$Species==spp[10],]

m<-mle2(minuslogl=mm,start=list(a=1,b=1,sd=1),data=list(x=data$percent.adult,y=data$P.relmax))
summary(m)
confint(m)

parameters<-data.frame(species=spp,a=c(0.967053,1.0051841,0.939589,1.133128,0.954219,1.280238,1.089644,1.389305,0.637144,0.773940),b=c(2.14322,0.2419481,3.044602,1.547509,0.762552,2.864120,1.183053,5.865299,0.415926,0.234964),lower.a=c(0.898,0.952,0.865,0.9464,0.8877190,1.0984,0.9631009,1.129,0.5269,0.68375091),upper.a=c(1.051,1.0607,1.0215,1.3929,1.0265027,1.527,1.252,1.806,0.7863,0.8741526),lower.b=c(1.41,0.02654,1.6811,0.7477,0.4661555,1.903,0.7292,3.542,-0.01716,-0.0206),upper.b=c(3.235,0.4869,5.1326,2.8659,1.148,4.317,1.857,10.245,1.1938,0.6103))

x<-data$percent.adult
y<-(x*0.9449566)/(x+1.1664604)
fit.data<-data.frame(x=x,y=y)

ggplot(data=data,aes(x=percent.adult,y=P.relmax))+geom_point()+geom_line(data=fit.data,aes(x=x,y=y))


ggplot(data=parameters,aes(x=factor(species),y=a))+geom_point()+geom_point(data=parameters,aes(x=factor(species),y=lower.a),shape='-',size=7)+geom_point(data=parameters,aes(x=factor(species),y=upper.a),shape='-',size=7)

ggplot(data=parameters,aes(x=factor(species),y=b))+geom_point()+geom_point(data=parameters,aes(x=factor(species),y=lower.b),shape='-',size=7)+geom_point(data=parameters,aes(x=factor(species),y=upper.b),shape='-',size=7)