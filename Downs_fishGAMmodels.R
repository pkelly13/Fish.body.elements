#Data anlysis for K. Downs looking at relationships between age and fish body chemistry, focusing on C, N, and P as a proportion of maximum C, N, and P. Thought is to use GAMs, similar to Boros et al., but do it in a way where we can see differences in fish species patterns across weeks. Boros used ANOCOVA
#Patrick Kelly last edit: 21 July 2017

rm(list=ls())
setwd('~')
source('.Rprofile')

#load data
setwd('~/Documents/Miami U/Downs_fishBodyChem')
data<-read.csv('HatcheryData_bodyChem.csv')

#remove NA rows
data<-data[!is.na(data$Weight..g.),]

library(mgcv) #load package for gams

#exclude fathead minnow for now
#data<-data[data$Species!='Fathead Minnow',]

#models will be in the form of fish ~ week + week*species -- need to make species an ordered factor
data$OFspecies<-as.factor(data$Species)
data$OFspecies<-as.ordered(data$OFspecies)

data$Hatchery<-as.factor(data$Hatchery)

#run model for carbon composition
c.mod<-gam(C.relmax~s(Week) + s(Week,by=OFspecies)+s(Hatchery,bs='re'),data=data)
#c.mod<-gam(C.relmax~s(Week,by=OFspecies)+s(Hatchery,bs='re'),data=data)

summary(c.mod)
plot(c.mod)

#blgl<-gam(C.relmax~s(Week),data=data[data$Species=='Walleye',])

#Now try this with genus instead of species
#make genus an ordered factor
data$OFgenus<-as.factor(data$Genus)
data$OFgenus<-as.ordered(data$OFgenus)

c.genus.mod<-gam(C.relmax~s(Week)+s(Week,by=OFgenus)+s(Hatchery,bs='re'),data=data)
summary(c.genus.mod)
plot(c.genus.mod)

#model for N
n.mod<-gam(N.relmax~s(Week)+s(Week,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(n.mod)
plot(n.mod)

#model for P
p.mod<-gam(P.relmax~s(Week)+s(Week,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(p.mod)
plot(p.mod)

#model for Ca
Ca.mod<-gam(Ca.relmax~s(Week)+s(Week,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(Ca.mod)

#Look at similar results in an ANCOVA -- results are about the same
mod<-lm(P.relmax~Week+factor(Hatchery)+Week*factor(Species),data=data)

#Now make some figures from the GAM models
#start with looking at the data, fitting spline models to each species
setwd('~/Documents/Miami U/Downs_fishBodyChem/Figures')
jpeg('FishCrelmax.jpeg',height=1200,width=1200)
ggplot(data=data[data$Species!='Fathead Minnow',],aes(x=Week,y=C.relmax,color=factor(Species)))+geom_point(size=2)+geom_smooth()+theme_bw()+theme(text=element_text(size=25))+ylab('Body Relative %C')+xlab('Week')
dev.off()

jpeg('FishNrelmax.jpeg',height=1200,width=1200)
ggplot(data=data,aes(x=Week,y=N.relmax,color=factor(Hatchery)))+geom_point(size=2)+geom_smooth(color='dark grey')+facet_wrap(~Species,scales='free')+theme_bw()+theme(text=element_text(size=25))+ylab('Body Relative %N')+xlab('Week')
dev.off()

jpeg('FishPrelmax.jpeg',height=1200,width=1200)
ggplot(data=data,aes(x=Week,y=P.relmax,color=factor(Species)))+geom_point(size=2)+geom_smooth(se=F)+theme_bw()+theme(text=element_text(size=25))+ylab('Body Relative %P')+xlab('Week')
dev.off()

#make figures (partial regression plots) for the smooth predictors for each species
setwd('~/Documents/Miami U/Downs_fishBodyChem/Figures')
jpeg('FishCrelmax_partialRegression.jpeg',height=1000,width=1000)
par(mfrow=c(3,4),mar=c(6,6,3,1))
plot.gam(c.mod,select=2,ylim=c(-0.5,0.5),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Brown Trout',cex.main=2.5)
plot.gam(c.mod,select=3,ylim=c(-0.5,0.5),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Catfish',cex.main=2.5)
plot.gam(c.mod,select=4,shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Fathead Minnow',cex.main=2.5)
plot.gam(c.mod,select=5,ylim=c(-5,5),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Largemouth Bass',cex.main=2.5)
plot.gam(c.mod,select=6,ylim=c(-0.5,0.5),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Muskellunge',cex.main=2.5)
plot.gam(c.mod,select=7,ylim=c(-0.5,0.5),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Rainbow Trout',cex.main=2.5)
plot.gam(c.mod,select=8,ylim=c(-1,1),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Saugeye',cex.main=2.5)
plot.gam(c.mod,select=9,ylim=c(-1,1),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='St. Mary Catfish',cex.main=2.5)
plot.gam(c.mod,select=10,ylim=c(-1,1),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Walleye',cex.main=2.5)
plot.gam(c.mod,select=11,ylim=c(-2,2),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Yellow Perch',cex.main=2.5)
mtext('Week',side=1,outer=T,cex=2.5,line=-1)
mtext('s(Week):Species Relative %C',side=2,outer=T,cex=2.5,line=-2.6)
dev.off()


setwd('~/Documents/Miami U/Downs_fishBodyChem/Figures')
jpeg('FishNrelmax_partialRegression.jpeg',height=1000,width=1000)
par(mfrow=c(3,4),mar=c(6,6,3,1))
plot.gam(n.mod,select=2,ylim=c(-0.5,0.5),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Brown Trout',cex.main=2.5)
plot.gam(n.mod,select=3,ylim=c(-1,1),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Catfish',cex.main=2.5)
plot.gam(n.mod,select=4,shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Fathead Minnow',cex.main=2.5)
plot.gam(n.mod,select=5,ylim=c(-5,5),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Largemouth Bass',cex.main=2.5)
plot.gam(n.mod,select=6,ylim=c(-1,1),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Muskellunge',cex.main=2.5)
plot.gam(n.mod,select=7,ylim=c(-0.5,0.5),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Rainbow Trout',cex.main=2.5)
plot.gam(n.mod,select=8,shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Saugeye',cex.main=2.5)
plot.gam(n.mod,select=9,ylim=c(-1,1),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='St. Mary Catfish',cex.main=2.5)
plot.gam(n.mod,select=10,ylim=c(-1,1),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Walleye',cex.main=2.5)
plot.gam(n.mod,select=11,ylim=c(-2,2),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Yellow Perch',cex.main=2.5)
mtext('Week',side=1,outer=T,cex=2.5,line=-1)
mtext('s(Week):Species Relative %N',side=2,outer=T,cex=2.5,line=-2.6)
dev.off()

setwd('~/Documents/Miami U/Downs_fishBodyChem/Figures')
jpeg('FishPrelmax_partialRegression.jpeg',height=1000,width=1000)
par(mfrow=c(3,4),mar=c(6,6,3,1))
plot.gam(p.mod,select=2,ylim=c(-20,20),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Brown Trout',cex.main=2.5)
plot.gam(p.mod,select=3,ylim=c(-5,5),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Catfish',cex.main=2.5)
plot.gam(p.mod,select=4,ylim=c(-2,2),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Fathead Minnow',cex.main=2.5)
plot.gam(p.mod,select=5,ylim=c(-5,5),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Largemouth Bass',cex.main=2.5)
plot.gam(p.mod,select=6,shade=T,ylim=c(-2,2),lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Muskellunge',cex.main=2.5)
plot.gam(p.mod,select=7,shade=T,ylim=c(-3,3),lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Rainbow Trout',cex.main=2.5)
plot.gam(p.mod,select=8,ylim=c(-1,1),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Saugeye',cex.main=2.5)
plot.gam(p.mod,select=9,ylim=c(-2,2),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='St. Mary Catfish',cex.main=2.5)
plot.gam(p.mod,select=10,ylim=c(-1,1),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Walleye',cex.main=2.5)
plot.gam(c.mod,select=11,ylim=c(-2,2),shade=T,lwd=2.5,cex.axis=2.2,xlab='',ylab='',main='Yellow Perch',cex.main=2.5)
mtext('Week',side=1,outer=T,cex=2.5,line=-1)
mtext('s(Week):Species Relative %P',side=2,outer=T,cex=2.5,line=-2.6)
dev.off()


#Now look at some models for C:N, C:P, and N:P
cn.mod<-gam(C.N~s(Week) + s(Week,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(cn.mod)

cp.mod<-gam(C.P~s(Week) + s(Week,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(cp.mod)

np.mod<-gam(N.P~s(Week)+s(Week,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(np.mod)

cap.mod<-gam(Ca.P~s(Week)+s(Week,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(cap.mod)


#Now look at all of these different things as a function of percent of adult mass
#remove NA rows

#change % data to numeric
percent.adult<-c()
for(i in 1:nrow(data)){
	percent.adult[i]<-as.numeric(strsplit(data$X._Adult_Size,split='%')[[i]])
}

data$percent.adult<-percent.adult

#models
c.pct.mod<-gam(C.relmax~s(percent.adult)+s(percent.adult,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(c.pct.mod)
plot(c.pct.mod)

#model for N
n.pct.mod<-gam(N.relmax~s(percent.adult)+s(percent.adult,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(n.pct.mod)
#plot(n.pct.mod)

#model for P
p.pct.mod<-gam(P.relmax~s(percent.adult)+s(percent.adult,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(p.pct.mod)
#plot(p.pct.mod)

#model for RNA


#model for Ca
Ca.pct.mod<-gam(Ca.relmax~s(percent.adult)+s(percent.adult,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(Ca.pct.mod)

#Now look at some models for C:N, C:P, and N:P
cn.pct.mod<-gam(C.N~s(percent.adult) + s(percent.adult,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(cn.pct.mod)

cp.pct.mod<-gam(C.P~s(percent.adult) + s(percent.adult,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(cp.pct.mod)

np.pct.mod<-gam(N.P~s(percent.adult)+s(percent.adult,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(np.pct.mod)

cap.pct.mod<-gam(Ca.P~s(percent.adult)+s(percent.adult,by=OFspecies)+s(Hatchery,bs='re'),data=data)
summary(cap.pct.mod)


#Look at nmle michaelis menten models of fish %P relmax by % of adult size
#load lme4 package
#install.packages('lme4')
#install.packages('RLRsim')

#load packages
library(lme4)
library(RLRsim)


d<-groupedData(P.relmax~percent.adult|Species,data=data)
#creat model without species as a random effect
mod.nospecies<-nls(P.relmax~SSmicmen(percent.adult,Vm,K),data=d,start=c(Vm=0.944,K=1.166))

AIC(mod.nospecies) #-173.5342

#add species to the model
mod.species<-nlme(P.relmax~SSmicmen(percent.adult,Vm,K),data=d,fixed=(Vm+K~1),random=Vm+K~1|Species,start=c(Vm=0.944,K=1.166)) #AIC = -276.3796
summary(mod.species)

anova(mod.species,mod.nospecies)

library(lmtest)
lrtest(mod.nospecies,mod.species)