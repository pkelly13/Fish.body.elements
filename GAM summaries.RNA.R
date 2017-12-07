#data.rna anlysis for K. Downs looking at relationships between age and fish RNA content. 
#Patrick Kelly last edit: 3 November 2017

#load data.rna
setwd('~/Documents/Miami U/Downs_fishBodyChem')
data.rna<-read.csv('HatcheryData_RNA.csv')

#change RNA column header
colnames(data.rna)[15]<-'mgRNA.mgFish'

#make week into numeric
weeks<-substr(data.rna$Week,start=6,stop=6)
data.rna$Week<-as.numeric(weeks)

#visualze data.rna
ggplot(data=data.rna,aes(x=as.numeric(Week),y=RNA.relmax))+geom_point()+facet_wrap(~Species)+geom_smooth(method='loess')

#make ordered factor to test for differences through time
data.rna$OFSpecies<-as.factor(data.rna$Species)
data.rna$OFSpecies<-as.ordered(data.rna$OFSpecies)

rna.gam<-gam(mgRNA.mgFish~s(Week,k=5)+s(Week,by=OFSpecies,k=5),data=data.rna)

summary(rna.gam)
plot(rna.gam)

#Load body composition data to compare RNA and Ca
data<-read.csv('HatcheryBodyData.csv')

#remove NA rows
data<-data[!is.na(data$Weight..g.),]

data.all<-merge(data,data.rna,by=c('Species','Week'))

#multiple regression of body P ~ body Ca + RNA
bodyP.mod<-lm(P~Ca+mgRNA.mgFish+Week+factor(Hatchery),data=data.rna)

#Look at data
data.rna$residuals<-summary(lm(P~Ca,data=data.rna))$residuals

ggplot(data=data.rna,aes(x=mgRNA.mgFish,y=residuals,color=factor(Species)))+geom_point()#+geom_smooth(method='lm')

#now do this over %adult size
#change % data to numeric
percent.adult<-c()
for(i in 1:nrow(data.rna)){
	percent.adult[i]<-as.numeric(strsplit(data.rna$X._Adult_Size,split='%')[[i]])
}

data.rna$percent.adult<-percent.adult

rna.gam2<-gam(mgRNA.mgFish~s(percent.adult,k=5)+s(percent.adult,by=OFSpecies,k=5),data=data.rna)
summary(rna.gam2)