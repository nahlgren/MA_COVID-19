### process MA city/town data to generate rates of increase in COVID cases
### to plot on a map of cities/towns in MA

library(sf)
library(RColorBrewer)


###### load in data for the population size and county membership of each city
city.pop<-read.table(file="MA_city_pops.txt",header=T,row.names=1,sep="\t")

###### load in case and rate data for COVID-19 available from MA DPH
## list of dates of the data files
file.dates<-c("4-14-2020","4-22-2020","4-29-2020","5_06_2020","5_13_2020")
## dates of the data in R Date format
dates<-c("2020-4-14","2020-4-22","2020-4-29","2020-5-06","2020-5-13")
dates<-as.Date(dates)

## prefix and suffix of cases data files
pre<-"covid-19-city-town-"
suff<-".txt"
f<-paste(pre,file.dates[1],suff,sep="")
d<-read.table(file=f,header=T,row.names=1,sep="\t")
d.ref<-d

## check that city names for the city.pop and case matrix d line up
(!(length(which(rownames(city.pop)==rownames(d.ref)[1:351]))))

## make dataframe to hold city population and county membership in the same order as the cases date
city.pop.m<-matrix(NA,nrow=dim(d.ref)[1],ncol=2)
rownames(city.pop.m)<-rownames(d.ref)
colnames(city.pop.m)<-c("POP","COUNTY")
city.pop.df<-as.data.frame(city.pop.m)

## add pop and county info to city.pop.df
for (i in c(1:(dim(city.pop.df)[1]-2)) ) {
	city.pop.df[i,1]<-as.numeric(city.pop[which(rownames(city.pop)==rownames(city.pop.df)[i]),4])
	city.pop.df[i,2]<-as.character(city.pop[which(rownames(city.pop)==rownames(city.pop.df)[i]),2])
	}

## manually add population for all of MA	
city.pop.df[353,1]<-sum(city.pop.df$POP,na.rm=T)


### check that all files are of the same length and row names are congruent
for (i in c(2:length(file.dates))) {
	f<-paste(pre,file.dates[1],suff,sep="")
	d<-read.table(file=f,header=T,row.names=1,sep="\t")
	if (!(length(which(rownames(d)==rownames(d.ref)))==dim(d.ref)[1])) {
			print(paste("Error with rownames for file ",f,sep=""))
		} else {
			print(paste("Rownames for file ",f," are ok.",sep=""))
		}
}

### set up matrix to hold all case data
case.m<-matrix(NA,nrow=dim(d.ref)[1],ncol=length(dates))
rownames(case.m)<-rownames(d.ref)
colnames(case.m)<-as.character(dates)

## read in case, rate data by each date and add to case.m matrix
for (i in c(1:length(file.dates))) {
	f<-paste(pre,file.dates[i],suff,sep="")
	d<-read.table(file=f,header=T,row.names=1,sep="\t")
	d$CASE[which(d$CASE=="<5")]<-NA
	case.m[,i]<-as.numeric(as.character(d$CASE))
}

### Determine an estimated rate of new cases/day by linear regression of the last n time points

n<-3

### set up matrix to hold rate data
rate.m<-matrix(NA,nrow=dim(d.ref)[1],ncol=1)
rownames(rate.m)<-rownames(d.ref)
colnames(rate.m)<-"Rate"

last.n<-c((dim(case.m)[2]-n+1):dim(case.m)[2])
Dates<-dates[last.n]

for (i in c(1:dim(case.m)[1])) {
	tmp.df<-as.data.frame(Dates)
	tmp.df$Cases<-case.m[i,last.n]

	## check that there are 3 point that are not NA before doing lm
	if (length(which(is.na(tmp.df$Cases)))<3) {
		lm.out<-lm(formula = Cases ~ Dates, data = tmp.df)		
		rate.m[i,1]<-lm.out$coefficients[2]
		}
	}

## calculate new cases/day normalized by population (per 100,000 people)
rate.df<-as.data.frame(rate.m)
rate.df$Rate.Pop<-rate.df$Rate/city.pop.df$POP*100000
round(rate.df$Rate.Pop,3)


#rate.mean.p.2sd<-mean(rate.df$Rate,na.rm=T)+2*sd(rate.df$Rate,na.rm=T)
#rate.df[which(rate.df$Rate>rate.mean.p.2sd),]

hist(rate.df$Rate.Pop,xlab="Linear regression of new cases/day/100,000 people")
## determine rates that are 2 stdev's above and below the mean
rate.pop.mean.p.2sd<-mean(rate.df$Rate.Pop,na.rm=T)+2*sd(rate.df$Rate.Pop,na.rm=T)
rate.pop.mean.m.2sd<-mean(rate.df$Rate.Pop,na.rm=T)-2*sd(rate.df$Rate.Pop,na.rm=T)
## display which towns in the rate.df are above or below 2 stdev's of the the mean
rate.df[which(rate.df$Rate.Pop>rate.pop.mean.p.2sd),]
rate.df[which(rate.df$Rate.Pop<rate.pop.mean.m.2sd),]

q.breaks<-quantile(rate.df$Rate.Pop,probs=c(0,0.05,0.25,0.75,0.95,1),na.rm=T)

####################################
### Plot map of the daily new cases/day/100,000 people by city
## source of .shp file: http://download.massgis.digital.mass.gov/shapefiles/state/townssurvey_shp.zip

ma<-st_read("TOWNSSURVEY_POLY.shp")

## load latest weekly case data
f<-paste(pre,file.dates[length(file.dates)],suff,sep="")
COVID.d<-read.table(file=f,sep="\t",header=T)
#ma.pop<-read.table(file="MA_town_pop_data.txt",sep="\t",header=T)
ma.pop<-read.table(file="MA_city_pops.txt",sep="\t",header=T)


levels(COVID.d$CITY.TOWN) == levels(ma$TOWN)
levels(ma.pop$MUNICIPALITY) == levels(ma$TOWN)

ma$COVID.RATE=rep(NA,dim(ma)[1])
ma$POP2010=rep(NA,dim(ma)[1])
ma$COUNTY=rep(NA,dim(ma)[1])

ma$RATE.LM=rep(NA,dim(ma)[1])
ma$RATE.LM.POP=rep(NA,dim(ma)[1])

#ma.towns<-levels(COVID.d$City.Town)
ma.towns<-levels(ma$TOWN)



for (i in c(1:length(ma.towns))) {

	j<-which(ma$TOWN == ma.towns[i])
	for (j in which(ma$TOWN == ma.towns[i])) {
		ma$COUNTY[j]<-as.character(ma.pop$COUNTY[which(ma.pop$MUNICIPALITY ==ma.towns[i])])
		ma$POP2010[j]<-as.numeric(ma.pop$POPULATION[which(ma.pop$MUNICIPALITY ==ma.towns[i])])
		ma$COVID.RATE[j]<-COVID.d$RATE[which(COVID.d$CITY.TOWN==ma.towns[i])]
		ma$RATE.LM[j]<-rate.df$Rate[which(rownames(rate.df)==ma.towns[i])]
		ma$RATE.LM.POP[j]<-rate.df$Rate.Pop[which(rownames(rate.df)==ma.towns[i])]
		}
	}


#plot(ma[which(ma$SHAPE_Area>0.5e7),"RATE.LM.POP"])
#plot(ma[which(ma$SHAPE_Area>0.5e7),"RATE.LM.POP"],pal=brewer.pal(9,"Reds"))

#plot(ma[which(ma$SHAPE_Area>0.5e7),"RATE.LM.POP"],breaks=q.breaks,main="New cases/day estimated from regression last 3 weekly datapoint")

### Plot new cases/day per 100,000 people based on regressions of weekly data for the last n time points (i.e. weeks)
jpeg(file="MA_city-town_3weekregression_cases_per_day_percapita.jpg",width=1600,height=800)
plot(ma[which(ma$SHAPE_Area>0.5e7),"RATE.LM.POP"],breaks=q.breaks,pal=brewer.pal((length(q.breaks)-1),"Reds"),main="New cases/day estimated from regressions of weekly data, last 3 weeks",key.pos=1,cex.main=2)
dev.off()

### plot case data for 5% and 95% quantile cities
q95.cities<-which(rate.df$Rate.Pop>q.breaks[5])
q5.cities<-which(rate.df$Rate.Pop>q.breaks[2])

jpeg(file="MA_cities_0.95quant.jpg",width=600,height=600)
par(mfrow=c(4,4))
for (i in c(1:length(q95.cities))) {
	plot(as.Date(colnames(case.m)),case.m[q95.cities[i],]/city.pop.df$POP[q95.cities[i]]*100000,xlab="Date",ylab="Cases per 100,000",main=rownames(case.m)[q95.cities[i]],las=1,bg='black',pch=21)
}
dev.off()

jpeg(file="MA_cities_0.05quant.jpg",width=600,height=600)
q5.cities<-which(rate.df$Rate.Pop<q.breaks[2])
par(mfrow=c(4,4))
for (i in c(1:length(q5.cities))) {
#	plot(as.Date(colnames(case.m)),case.m[q5.cities[i],],xlab="Date",ylab="Cumulative cases",main=rownames(case.m)[q5.cities[i]],cex=3)
	plot(as.Date(colnames(case.m)),case.m[q5.cities[i],]/city.pop.df$POP[q5.cities[i]]*100000,xlab="Date",ylab="Cases per 100,000",main=rownames(case.m)[q5.cities[i]],las=1,bg='black',pch=21)
}
dev.off()

### plot of statewide case data
i<-353
plot(as.Date(colnames(case.m)),case.m[i,],xlab="Date",ylab="Cumulative cases",main=rownames(case.m)[i])