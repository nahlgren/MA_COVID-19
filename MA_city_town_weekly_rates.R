### process MA city/town data to generate rates of increase in COVID cases
### to plot on a map of cities/towns in MA

library(sf)
library(RColorBrewer)



###### load in data for the population size and county membership of each city
city.pop<-read.table(file="MA_city_pops.txt",header=T,row.names=1,sep="\t")

###### load in case and rate data for COVID-19 available from MA DPH
## list of dates of the data files
file.dates<-c("4-14-2020","4-22-2020","4-29-2020","5_06_2020","5_13_2020","5_20_2020","5_27_2020","6_03_2020","6_10_2020","6_17_2020")
## dates of the data in R Date format
dates<-c("2020-4-14","2020-4-22","2020-4-29","2020-5-06","2020-5-13","2020-5-20","2020-5-27","2020-6-03","2020-6-10","2020-6-17")

dates<-as.Date(dates)
last.date<-dates[length(dates)]

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
rate.m<-matrix(NA,nrow=dim(d.ref)[1],ncol=6)
rownames(rate.m)<-rownames(d.ref)
colnames(rate.m)<-c("Rate","Perc.change.last2","Dnew.regression.nextlast3","Dnew.regression.last3","Lastcasenumber","Perc.change.rate")

last.n<-c((dim(case.m)[2]-n+1):dim(case.m)[2])
Dates<-dates[last.n]

for (i in c(1:dim(case.m)[1])) {
    tmp.df<-as.data.frame(Dates)
    tmp.df$Cases<-case.m[i,last.n]
    
    ## check that there are 3 point that are not NA before doing lm
    if (length(which(is.na(tmp.df$Cases)))<3) {
        lm.out<-lm(formula = Cases ~ Dates, data = tmp.df)
        rate.m[i,1]<-lm.out$coefficients[2]/city.pop.df$POP[i]*1e5
    }
    
    #	tmp.df$Cases<-case.m[i,2]
    ### add percentage change in cases
    rate.m[i,2]<-(case.m[i,(dim(case.m)[2])]-case.m[i,(dim(case.m)[2]-1)])/case.m[i,(dim(case.m)[2]-1)]
    
    
    ### regression of dnew last n dates
    
    Date<-tail(dates,(n+1))
    Cases<-tail(case.m[i,],(n+1))
    tmp2.df<-data.frame(Date,Cases)
    tmp2.df$dnew<-c(NA,tmp2.df$Cases[2:(n+1)]-tmp2.df$Cases[1:(n)])
    ### only calc regression if no NAs in Cases
    if ( !(length(which(is.na(tmp2.df$Cases)))>0) ) {
        lm.sum2<-summary(lm(Cases~Date,data=tail(tmp2.df,3)))
        lm2<-lm(Cases~Date,data=tmp2.df)
        rate.m[i,4]<-lm.sum2$coefficients[2,1]/city.pop.df$POP[i]*1e5
    }
    
    rate.m[i,5]<-tail(case.m[i,],1)
    
    ### regression of dnew last n dates
    Date<-head(tail(dates,(n+2)),(n+1))
    Cases<-head(tail(case.m[i,],(n+2)),(n+1))
    tmp3.df<-data.frame(Date,Cases)
    tmp3.df$dnew<-c(NA,tmp3.df$Cases[2:(n+1)]-tmp3.df$Cases[1:(n)])
    ### only calc regression if no NAs in Cases
    if ( !(length(which(is.na(tmp2.df$Cases)))>0) ) {
        lm.sum3<-summary(lm(Cases ~Date,data=tail(tmp3.df,3)))
        rate.m[i,3]<-lm.sum3$coefficients[2,1]/city.pop.df$POP[i]*1e5
        lm3<-lm(Cases~Date,data=tmp3.df)
    }
    
    ## fractional change in regression
    if( !(is.na(rate.m[i,3])) && !(is.na(rate.m[i,4]))  ) {
        rate.m[,6]<-(rate.m[,4]-rate.m[,3])/rate.m[,4]
    }
    
}




## calculate new cases/day normalized by population (per 100,000 people)
### note already normalized rates by pop in loop above

rate.df<-as.data.frame(rate.m)
rate.df$COVID.POP<-rate.df$Lastcasenumber/city.pop.df$POP*1e5

#round(rate.df$Rate.Pop,3)

rate.df.gt50cases<-rate.df[which(rate.df$Lastcasenumber>=50),]



#rate.mean.p.2sd<-mean(rate.df$Rate,na.rm=T)+2*sd(rate.df$Rate,na.rm=T)
#rate.df[which(rate.df$Rate>rate.mean.p.2sd),]

hist(rate.df$Rate,xlab="Linear regression of new cases/day/100,000 people")
## determine rates that are 2 stdev's above and below the mean
rate.pop.mean.p.2sd<-mean(rate.df$Rate,na.rm=T)+2*sd(rate.df$Rate,na.rm=T)
rate.pop.mean.m.2sd<-mean(rate.df$Rate,na.rm=T)-2*sd(rate.df$Rate,na.rm=T)
## display which towns in the rate.df are above or below 2 stdev's of the the mean
rate.df[which(rate.df$Rate>rate.pop.mean.p.2sd),]
rate.df[which(rate.df$Rate<rate.pop.mean.m.2sd),]

q.breaks<-quantile(rate.df$Rate,probs=c(0,0.05,0.25,0.75,0.95,1),na.rm=T)


rate.m.sort<-rate.m[order(rate.m[,6]),]
rate.m.sort<-rate.m[order(rate.m[,4]),]
hist(rate.m.sort[which(rate.m.sort[,5]>=50),6])
rate.m.sort.gt50cases<-rate.m.sort[which(rate.m.sort[,5]>=50),]

rate.df.gt50cases<-rate.df[which(rate.df$Lastcasenumber>=50),]
plot(rate.df.gt50cases[,1],rate.df.gt50cases[,6])

plot(rate.m.sort.gt50cases[,1],rate.m.sort.gt50cases[,6])

high.i<-intersect(which(rate.m.sort.gt50cases[,1]>=20),which(rate.m.sort.gt50cases[,6]>=-0.05))

############################################################################################################
############################################################################################################

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
ma$LATEST.CASE.NO=rep(NA,dim(ma)[1])
ma$RATE.LM.POP=rep(NA,dim(ma)[1])

#ma.towns<-levels(COVID.d$City.Town)
ma.towns<-levels(ma$TOWN)


#### add COVID data to ma frame
for (i in c(1:length(ma.towns))) {

	j<-which(ma$TOWN == ma.towns[i])
	for (j in which(ma$TOWN == ma.towns[i])) {
		ma$COUNTY[j]<-as.character(ma.pop$COUNTY[which(ma.pop$MUNICIPALITY ==ma.towns[i])])
		ma$POP2010[j]<-as.numeric(ma.pop$POPULATION[which(ma.pop$MUNICIPALITY ==ma.towns[i])])
		ma$COVID.RATE[j]<-COVID.d$RATE[which(COVID.d$CITY.TOWN==ma.towns[i])]
#		ma$RATE.LM[j]<-rate.df$Rate[which(rownames(rate.df)==ma.towns[i])]
		ma$RATE.LM.POP[j]<-rate.df$Rate[which(rownames(rate.df)==ma.towns[i])]
        ma$LATEST.CASE.NO[j]<-rate.df$Lastcasenumber[which(rownames(rate.df)==ma.towns[i])]
		}
	}


#plot(ma[which(ma$SHAPE_Area>0.5e7),"RATE.LM.POP"])
#plot(ma[which(ma$SHAPE_Area>0.5e7),"RATE.LM.POP"],pal=brewer.pal(9,"Reds"))


### only consider town/cities with >= 50 cases
ma.gt50<-ma
for (i in c(1:dim(ma.gt50)[1])) {
	if((!is.na(ma.gt50$LATEST.CASE.NO[i]))) {
		if(ma.gt50$LATEST.CASE.NO[i]<50) {
			ma.gt50$COVID.RATE[i]<-NA
			ma.gt50$RATE.LM[i]<-NA
			ma.gt50$RATE.LM.POP[i]<-NA
			}
		}
	}

## set color palette
red.pal=rev(brewer.pal((length(q.breaks)-1),"Spectral"))


### Plot new cases/day per 100,000 people based on regressions of weekly data for the last n time points (i.e. weeks)

jpeg(file=paste("MA_city-town_3weekregression_cases_per_day_percapita_",file.dates[length(file.dates)],"_red.jpg",sep=""),width=1600,height=800)
## reduce the number of shapes plotted
#plot(ma[which(ma$SHAPE_Area>0.5e7),"RATE.LM.POP"],breaks=q.breaks,pal=brewer.pal((length(q.breaks)-1),"Reds"),main=main=paste("New cases/day estimated from regressions of weekly data, last ",n, " weeks, updated ",last.date,sep=""),key.pos=1,cex.main=2)
##plot all the shapes
plot(ma[,"RATE.LM.POP"],breaks=q.breaks,main=paste("New cases/day  per 100,000 estimated from regressions of weekly data, last ",n, " weeks, updated ",last.date,sep=""),key.pos=1,cex.main=2,pal=brewer.pal(5,"Reds"))
dev.off()

jpeg(file=paste("MA_city-town_3weekregression_cases_per_day_percapita_",file.dates[length(file.dates)],".jpg",sep=""),width=1600,height=800)
plot(ma[,"RATE.LM.POP"],breaks=q.breaks,main=paste("New cases/day  per 100,000 estimated from regressions of weekly data, last ",n, " weeks, updated ",last.date,sep=""),key.pos=1,cex.main=2,pal=red.pal)
dev.off()

## plot using only cities with at least 50 cases
jpeg(file=paste("MA_city-town_3weekregression_cases_per_day_percapita_",file.dates[length(file.dates)],"gt50_red.jpg",sep=""),width=1600,height=800)
plot(ma.gt50[,"RATE.LM.POP"],breaks=q.breaks,main=paste("New cases/day per 100,000 estimated from regressions of weekly data, last ",n, " weeks, updated ",last.date,sep=""),key.pos=1,cex.main=2,pal=brewer.pal(5,"Reds"))
dev.off()

jpeg(file=paste("MA_city-town_3weekregression_cases_per_day_percapita_",file.dates[length(file.dates)],"_gt50.jpg",sep=""),width=1600,height=800)
plot(ma.gt50[,"RATE.LM.POP"],breaks=q.breaks,main=paste("New cases/day per 100,000 estimated from regressions of weekly data, last ",n, " weeks, updated ",last.date,sep=""),key.pos=1,cex.main=2,pal=red.pal)
dev.off()


### plot latest COVID-19 cases per 100,000

jpeg(file=paste("MA_city-town_ratepercap_",file.dates[length(file.dates)],".jpg",sep=""),width=1600,height=800)
plot(ma[,"COVID.RATE"],breaks=quantile(ma$COVID.RATE,probs=c(0,0.05,0.25,0.75,0.95,1),na.rm=T),main=paste("Total COVID-19 cases per 100,000 people, as of ",last.date,sep=""),key.pos=1,cex.main=2)
dev.off()


jpeg(file=paste("MA_city-town_ratepercap_",file.dates[length(file.dates)],"_reds.jpg",sep=""),width=1600,height=800)
plot(ma[,"COVID.RATE"],breaks=quantile(ma$COVID.RATE,probs=c(0,0.05,0.25,0.75,0.95,1),na.rm=T),main=paste("Total COVID-19 cases per 100,000 people, as of ",last.date,sep=""),key.pos=1,cex.main=2,pal=red.pal)
dev.off()



### plot case data for 5% and 95% quantile cities
q95.cities<-which(rate.df$Rate.Pop>q.breaks[5])
q5.cities<-which(rate.df$Rate.Pop>q.breaks[2])

#jpeg(file="MA_cities_0.95quant.jpg",width=600,height=600)
#par(mfrow=c(4,4))
#for (i in c(1:length(q95.cities))) {
#	plot(as.Date(colnames(case.m)),case.m[q95.cities[i],]/city.pop.df$POP[q95.cities[i]]*100000,xlab="Date",ylab="Cases per 100,000",main=rownames(case.m)[q95.cities[i]],las=1,bg='black',pch=21)
#}
#dev.off()

#jpeg(file="MA_cities_0.05quant.jpg",width=600,height=600)
#q5.cities<-which(rate.df$Rate.Pop<q.breaks[2])
#par(mfrow=c(4,4))
#for (i in c(1:length(q5.cities))) {
##	plot(as.Date(colnames(case.m)),case.m[q5.cities[i],],xlab="Date",ylab="Cumulative cases",main=rownames(case.m)[q5.cities[i]],cex=3)
#	plot(as.Date(colnames(case.m)),case.m[q5.cities[i],]/city.pop.df$POP[q5.cities[i]]*100000,xlab="Date",ylab="Cases per 100,000",main=rownames(case.m)[q5.cities[i]],las=1,bg='black',pch=21)
#}
#dev.off()


########################################################################
########################################################################
### check plot of cases by date for a particular city
## n = number of days to plot regression lines
## i = index in the rate.m or rate.df data

check.city.plot.rate<-function(i,n) {
    Date<-tail(dates,(n+1))
    Cases<-tail(case.m[i,],(n+1))
    tmp2.df<-data.frame(Date,Cases)
    tmp2.df$dnew<-c(NA,tmp2.df$Cases[2:(n+1)]-tmp2.df$Cases[1:(n)])
    lm2<-lm(Cases~Date,data=tmp2.df)
    
    Date<-head(tail(dates,(n+2)),(n+1))
    Cases<-head(tail(case.m[i,],(n+2)),(n+1))
    tmp3.df<-data.frame(Date,Cases)
    tmp3.df$dnew<-c(NA,tmp3.df$Cases[2:(n+1)]-tmp3.df$Cases[1:(n)])
    lm3<-lm(Cases~Date,data=tmp3.df)
    
    plot(dates,case.m[i,],ylab="Cases",main=rownames(case.m)[i],pch=19,col='black')
    lines(tail(tmp2.df$Date,3),tail(lm2$fitted.values,3),col='red')
    lines(tail(tmp3.df$Date,3),tail(lm3$fitted.values,3),col='red')
}

check.city.plot.rate.logy<-function(i,n) {
    Date<-tail(dates,(n+1))
    Cases<-tail(case.m[i,],(n+1))
    tmp2.df<-data.frame(Date,Cases)
    tmp2.df$dnew<-c(NA,tmp2.df$Cases[2:(n+1)]-tmp2.df$Cases[1:(n)])
    lm2<-lm(Cases~Date,data=tmp2.df)
    
    Date<-head(tail(dates,(n+2)),(n+1))
    Cases<-head(tail(case.m[i,],(n+2)),(n+1))
    tmp3.df<-data.frame(Date,Cases)
    tmp3.df$dnew<-c(NA,tmp3.df$Cases[2:(n+1)]-tmp3.df$Cases[1:(n)])
    lm3<-lm(Cases~Date,data=tmp3.df)
    
    plot(dates,case.m[i,],ylab="Cases",main=rownames(case.m)[i],pch=19,col='black',log="y")
    lines(tail(tmp2.df$Date,3),tail(lm2$fitted.values,3),col='red')
    lines(tail(tmp3.df$Date,3),tail(lm3$fitted.values,3),col='red')
}


pos.i<-which(rate.df.gt50cases[,6]>0)
pos.i<-which(rate.df.gt50cases[,4]>0)
pos.i<-tail(pos.i,20)
pos.i<-c(188:197)

par(mfrow=c(4,5))
for (i in c(1:length(pos.i))) {
    k<-which(rownames(case.m)==rownames(rate.m.sort.gt50cases)[pos.i[i]])
    #	k<-which(rownames(case.m)==rownames(rate.m.sort.gt50cases.nona)[pos.i[i]])
    #	check.city.plot.rate(k,n)
    check.city.plot.rate.logy(k,n)
}




### plot of statewide case data
i<-353
plot(as.Date(colnames(case.m)),case.m[i,],xlab="Date",ylab="Cumulative cases",main=rownames(case.m)[i])
