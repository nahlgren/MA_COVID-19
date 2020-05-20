### process MA city/town data to generate rates of increase in COVID cases
### to plot on a map of cities/towns in MA

library(sf)

setwd("/Users/nathanahlgren/Documents/Clark/Outreach/COVID/Weekly_city")

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




