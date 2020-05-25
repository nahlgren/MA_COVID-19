### Written by Nathan Ahlgren, Clark University
### Analysis of Massachusetts state-wide and county-level COVID-19 data
### Focused on comparing daily new case and COVID-19 related death data
### and in particular comparing Worcester county to other counties

library(ggplot2)
library(grid)
library(gridExtra)


##################################################################

### Analysis of MA County data
### Data downloaded from MA.gov website: https://www.mass.gov/info-details/covid-19-response-reporting; https://www.mass.gov/doc/covid-19-raw-data-may-24-2020/download
### Analyze case data

### load and process MA state-wide data

ma.d<-read.table(file="CasesByDate.csv",sep=",",header=T)

# process cases
ma.d$Date<-as.Date(ma.d$Date,"%m/%d/%Y")
ma.d$cases<-ma.d$Total

### calculate new daily cases
ma.d$dnew<-c(0,ma.d$cases[c(2:length(ma.d$cases))] - ma.d$cases[c(1:(length(ma.d$cases)-1))])

ma.d$dnew<-c(0,ma.d$cases[c(2:length(ma.d$cases))] - ma.d$cases[c(1:(length(ma.d$cases)-1))])

## Running Days
ma.d$Days<-as.numeric(ma.d$Date-ma.d$Date[1])

## Weekly average of daily new deaths
weekly.mean<-rep(NA,6)
for (i in c(7:length(ma.d$cases)) ) {
	weekly.mean<-c(weekly.mean,mean(ma.d$dnew[c((i-6):i)]))
}
ma.d$dnew.weekmean<-weekly.mean

#### process MA total deaths

ma.death.d<-read.table(file="DeathsReported.csv",sep=",",header=T)


ma.death.d$Date<-as.Date(ma.death.d$Date,"%m/%d/%Y")
#ma.death.d$cases<-ma.death.d$Deaths

### calculate new daily Deaths
ma.death.d$dnew<-c(0,ma.death.d$Deaths[c(2:length(ma.death.d$Deaths))] - ma.death.d$Deaths[c(1:(length(ma.death.d$Deaths)-1))])



## Running Days
ma.death.d$Days<-as.numeric(ma.death.d$Date-ma.death.d$Date[1])

## Weekly average of daily new deaths
weekly.mean<-rep(NA,6)
for (i in c(7:length(ma.death.d$Deaths)) ) {
	weekly.mean<-c(weekly.mean,mean(ma.death.d$dnew[c((i-6):i)]))
}
ma.death.d$dnew.weekmean<-weekly.mean



### load and process county-level data


ma.county.d<-read.table(file="County.csv",sep=",",header=T)

county.list<-levels(ma.county.d$County)
#county.list<-c("Worcester")

# process dates
ma.county.d$Date<-as.Date(ma.county.d$Date,"%m/%d/%Y")

############### plots  ##################
### loop over counties

for (j in c(1:length(county.list)) ) {
	print(county.list[j])
	}

for (j in c(1:length(county.list)) ) {
	c.i<-which(ma.county.d$County==county.list[j])
	county.d.tmp<-ma.county.d[c.i,]
	print(county.list[j])
	
	
	county.d.tmp$cases<-county.d.tmp$Count
	county.d.tmp$cases<-county.d.tmp$Deaths
	
	## Calculate daily new
	county.d.tmp$dnew<-c(0,county.d.tmp$cases[c(2:length(county.d.tmp$cases))] - county.d.tmp$cases[c(1:(length(county.d.tmp$cases)-1))])
	
	
	## Running Days
	county.d.tmp$Days<-as.numeric(county.d.tmp$Date-county.d.tmp$Date[1])
	
	## Weekly average of daily new cases
	weekly.mean<-rep(NA,6)
	for (i in c(7:length(county.d.tmp$cases)) ) {
		weekly.mean<-c(weekly.mean,mean(county.d.tmp$dnew[c((i-6):i)]))
		}
	county.d.tmp$dnew.weekmean<-weekly.mean

	
	ma.coeff<-max(county.d.tmp$dnew.weekmean,na.rm=T)/max(ma.d$dnew.weekmean,na.rm=T)
	p10 <- ggplot(data = county.d.tmp, aes(x = Date)) +
			geom_point(aes(y=dnew.weekmean), cex=3) +
			geom_line(aes(y=dnew.weekmean)) +
	#        geom_point(data = d.suns, aes(x = Date, y = dnew.weekmean), bg='yellow', col='black',cex=3, pch=21 ) +	
	        geom_point(data = ma.d, aes(y=dnew.weekmean*ma.coeff),col='blue',cex=3) +
			geom_line(data = ma.d, aes(y=dnew.weekmean*ma.coeff),col='blue') +
			scale_y_continuous(sec.axis = sec_axis(~./ma.coeff, name = "Massachusetts daily new cases")) + 
	#      geom_line(cex=2) +
	 #     scale_y_log10() +
	    theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size), axis.title.y.right = element_text(color = 'blue')) +
	      labs(title = paste(county.list[j]," county vs. MA\ndaily new COVID-19 cases, 7-day average",sep=""),
	           x = "Date", y = "County daily new cases")
	
	jpg.f<-paste(county.list[j],"_county_vs_MA.jpg",sep="")
	print(jpg.f)
	jpeg(file=jpg.f,width=400,height=400)
	p10
	dev.off()

	} #end county loop

#################################
### Analyze death data

for (j in c(1:length(county.list)) ) {
	c.i<-which(ma.county.d$County==county.list[j])
	county.d.tmp<-ma.county.d[c.i,]
	print(county.list[j])
	
	## Death data
	county.d.tmp$cases<-county.d.tmp$Deaths
	
	## Calculate daily new
	county.d.tmp$dnew<-c(0,county.d.tmp$cases[c(2:length(county.d.tmp$cases))] - county.d.tmp$cases[c(1:(length(county.d.tmp$cases)-1))])
	
	
	## Running Days
	county.d.tmp$Days<-as.numeric(county.d.tmp$Date-county.d.tmp$Date[1])
	
	## Weekly average of daily new cases
	weekly.mean<-rep(NA,6)
	for (i in c(7:length(county.d.tmp$cases)) ) {
		weekly.mean<-c(weekly.mean,mean(county.d.tmp$dnew[c((i-6):i)]))
		}
	county.d.tmp$dnew.weekmean<-weekly.mean

	
	ma.coeff<-max(county.d.tmp$dnew.weekmean,na.rm=T)/max(ma.death.d$dnew.weekmean,na.rm=T)
	p10 <- ggplot(data = county.d.tmp, aes(x = Date)) +
			geom_point(aes(y=dnew.weekmean), cex=3) +
			geom_line(aes(y=dnew.weekmean)) +
	#        geom_point(data = d.suns, aes(x = Date, y = dnew.weekmean), bg='yellow', col='black',cex=3, pch=21 ) +	
	        geom_point(data = ma.death.d, aes(y=dnew.weekmean*ma.coeff),col='blue',cex=3) +
			geom_line(data = ma.death.d, aes(y=dnew.weekmean*ma.coeff),col='blue') +
			scale_y_continuous(sec.axis = sec_axis(~./ma.coeff, name = "Massachusetts daily new deaths")) + 
	#      geom_line(cex=2) +
	 #     scale_y_log10() +
	    theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size), axis.title.y.right = element_text(color = 'blue')) +
	      labs(title = paste(county.list[j]," county vs. MA\ndaily new COVID-19 related deaths, 7-day average",sep=""),
	           x = "Date", y = "County daily new deaths")
	
	jpg.f<-paste(county.list[j],"_county_vs_MA_deaths.jpg",sep="")
	print(jpg.f)
	jpeg(file=jpg.f,width=400,height=400)
	p10
	dev.off()

	} #end county loop

