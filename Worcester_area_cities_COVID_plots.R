library(ggplot2)
library(grid)
library(gridExtra)

cities<-c("Worcester","Shrewsbury","Holden","Grafton","Millbury","Leceister","West\ Boylston")


city.col<-c("#e41a1c","#377eb8","#4daf4a","#984ea3","#a65628","#ff7f00","#ffff33")
city.pop=c(185877,37973,19163,17765,13888,10970,8215)
city.area=c(38.57,21.66,36.22,23.3,16.3,24.7,13.8)
city.dens<-city.pop/city.area

city.pop.text<-paste(cities,", pop.: ",formatC(city.pop, format="f", big.mark=",", digits=0),sep="")
pch.no<-21

## read in Worceter file to pull colnames
i<-1
f<-paste(cities[i],"_city.tsv",sep="")
d<-read.table(file=f,sep="\t",header=T)


d.latest<-matrix(NA,nrow=length(cities),ncol=5)
#colnames(d.latest)<-colnames(d)
rownames(d.latest)<-cities

### generate plot for cumulative case #s over time Worcester and surrounding city plots

jpeg("Worcester_cities_cases.jpg",width=700,height=600)
par(cex=2)

for (i in c(1:length(cities))) {
	f<-paste(cities[i],"_city.tsv",sep="")
	d<-read.table(file=f,sep="\t",header=T)
	
	# process dates
	d$Date<-as.Date(d$Date)
	d$cases<-as.numeric(d$cases)
	d$cases.pop<-d$cases/city.pop[i]*1000
	
	### Day diff
	as.numeric(d$Date-d$Date[1])
	day.diff<-c(0)
	for (j in c(2:dim(d)[1])) {
		day.diff[j]<-as.numeric(d$Date[j]-d$Date[j-1])
		}
	
	### calculate new daily cases
	delta.newcases<-c(0,d$cases[c(2:length(d$cases))] - d$cases[c(1:(length(d$cases)-1))])
	d$dnewcases<-delta.newcases/day.diff
	d$dnewcases[1]<-0
	#d$dnewcases<-as.numeric(d$cases)
	
	## Running Days
	d$Days<-as.numeric(d$Date-d$Date[1])

	## if 1st city in the order, generate the plot, for the others add points with points()
	if (i==1) {
		### cases
#		plot(d$Date,d$cases,col='red')
#		plot(d$Date,d$cases/city.pop[i]*1e3,col=city.col[i],pch=pch.no,log="y")
		plot(d$Date,d$cases/city.pop[i]*1e3,bg=city.col[i],col='black',pch=pch.no,las=1,xlab="Date",ylab="Cases (per 1,000 people)",main="Cumulative COVID-19 cases")
		lines(d$Date,d$cases/city.pop[i]*1e3,col=city.col[i])	

		### daily new cases
#		plot(d$Date,d$dnewcases/city.pop[i]*1e3,col=city.col[i],pch=pch.no,las=1,xlab="Date",ylab="Daily new cases (per 1,000 people)",main="New COVID-19 cases per day")		

		} else {
		### cases
		points(d$Date,d$cases/city.pop[i]*1e3,bg=city.col[i],col='black',pch=pch.no)			
		lines(d$Date,d$cases/city.pop[i]*1e3,col=city.col[i])
		
		### daily new cases
#		points(d$Date,d$dnewcases/city.pop[i]*1e3,col=city.col[i],pch=pch.no)		

		}
	
	d.latest[i,]<-as.matrix(d[dim(d)[1],])
	
	}
legend("topleft",legend=city.pop.text,pt.bg=city.col,pch=rep(21,length(cities)),col=rep('black',length(cities)),box.col=NA,bg=NA)

dev.off()

#################################
### plot latest case number per capita vs. population density for these cities
### pull the case numbers for the last available date
d.latest[,1]<-as.Date(d.latest[,1])
d.latest[,2]<-as.numeric(d.latest[,2])
d.latest[,3]<-as.numeric(d.latest[,3])
d.latest[,4]<-as.numeric(d.latest[,4])
d.latest[,5]<-as.numeric(d.latest[,5])


jpeg("Worcester_cities_dens_cases.jpg",width=700,height=600)
par(cex=1)
plot(city.dens,d.latest[,3],pch=21,bg=city.col,col='black',xlab="Population density (people/square miles)",ylab="Cases per 1,000 people",las=1,cex=3,cex.lab=1.5,cex.axis=1.5, main="COVID-19 data, Worcester area towns",cex.main=2)
legend("bottomright",legend=city.pop.text,pt.bg=city.col,pch=rep(21,length(cities)),col=rep('black',length(cities)),box.col=NA,bg=NA,cex=1.5)
dev.off()