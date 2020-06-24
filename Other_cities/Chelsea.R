
chelsea.d<-read.table(file="Chelsea_city.tsv",header=T,sep="\t")

chelsea.d$Date<-as.Date(chelsea.d$Date,"%m/%d/%y")

as.numeric(chelsea.d$Date-chelsea.d$Date[1])

chelsea.d$cases<-as.numeric(chelsea.d$cases)


### daily new cases (divide by difference in day to get new per day)
chelsea.d$dnew<-c(0,chelsea.d$cases[c(2:length(chelsea.d$cases))] - chelsea.d$cases[c(1:(length(chelsea.d$cases)-1))])/c(0,chelsea.d$Date[c(2:length(chelsea.d$Date))] - chelsea.d$Date[c(1:(length(chelsea.d$Date)-1))])

## Weekly average of daily new cases
weekly.mean<-rep(NA,6)
for (i in c(7:length(chelsea.d$cases)) ) {
	weekly.mean<-c(weekly.mean,mean(chelsea.d$dnew[c((i-6):i)]))
}
chelsea.d$dnew.weekmean<-weekly.mean

chelsea.pop<-40160

##### Mass data
ma.total.pop<-6547695

ma.d<-read.table(file="CasesByDate.csv",sep=",",header=T)

# process cases
ma.d$Date<-as.Date(ma.d$Date,"%m/%d/%Y")
ma.d$cases<-ma.d$Positive.Total

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

## normalize by pop, comment out for raw daiy new cases
#ma.d$dnew.weekmean<-ma.d$dnew.weekmean/ma.total.pop*1e5





jpeg(file="Chelsea_MA_daily_new_cases.jpg",width=400,height=400)

plot(chelsea.d$Date,chelsea.d$dnew/chelsea.pop*100000,bg='red',pch=21,xlab="Date",ylab="New cases per day per 100,000 people",main="Chelsea vs. MA\nDaily new COVID-19 cases",cex.lab=1,cex.main=1,las=1,cex=1, ylim=c(0,350))
lines(chelsea.d$Date,chelsea.d$dnew.weekmean/chelsea.pop*100000,col='red',lwd=3)

points(ma.d$Date,ma.d$dnew/6.3e6*100000,bg='blue',pch=21,cex=1)
lines(ma.d$Date,ma.d$dnew.weekmean/6.3e6*100000,col='blue',lwd=3)
legend("topleft", c("Chelsea","Mass.","Chelsea 7 day avg","Mass. 7 day avg"), pch=c(19,19,NA,NA),lty=c(NA,NA,1,1),col=c('red','blue','red','blue'),cex=1)
dev.off()

n<-7
lm_fit<-lm(tail(chelsea.d$dnew,n)~tail(chelsea.d$Date,n))
lm_fit_summ<-summary(lm_fit)
paste("slope: ",round(lm_fit_summ$coefficients[2,1],3),sep="")
paste("p-value: ",round(lm_fit_summ$coefficients[2,4],3),sep="")

