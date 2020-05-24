library(ggplot2)
library(grid)
library(gridExtra)

d<-read.table(file="Worcester_city.tsv",sep="\t",header=T)
death.d<-read.table(file="Worcester_deaths.tsv",sep="\t",header=T)
hosp.d<-read.table(file="Worcester_hospitalizations.tsv",sep="\t",header=T)

# process dates, cases, deaths, hospitalizations
d$Date<-as.Date(d$Date)
d$cases<-as.numeric(d$cases)

death.d$Date<-as.Date(death.d$Date)
death.d$Deaths<-as.numeric(death.d$Deaths)

hosp.d$Date<-as.Date(hosp.d$Date)
hosp.d$Hospitalizations<-as.numeric(hosp.d$Hospitalizations)


### calculate new daily cases
d$dnew<-c(0,d$cases[c(2:length(d$cases))] - d$cases[c(1:(length(d$cases)-1))])
#d$dnew<-as.numeric(d$cases)

### calculate new daily deaths
death.d$dnew<-c(0,death.d$Deaths[c(2:length(death.d$Deaths))] - death.d$Deaths[c(1:(length(death.d$Deaths)-1))])

### calculate new daily hospitalizations
hosp.d$dnew<-c(0,hosp.d$Hospitalizations[c(2:length(hosp.d$Hospitalizations))] - hosp.d$Hospitalizations[c(1:(length(hosp.d$Hospitalizations)-1))])



## Running Days
d$Days<-as.numeric(d$Date-d$D ate[1])
death.d$Days<-as.numeric(death.d$Date-death.d$Date[1])

## Weekly average of daily new cases
weekly.mean<-rep(NA,6)
for (i in c(7:length(d$cases)) ) {
	weekly.mean<-c(weekly.mean,mean(d$dnew[c((i-6):i)]))
}
d$dnew.weekmean<-weekly.mean

## Weekly average of daily new deaths

weekly.mean<-rep(NA,6)
for (i in c(7:length(death.d$Deaths)) ) {
	weekly.mean<-c(weekly.mean,mean(death.d $dnew[c((i-6):i)]))
}
death.d$dnew.weekmean<-weekly.mean

weekly.mean<-rep(NA,6)
for (i in c(7:length(hosp.d$Hospitalizations)) ) {
	weekly.mean<-c(weekly.mean,mean(hosp.d$dnew[c((i-6):i)]))
}
hosp.d$dnew.weekmean<-weekly.mean

weekly.mean<-rep(NA,6)
for (i in c(7:length(hosp.d$Hospitalizations)) ) {
	weekly.mean<-c(weekly.mean,mean(hosp.d$Hospitalizations[c((i-6):i)]))
}
hosp.d$hosp.weekmean<-weekly.mean


########## Determine linear regressions on case data last 7 or 14 days
########## and extraoplate in the future for 14 days

### Regression on last 7 days + extend prediction by ex.d days
d.l<-dim(d)[1]
d.last7<-d[c((d.l - 6):d.l),]
ex.d<-14

lm_fit <- lm(cases ~ Date, data=d.last7)
#summary(lm_fit)
d.pred.last7 <- data.frame(cases_pred = predict(lm_fit, d.last7))
d.pred.last7$Date <- d.last7$Date

d.extend<-data.frame(Date = c(d.last7$Dat,(d.last7$Date[length(d.last7$Date)] + ex.d)))
d.extend$cases_pred<-predict(lm_fit,d.extend)


lm_fit <- lm(dnew ~ Date, data=d.last7)
#summary(lm_fit)
d.pred.last7$dnew_pred <- predict(lm_fit, d.last7)

####################################
### Regression on last 14 days
d.l<-dim(d)[1]
d.last14<-d[c((d.l - 13):d.l),]

lm_fit <- lm(cases ~ Date, data=d.last14)
#summary(lm_fit)
d.pred.last14 <- data.frame(cases_pred = predict(lm_fit, d.last14))
d.pred.last14$Date <- d.last14$Date
newcase.p.day<-as.character(round(lm_fit$coefficients[2],0))

lm_fit <- lm(dnew ~ Date, data=d.last14)
#summary(lm_fit)
d.pred.last14$dnew_pred <- predict(lm_fit, d.last14)



#plot(d$cases,d$Date)

#ggplot(data = d, aes(x = Days, y = cases)) +
#      geom_point() +
#      scale_y_log10() +#      labs(title = "Cumulative COVID-19 cases in Worcester, MA",#           x = "Days", y = "Cases")


d.legend<-data.frame(Date = d$Date[c(1,10)])
d.legend$cases<-0.9*c(max(d.extend$cases_pred),max(d.extend$cases_pred))

d.legend.2<-data.frame(Date = d$Date[c(1,10)])
d.legend.2$dnew<-0.9*c(max(d$dnew),max(d$dnew))

font.size=14
	
sun.start<-as.Date("2020/03/15")
seq.c<-seq(from=0,by=7,to=as.numeric(as.Date("2020-05-24")-sun.start)/7)
#suns<-c(2+seq(from=0,by=7,dim(d)[1]))
suns.seq<-sun.start+seq.c

suns<-c()
for (i in c(1:length(suns.seq))) {
	sun.i<-which(d$Date==suns.seq[i])
	if (length(which(d$Date==suns.seq[i]))==0) {
	} else {
		suns<-c(suns,which(d$Date==suns.seq[i]))	
		}
	}

d.suns<-d[suns,]
deaths.d.suns<-death.d[suns,]

####### Create ggplots to arrange together 

##### Date vs cases linear 
p1 <- ggplot(data = d, aes(x = Date, y = cases)) +
      geom_point(cex=3) +
 #     scale_y_log10() +
	geom_line() +
 	geom_line(color='red',data = d.extend, aes(x=Date, y=cases_pred),size = 2) +
 	xlim(min(d$Date),max(d.extend$Date)) +
 	ylim(min(d$cases),max(d.extend$cases_pred)) +
	annotate("text", label = paste("2 week regression\n& extrapolation:",paste(newcase.p.day," new cases per day,\non average",sep=""),sep="\n"), x = min(d$Date+14), y = 0.85*max(d.extend$cases_pred), colour = "red", hjust=0, size=6) +
	geom_line(color='red',data = d.legend, aes(x=Date,y=cases),size = 2) +
	 theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size)) +
#	scale_color_manual(name = "Regression",values = c("2 week" = "red")) +

#	 theme(plot.title = element_text(face = "bold")) +
      labs(title = "Cumulative cases (linear scale)",
           x = "Date", y = "Confirmed cases")
 
##### Date vs cases log scale
p2 <- ggplot(data = d, aes(x = Date, y = cases)) +
      geom_point(cex=3) +
	geom_line() +
	      scale_y_log10() +
	geom_line(color='red',data = d.extend, aes(x=Date, y=cases_pred),size = 2) +
 	xlim(min(d$Date),max(d.extend$Date)) +
# 	ylim(min(d$cases),max(d.extend$cases_pred)) +
#	annotate("text", label = "2 week regression\n& extrapolation", x = min(d$Date+7), y = 0.9*max(d.extend$cases_pred), colour = "red", hjust=0) +
#	geom_line(color='red',data = d.legend, aes(x=Date,y=cases)) +

	 theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size)) +
#	 theme(plot.title = element_text(face = "bold")) +
      labs(title = "Cumulative cases (log scale)",
           x = "Date", y = "Confirmed cases")

##### Date vs daily new cases linear           
p3 <- ggplot(data = d, aes(x = Date, y = dnew)) +
      geom_point(cex=3) +
	geom_line() +
      geom_point(data = d.suns, aes(x = Date, y = dnew), col='yellow', cex=3 ) +
	#      scale_y_log10() +
 	xlim(min(d$Date),max(d.extend$Date)) +
 	geom_line(color='red',data = d.pred.last14, aes(x=Date, y=dnew_pred),size = 2) +

	annotate("text", label = "2 week regression", x = min(d$Date+14), y = 0.9*max(d$dnew), colour = "red", hjust=0, size=6) +
	geom_line(color='red',data = d.legend.2, aes(x=Date,y=dnew),size = 2) +

	 theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size)) +#	 theme(plot.title = element_text(face = "bold")) +
      labs(title = "Daily new cases (linear scale)",
           x = "Date", y = "Daily new cases")           

##### Date vs daily new cases log
p4 <- ggplot(data = d, aes(x = Date, y = dnew)) +
      geom_point(cex=3) +
      scale_y_log10() +
	geom_line() +
      geom_point(data = d.suns, aes(x = Date, y = dnew), col='yellow', cex=3 ) +
  	xlim(min(d$Date),max(d.extend$Date)) +
 	geom_line(color='red',data = d.pred.last14, aes(x=Date, y=dnew_pred),size = 2) +
	 theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size)) +
#	 theme(plot.title = element_text(face = "bold")) +
      labs(title = "Daily new cases (log scale)",
           x = "Date", y = "Daily new cases") 

#### Dave vs 7 day mean of daily new cases (dnew.weekmean) linear
p5 <- ggplot(data = d, aes(x = Date, y = dnew.weekmean)) +
      geom_point(cex=3) +
#      scale_y_log10() +
#	geom_line() +
 	xlim(min(d$Date),max(d.extend$Date)) +
	 theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size)) +
#	 theme(plot.title = element_text(face = "bold")) +
      labs(title = "Weekly avg of daily\nnew cases (linear scale)",
           x = "Date", y = "Weekly mean of\ndaily new cases")           

#### Dave vs 7 day mean of daily new cases (dnew.weekmean) log
p6 <- ggplot(data = d, aes(x = Date, y = dnew.weekmean)) +
      geom_point(cex=3) +
      scale_y_log10() +
 	xlim(min(d$Date),max(d.extend$Date)) +      
	 theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size)) +
#	 theme(plot.title = element_text(face = "bold")) +
      labs(title = "Weekly avg of daily\nnew cases (log scale)",
           x = "Date", y = "Weekly mean of\ndaily new cases") 



##### Date vs cases & deaths linear 
death.coeff<-max(d$dnew.weekmean,na.rm=T)/max(death.d$dnew.weekmean,na.rm=T)
p7 <- ggplot(data = d, aes(x = Date)) +
		geom_point(aes(y=dnew.weekmean), cex=3) +
		geom_line(aes(y=dnew.weekmean)) +
        geom_point(data = d.suns, aes(x = Date, y = dnew.weekmean), bg='yellow', col='black',cex=3, pch=21 ) +
		geom_line(data = death.d, aes(y=dnew.weekmean*death.coeff),col='red') +
		geom_point(data = death.d, aes(y=dnew.weekmean*death.coeff),col='red',cex=3) +
		scale_y_continuous(sec.axis = sec_axis(~./death.coeff, name = "Daily new deaths")) + 
#      geom_line(cex=2) +
 #     scale_y_log10() +
    theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size), axis.title.y.right = element_text(color = 'red')) +
      labs(title = "Worcester daily new COVID-19 cases\n and deaths, 7 day mean",
           x = "Date", y = "Daily new cases")

#plot(d$Date,d$cases)
#points(death.d$Date,death.d$Deaths*death.coeff,col='red')

hosp.coeff<-max(d$dnew.weekmean,na.rm=T)/max(hosp.d$hosp.weekmean,na.rm=T)
p8 <- ggplot(data = d, aes(x = Date)) +
		geom_point(aes(y=dnew.weekmean), cex=3) +
		geom_line(aes(y=dnew.weekmean)) +
        geom_point(data = d.suns, aes(x = Date, y = dnew.weekmean), bg='yellow', col='black',cex=3, pch=21 ) +	
        geom_point(data = hosp.d, aes(y=hosp.weekmean*hosp.coeff),col='red',cex=3) +
		geom_line(data = hosp.d, aes(y=hosp.weekmean*hosp.coeff),col='red') +
		scale_y_continuous(sec.axis = sec_axis(~./hosp.coeff, name = "Hospitilizations")) + 
#      geom_line(cex=2) +
 #     scale_y_log10() +
    theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size), axis.title.y.right = element_text(color = 'red')) +
      labs(title = "Worcester daily new COVID-19 cases\nand hospitalizations, 7 day mean",
           x = "Date", y = "Daily new cases")



t<-textGrob(expression(bold("COVID-19 case data for Worcester, MA")),gp=gpar(fontsize=28))
t2<-textGrob(expression(italic("N. Ahlgren, Clark University")),gp=gpar(fontsize=12),hjust=1,x=1)



#grid.arrange(p1,p2,p3,p4,nrow=2)
#grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3,top="COVID-19 case data for Worcester, MA")
#grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3,top=t,bottom=t2)
#grid.arrange(p5,p6,nrow=1)

#jpeg("Worcester_COVID.jpg",width=600,height=800)
#grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3,top=t,bottom=t2)
#dev.off()

jpeg("Worcester_COVID_v2.jpg",width=800,height=800)
grid.arrange(p1,p2,p3,p4,nrow=2,top=t,bottom=t2)
dev.off()

jpeg("Worcester_COVID_cases_deaths.jpg",width=600,height=400)
grid.arrange(p7,nrow=1)
dev.off()

jpeg("Worcester_COVID_7daymean_cases_hospitalizations.jpg",width=600,height=400)
grid.arrange(p8,nrow=1)
dev.off()

