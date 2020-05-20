library(ggplot2)
library(grid)
library(gridExtra)

d<-read.table(file="../Worcester_city.tsv",sep="\t",header=T)

# process dates
d$Date<-as.Date(d$Date)
d$cases<-as.numeric(d$cases)

### calculate new daily cases
d$dnewcases<-c(0,d$cases[c(2:length(d$cases))] - d$cases[c(1:(length(d$cases)-1))])
#d$dnewcases<-as.numeric(d$cases)

## Running Days
d$Days<-as.numeric(d$Date-d$Date[1])

## Weekly average of daily new cases
weekly.mean<-rep(NA,6)
for (i in c(7:length(d$cases)) ) {
	weekly.mean<-c(weekly.mean,mean(d$dnewcases[c((i-6):i)]))
}
d$weekly.newcases<-weekly.mean

### last 7 days + extend prediction by ex.d days
d.l<-dim(d)[1]
d.last7<-d[c((d.l - 6):d.l),]
ex.d<-14

lm_fit <- lm(cases ~ Date, data=d.last7)
#summary(lm_fit)
d.pred.last7 <- data.frame(cases_pred = predict(lm_fit, d.last7))
d.pred.last7$Date <- d.last7$Date

d.extend<-data.frame(Date = c(d.last7$Dat,(d.last7$Date[length(d.last7$Date)] + ex.d)))
d.extend$cases_pred<-predict(lm_fit,d.extend)


lm_fit <- lm(dnewcases ~ Date, data=d.last7)
#summary(lm_fit)
d.pred.last7$dnewcases_pred <- predict(lm_fit, d.last7)


### last 14 days
d.l<-dim(d)[1]
d.last14<-d[c((d.l - 13):d.l),]

lm_fit <- lm(cases ~ Date, data=d.last14)
#summary(lm_fit)
d.pred.last14 <- data.frame(cases_pred = predict(lm_fit, d.last14))
d.pred.last14$Date <- d.last14$Date
newcase.p.day<-as.character(round(lm_fit$coefficients[2],0))

lm_fit <- lm(dnewcases ~ Date, data=d.last14)
#summary(lm_fit)
d.pred.last14$dnewcases_pred <- predict(lm_fit, d.last14)



#plot(d$cases,d$Date)

#ggplot(data = d, aes(x = Days, y = cases)) +
#      geom_point() +
#      scale_y_log10() +#      labs(title = "Cumulative COVID-19 cases in Worcester, MA",#           x = "Days", y = "Cases")


d.legend<-data.frame(Date = d$Date[c(1,10)])
d.legend$cases<-0.9*c(max(d.extend$cases_pred),max(d.extend$cases_pred))

d.legend.2<-data.frame(Date = d$Date[c(1,10)])
d.legend.2$dnewcases<-0.9*c(max(d$dnewcases),max(d$dnewcases))

font.size=14
	
sun.start<-as.Date("2020/03/15")
suns<-c(2+seq(from=0,by=7,dim(d)[1]))
d.suns<-d[suns,]

  
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
           
p3 <- ggplot(data = d, aes(x = Date, y = dnewcases)) +
      geom_point(cex=3) +
	geom_line() +
      geom_point(data = d.suns, aes(x = Date, y = dnewcases), col='yellow', cex=3 ) +
	#      scale_y_log10() +
 	xlim(min(d$Date),max(d.extend$Date)) +
 	geom_line(color='red',data = d.pred.last14, aes(x=Date, y=dnewcases_pred),size = 2) +

	annotate("text", label = "2 week regression", x = min(d$Date+14), y = 0.9*max(d$dnewcases), colour = "red", hjust=0, size=6) +
	geom_line(color='red',data = d.legend.2, aes(x=Date,y=dnewcases),size = 2) +

	 theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size)) +#	 theme(plot.title = element_text(face = "bold")) +
      labs(title = "Daily new cases (linear scale)",
           x = "Date", y = "Daily new cases")           

p4 <- ggplot(data = d, aes(x = Date, y = dnewcases)) +
      geom_point(cex=3) +
      scale_y_log10() +
	geom_line() +
      geom_point(data = d.suns, aes(x = Date, y = dnewcases), col='yellow', cex=3 ) +
  	xlim(min(d$Date),max(d.extend$Date)) +
 	geom_line(color='red',data = d.pred.last14, aes(x=Date, y=dnewcases_pred),size = 2) +
	 theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size)) +
#	 theme(plot.title = element_text(face = "bold")) +
      labs(title = "Daily new cases (log scale)",
           x = "Date", y = "Daily new cases") 

p5 <- ggplot(data = d, aes(x = Date, y = weekly.newcases)) +
      geom_point(cex=3) +
#      scale_y_log10() +
#	geom_line() +
 	xlim(min(d$Date),max(d.extend$Date)) +
	 theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size)) +
#	 theme(plot.title = element_text(face = "bold")) +
      labs(title = "Weekly avg of daily\nnew cases (linear scale)",
           x = "Date", y = "Weekly mean of\ndaily new cases")           

p6 <- ggplot(data = d, aes(x = Date, y = weekly.newcases)) +
      geom_point(cex=3) +
      scale_y_log10() +
 	xlim(min(d$Date),max(d.extend$Date)) +      
	 theme(plot.title = element_text(hjust = 0.5), text=element_text(size=font.size)) +
#	 theme(plot.title = element_text(face = "bold")) +
      labs(title = "Weekly avg of daily\nnew cases (log scale)",
           x = "Date", y = "Weekly mean of\ndaily new cases") 





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
