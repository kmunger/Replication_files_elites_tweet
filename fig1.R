##Graphing Tweet Density: code for Figure 1

library("reshape2")
library("foreign")
library("RTextTools")
library("ggplot2")
library("lubridate")
library("dplyr")

load("anonymized_tweets.Rdata")

##meld data
alldens.m = melt(alldens, id.vars ="days", measure.vars = c("oppdensity.Freq","govdensity.Freq"))
alldens.m$Faction<-alldens.m$variable
levels(alldens.m$Faction)[levels(alldens.m$Faction) =="oppdensity.Freq"]<-"Opposition"
levels(alldens.m$Faction)[levels(alldens.m$Faction) =="govdensity.Freq"]<-"Regime"


##make plot

pdf("fig1.pdf", 7, 5)

{ggplot(alldens.m, aes(y=value, x=days, pch = Faction, linetype=Faction)) + geom_point()  +
   stat_smooth(span=.3, se=FALSE) +
    xlab(NULL) + ylab("Frequency")  + scale_shape_manual(values=c(18, 1)) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_vline(xintercept=as.numeric(days[58])) +
  geom_vline(xintercept=as.numeric(days[145]))  +
  geom_vline(xintercept=as.numeric(days[115]), linetype=2) 

}
dev.off()





