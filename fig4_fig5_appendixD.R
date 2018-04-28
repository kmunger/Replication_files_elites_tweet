##hashtag analysis: code for Figures 4 and 5 and hashtags in Appendix D 



library("topicmodels")
library("SnowballC")
library("slam")
library("tm")
library("XML")
library("koRpus")
library("tm")
library("foreign")
library("RTextTools")
library("ggplot2")
library("quanteda")
library("reshape2")
library("dplyr")
library("lubridate")


##
oad("anonymized_tweets.Rdata")

gov<-dictDfm[1:162,]
opp<-dictDfm[163:324,]


##initialize: hashtags at least 20 characters long

gov.means<-rep(0,162)
opp.means<-rep(0,162)

for (i in 1:162){
  gov4<-gov[i,]
  opp4<-opp[i,]
  
  #sum up use of each hashtag
  govhash<-apply(gov4, 2, sum)
  opphash<-apply(opp4, 2, sum)
  
  ##sort from most to least
  sortgov<-govhash[order(govhash, decreasing=TRUE)]
  sortopp<-opphash[order(opphash, decreasing=TRUE)]
  
  #restrict to the ones that exist in this document
  sortgov<-sortgov[sortgov>0]
  sortopp<-sortopp[sortopp>0]
  
  ##restrict based on number of characters
  
  govs<-which(nchar(names(sortgov)) >19)
  opps<-which(nchar(names(sortopp)) >19)
  
  
  govs[length(govs)==0] <- 0
  opps[length(opps)==0] <- 0
  
  for (j in 1:length(govs)){
    k<-govs[j]
    kk<-as.numeric(sortgov[k])
    kk[length(kk)==0]<-0
    
    gov.means[i]<- gov.means[i]+ kk
  }
  
  for (p in 1:length(opps)){
    l<-opps[p]
    ll<-as.numeric(sortopp[l])
    ll[length(ll)==0]<-0
    
    opp.means[i]<- opp.means[i]+ll
  }
}


##PLOT 
index<-seq(1:162)

dat<-data.frame(days, gov.means, opp.means)
alldens.mm = melt(dat, id.vars ="days", measure.vars = c("opp.means","gov.means"))
alldens.mm$Faction<-alldens.mm$variable
levels(alldens.mm$Faction)[levels(alldens.mm$Faction) =="opp.means"]<-"Opposition"
levels(alldens.mm$Faction)[levels(alldens.mm$Faction) =="gov.means"]<-"Government"


pdf("fig4a.pdf", 7, 5)
{ggplot(alldens.mm, aes(y=value, x=days, pch = Faction, linetype = Faction)) + geom_point()  +
    stat_smooth(span=.5, se=TRUE) + theme_bw() +
    xlab(NULL) + ylab("Number of #'s at Least 20 Characters Long")  + scale_shape_manual(values=c(1,4))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept=as.numeric(days[58])) +
    geom_vline(xintercept=as.numeric(days[145]))  +
    geom_vline(xintercept=as.numeric(days[115]), linetype=2) 
  
}
dev.off()


##initialize: all hashtags

gov.means<-rep(0,162)
opp.means<-rep(0,162)

for (i in 1:162){
  gov4<-gov[i,]
  opp4<-opp[i,]
  
  #sum up use of each hashtag
  govhash<-apply(gov4, 2, sum)
  opphash<-apply(opp4, 2, sum)
  
  ##sort from most to least
  sortgov<-govhash[order(govhash, decreasing=TRUE)]
  sortopp<-opphash[order(opphash, decreasing=TRUE)]
  
  #restrict to the ones that exist in this document
  sortgov<-sortgov[sortgov>0]
  sortopp<-sortopp[sortopp>0]
  
  ##restrict based on number of characters--CHANGE THIS FOR THE DIFFERENT GRAPHS
  
  govs<-which(nchar(names(sortgov)) >0)
  opps<-which(nchar(names(sortopp)) >0)
  
  
  govs[length(govs)==0] <- 0
  opps[length(opps)==0] <- 0
  
  for (j in 1:length(govs)){
    k<-govs[j]
    kk<-as.numeric(sortgov[k])
    kk[length(kk)==0]<-0
    
    gov.means[i]<- gov.means[i]+ kk
  }
  
  for (p in 1:length(opps)){
    l<-opps[p]
    ll<-as.numeric(sortopp[l])
    ll[length(ll)==0]<-0
    
    opp.means[i]<- opp.means[i]+ll
  }
}


##PLOT 
index<-seq(1:162)

dat<-data.frame(days, gov.means, opp.means)
alldens.mm = melt(dat, id.vars ="days", measure.vars = c("opp.means","gov.means"))
alldens.mm$Faction<-alldens.mm$variable
levels(alldens.mm$Faction)[levels(alldens.mm$Faction) =="opp.means"]<-"Opposition"
levels(alldens.mm$Faction)[levels(alldens.mm$Faction) =="gov.means"]<-"Government"


pdf("fig4b.pdf", 7, 5)
{ggplot(alldens.mm, aes(y=value, x=days, pch = Faction, linetype = Faction)) + geom_point()  +
    stat_smooth(span=.5, se=TRUE) + theme_bw() +
    xlab(NULL) + ylab("Number of #'s")  + scale_shape_manual(values=c(1,4))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept=as.numeric(days[58])) +
    geom_vline(xintercept=as.numeric(days[145]))  +
    geom_vline(xintercept=as.numeric(days[115]), linetype=2) 
  
}
dev.off()





###fig5a: raw hashtag co-occurance 


gov_co<-cooccur[1:162]
opp_co<-cooccur[163:324]
##PLOT 
index<-seq(1:162)
dat<-data.frame(days, gov_co, opp_co)
alldens.mm = melt(dat, id.vars ="days", measure.vars = c("opp_co","gov_co"))
alldens.mm$Faction<-alldens.mm$variable
levels(alldens.mm$Faction)[levels(alldens.mm$Faction) =="opp_co"]<-"Opposition"
levels(alldens.mm$Faction)[levels(alldens.mm$Faction) =="gov_co"]<-"Government"


pdf("fig5a.pdf", 7, 5)
{ggplot(alldens.mm, aes(y=value, x=days, pch = Faction, linetype = Faction)) + geom_point()  +
    stat_smooth(span=.5, se=TRUE) + theme_bw() +
    xlab(NULL) + ylab("Number of raw # co-occurances")  + scale_shape_manual(values=c(1,4))+
    
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept=as.numeric(days[58])) +
    geom_vline(xintercept=as.numeric(days[145]))  +
    geom_vline(xintercept=as.numeric(days[115]), linetype=2) 
  
}
dev.off()




###fig5b: weighted hashtag co-occurance 





gov_co<-cooccur_w[1:162]
opp_co<-cooccur_w[163:324]
##PLOT 
index<-seq(1:162)
dat<-data.frame(days, gov_co, opp_co)
alldens.mm = melt(dat, id.vars ="days", measure.vars = c("opp_co","gov_co"))
alldens.mm$Faction<-alldens.mm$variable
levels(alldens.mm$Faction)[levels(alldens.mm$Faction) =="opp_co"]<-"Opposition"
levels(alldens.mm$Faction)[levels(alldens.mm$Faction) =="gov_co"]<-"Government"


pdf("fig5b.pdf", 7, 5)
{ggplot(alldens.mm, aes(y=value, x=days, pch = Faction, linetype = Faction)) + geom_point()  +
    stat_smooth(span=.5, se=TRUE) + theme_bw() +
    xlab(NULL) + ylab("Number of weighted # co-occurances")  + scale_shape_manual(values=c(1,4))+
    
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept=as.numeric(days[58])) +
    geom_vline(xintercept=as.numeric(days[145]))  +
    geom_vline(xintercept=as.numeric(days[115]), linetype=2) 
  
}
dev.off()



####### Code for Raw Hashtag Counts in Appendix D 


gov_raw
opp_raw

