##################NOTE########################################
#

#         This file cannot be run becuase the raw Twitter data are not in this repository

#         We include this file so that all pre-processing that leads to 

#         R data objects used in other files is transparent



##################END NOTENOTE########################################

##Take raw tweets (not posted here in keeping with Twitter's Terms of Service ) and create tweet density counts

## create general DFM

##create hashtag-specific DFM

##calculate hashtag co-occurances
library("topicmodels")
library("SnowballC")
library("slam")
library("tm")
library("stm")
library("koRpus")
library("tm")
library("foreign")
library("RTextTools")
library("ggplot2")
library("quanteda")
library("reshape2")
library("dplyr")
library("lubridate")
library("stringr")

  
##Grabbin and aranging tweets
tweets<-read.csv("tweets.csv", stringsAsFactors = F)


tweets$day<-substring(tweets$time, 1,10)
tweets$day<-as.Date((tweets$day), "%m/%d/%Y")

##create a vector of dates

days<-as.Date(unique(tweets$day), "%m/%d/%Y")

days<-sort(days)

#divide into gov and opposition for density
gov<-tweets[tweets[, "coalition"] == "GOVERNMENT",]
opp<-tweets[tweets[, "coalition"] == "OPPOSITION",]


govdensity<-as.data.frame(table(gov$day))


oppdensity<-as.data.frame(table(opp$day))


alldens<-data.frame(days, oppdensity$Freq, govdensity$Freq)



##divide into coalitions text

gov_txt<-character()
opp_txt<-character()
for( i in 1:length(days)){
  opp_txt[i]<-paste0(tweets$raw.text[tweets$day==days[i] & tweets$coalition=="OPPOSITION"], collapse = " ")
  gov_txt[i]<-paste0(tweets$raw.text[tweets$day==days[i] & tweets$coalition=="GOVERNMENT"], collapse = " ")
  
}


txt<-c(gov_txt, opp_txt)


##create matrix
mat <- create_matrix(txt
,  stemWords=TRUE,  language="spanish", removeStopwords=TRUE, removeNumbers=TRUE,
removePunctuation = TRUE)


###create dictionary DFM of just hashtags
dictDfm <- dfm(txt, select="^#\\w+\\b", valuetype = "regex" , remove_twitter=FALSE)

gov_dict<-dictDfm[1:162,]
opp_dict<-dictDfm[163:324,]

############create DFM for CTM model in appendix
##days
days_double<-days


days_double[163:324]<-days


#create DF for stm pre-processing
data<-data.frame( txt, days_double)


##pre-process text

processed <- textProcessor(data$txt, metadata=data, language="spanish", stem=T, removestopwords=T, removenumbers=T, striphtml = TRUE)



#sort each faction by hashtag popularity--for terms in Appendix D 


gov_sums<-apply(gov_dict, 2,sum)
gov_sums<-sort(gov_sums, decreasing=TRUE)
gov_raw<-gov_sums[1:100]

gov_raw

opp_sums<-apply(opp_dict, 2,sum)
opp_sums<-sort(opp_sums, decreasing=TRUE)
opp_raw<-opp_sums[1:100]

opp_raw

###look at raw tweets to calculate hashtag co-occurance

tweets_hash<-filter(tweets, grepl("#", tweets$raw.text))


cooccur<-vector()
coalitions = c("GOVERNMENT", "OPPOSITION" )

for(j in 1:2){
  
  for( i in 1:length(days)){
    day_tweets<-tweets_hash$raw.text[tweets_hash$day==days[i] & tweets_hash$coalition==coalitions[j]]
    
    
    cooccur[i + (162*(j-1))] <-   length(which(str_count(day_tweets, "#")>=2) )
    
    
  }
  
}


###look at raw tweets to calculate weighted hashtag co-occurance 

cooccur_w<-vector()
coalitions = c("GOVERNMENT", "OPPOSITION" )

for(j in 1:2){
  
  for( i in 1:length(days)){
    day_tweets<-tweets_hash$raw.text[tweets_hash$day==days[i] & tweets_hash$coalition==coalitions[j]]
    
    ##this calculates the weighted cooccurances
    
    cooccur_w[i + (162*(j-1))]<-    length(which(str_count(day_tweets, "#")==2) ) * 2 + 
      length(which(str_count(day_tweets, "#")==3)) * 3 + 
      length(which(str_count(day_tweets, "#")==4)) * 4 +
      length(which(str_count(day_tweets, "#")==5)) * 5 + 
      length(which(str_count(day_tweets, "#")==6)) * 6 
    
  }
  
}

####DFM for placebo tweets

##Grabbin and aranging tweets
tweets<-read.csv("Placebotweets.csv", stringsAsFactors = F)


tweets$day<-substring(tweets$time, 1,10)
tweets$day<-as.Date((tweets$day), "%m/%d/%Y")

days_p<-as.Date(unique(tweets$day), "%m/%d/%Y")

days_p<-sort(days_p)


days_p<-days_p[which(days_p > as.Date("2014/5/28"))]

##divide into coalitions text

gov_txt<-character()
opp_txt<-character()
for( i in 1:length(days_p)){
  opp_txt[i]<-paste0(tweets$clean.text[tweets$day==days_p[i] & tweets$coalition=="OPPOSITION"], collapse = " ")
  gov_txt[i]<-paste0(tweets$clean.text[tweets$day==days_p[i] & tweets$coalition=="GOVERNMENT"], collapse = " ")
  
}


txt<-c(gov_txt, opp_txt)



##create matrix
mat_placebo <- create_matrix(txt
                     ,  stemWords=TRUE,  language="spanish", removeStopwords=TRUE, removeNumbers=TRUE,
                     removePunctuation = TRUE)









##delete originals
rm(list =c("gov", "opp", "govdensity", "oppdensity", "gov_txt", "opp_txt", "tweets", "txt", "i", "gov_sums", "opp_sums",
           "tweets_hash", "day_tweets", "data", "j"))



save.image(file = "anonymized/anonymized_tweets.Rdata")
