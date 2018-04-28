##CTM analysis: code for Figure F in Appendix

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
library("stm")
load("anonymized_tweets.Rdata")



##prep documents
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 0)


###run CTM 
fitCTM <- stm(out$documents,out$vocab, K=100, init.type="LDA",  max.em.its=100, data=out$meta, seed=5926696)


###only look at the most semantically coherent topics


semcoh<- semanticCoherence(fitCTM, out$documents, 20)


##apply the same semantic coherence cutoff as in LDA: -150


indices<-which(semcoh >  -150)


##define Shannon Diversity

shanDiv <- function( pVec, zeroPad = 1e-100 ) {  
  psum <- sum( pVec )
  zeros <- which(pVec == 0)
  if (length(zeros) > 0) {
    ##print( "you can't take log of 0\n")
    pVec[zeros] <- zeroPad
  }
  if (psum != 1) {
    print( "your probabilities don't sum to 1 \n")
  }
  hvec <- pVec * log( pVec, base = 2)
  h <- -1.0 * sum( hvec )
  return( h )
}



##only keep the topics with high semcoh
ctm<-fitCTM$theta[,indices]

ctm<-t(ctm)

##calculate shannon diversity
nd<-apply(ctm, 2, shanDiv)



##reweight to reflect the fact that we're looking at fewer topics


reweight<-function(vec){
  tot<-sum(vec)
  out<-vec/tot
  return(out)
}

reweighted<-apply(ctm, 2 , reweight)

nd<-apply(reweighted, 2, shanDiv)



#divide into coalitions and by time
govgammas<-nd[1:162]
oppgammas<-nd[163:324]
index<-seq(1:162)




dat<-data.frame(days, govgammas, oppgammas)
alldens.mm = melt(dat, id.vars ="days", measure.vars = c("govgammas","oppgammas"))
alldens.mm$Faction<-alldens.mm$variable
levels(alldens.mm$Faction)[levels(alldens.mm$Faction) =="oppgammas"]<-"Opposition"
levels(alldens.mm$Faction)[levels(alldens.mm$Faction) =="govgammas"]<-"Regime"




###Plot results
pdf("figF.pdf", 7, 5)



{ggplot(alldens.mm, aes(y=value, x=days, pch = Faction, linetype=Faction)) + geom_point() + 
  
  xlab(NULL) + ylab("Shannon Entropy") + scale_shape_manual(values=c(18, 1))+
  stat_smooth(span=.6, se=TRUE)+ theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_vline(xintercept=as.numeric(days[58])) +
  geom_vline(xintercept=as.numeric(days[145]))  +
  geom_vline(xintercept=as.numeric(days[115]), linetype=2) 

}


dev.off()
