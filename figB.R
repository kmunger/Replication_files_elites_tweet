##LDA analysis: code for Appendix Figure B

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

load("anonymized_tweets.Rdata")


##run LDA
k <-100
SEED<-2010
jss_TM<-list(Gibbs = LDA(mat_placebo, k = k, method = "Gibbs",  control = list(seed = SEED, burnin = 300,thin = 30, iter = 300)))

##assign topic distribution
lda<-jss_TM[["Gibbs"]]@gamma



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

##apply diversity scores by day
lda<-t(lda)

nd<-apply(lda, 2, shanDiv)
###test<-apply(ndx, 2, sum) ##YES IT SUMS TO 1


#divide into coalitions and by time
govgammas<-nd[1:138]
oppgammas<-nd[139:276]


##plot
pdf("figB.pdf", 7, 5)

plot(days_p, govgammas, pch=3,  main="Placebo Check: Focus After the Protests ", ylim=c(.5, 3), ylab="Shannon Entropy" )
points(days_p, oppgammas, pch=5)
legend("topright",c("Government","Opposition"),  lty=c(1,1), pch=c(3, 5))
dev.off()

