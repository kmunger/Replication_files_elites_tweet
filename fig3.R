##LDA analysis: code for Figure 3

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
jss_TM<-list(Gibbs = LDA(mat, k = k, method = "Gibbs",  control = list(seed = SEED, burnin = 300,thin = 30, iter = 300)))


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





###only look at the most semantically coherent topics -- function adapted from the STM package


semCoh1beta <- function(mat, M, beta){
  #Get the Top N Words
  top.words <- apply(beta, 1, order, decreasing=TRUE)[1:M,]
  wordlist <- unique(as.vector(top.words))
  mat <- mat[,wordlist]
  mat$v <- ifelse(mat$v>1, 1,mat$v) #binarize
  
  #do the cross product to get co-occurences
  cross <- tcrossprod_simple_triplet_matrix(t(mat))
  
  #create a list object with the renumbered words (so now it corresponds to the rows in the table)
  temp <- match(as.vector(top.words),wordlist)
  labels <- split(temp, rep(1:nrow(beta), each=M))
  
  #Note this could be done with recursion in an elegant way, but let's just be simpler about it.
  sem <- function(ml,cross) {
    m <- ml[1]; l <- ml[2]
    log(.01 + cross[m,l]) - log(cross[l,l] + .01)
  }
  result <- vector(length=nrow(beta))
  for(k in 1:nrow(beta)) {
    grid <- expand.grid(labels[[k]],labels[[k]])
    colnames(grid) <- c("m", "l") #corresponds to original paper
    grid <- grid[grid$m > grid$l,]
    calc <- apply(grid,1,sem,cross)
    result[k] <- sum(calc)
  }
  return(result)
}

semcoh<- semCoh1beta(mat, 10, jss_TM[["Gibbs"]]@beta)

#store topic-document proportions

lda<-jss_TM[["Gibbs"]]@gamma




##restrict to only the most coherent topics
##inspect plot, see the gap at -150
plot(semcoh)


indices<-which(semcoh > -150)


lda<-lda[,indices]

lda<-t(lda)

##reweight to reflect the fact that we're looking at fewer topics


reweight<-function(vec){
  tot<-sum(vec)
  out<-vec/tot
  return(out)
}

reweighted<-apply(lda, 2 , reweight)

nd<-apply(reweighted, 2, shanDiv)
##make sure all proportions sum to 1 

test<-apply(reweighted, 2, sum) ##YES IT SUMS TO 1


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
pdf("fig3.pdf", 7, 5)



ggplot(alldens.mm, aes(y=value, x=days, pch = Faction, linetype=Faction)) + geom_point() + 
  
  xlab(NULL) + ylab("Shannon Entropy") + scale_shape_manual(values=c(18, 1))+
  stat_smooth(span=.6, se=TRUE)+ theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_vline(xintercept=as.numeric(days[58])) +
  geom_vline(xintercept=as.numeric(days[145]))  +
  geom_vline(xintercept=as.numeric(days[115]), linetype=2) 




dev.off()

