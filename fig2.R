####Make LL Table: Figure 2

##code based on code by Pablo Barbera; any errors are KM's


library("reshape2")
library("foreign")
library("RTextTools")
library("ggplot2")
library("lubridate")
library("dplyr")
library("cvTools")
library("topicmodels")
setwd('C:/Users/kevin/Dropbox/Social_Media_Lab/Lab_Papers_in_Progress/Venezuela_Kevin_Munger/elites_tweet_replication/anonymized')

load("anonymized_tweets.Rdata")

####this was initially taken from package cvTools; it doesn't appear to be in the current version

cvLDA <- function(Ntopics,dtm,K=10) {
   folds<-cvFolds(nrow(dtm),K,1)
   perplex <- rep(NA,K)
   llk <- rep(NA,K)
   for(i in unique(folds$which)){
     cat(i, " ")
     which.test <- folds$subsets[folds$which==i]
     which.train <- {1:nrow(dtm)}[-which.test]
     dtm.train <- dtm[which.train,]
     dtm.test <- dtm[which.test,]
     lda.fit <- LDA(dtm.train, k=Ntopics, method="Gibbs",
         control=list(verbose=50L, iter=100))
     perplex[i] <- perplexity(lda.fit,dtm.test)
     llk[i] <- logLik(lda.fit)
   }
   return(list(K=Ntopics,perplexity=perplex,logLik=llk))
 }


K <- c(5, 10, 15, 20, 25, 30, 40, 50, 75, 100)

results <- list()

i = 1
for (k in K){
    cat("\n\n\n##########\n\n\n ", k, "topics", "\n\n\n")
    res <- cvLDA(k, mat)
    results[[i]] <- res
#    save(results, file="data_out/k_topics_results_cv.Rdata")
    i = i + 1
}

#save(results, file="data_out/k_topics_results_cv.Rdata")

## plot

df <- data.frame(
    k = rep(K, each=10),
    perp =  unlist(lapply(results, '[[', 'perplexity')),
    loglk = unlist(lapply(results, '[[', 'logLik')),
    stringsAsFactors=F)

min(df$perp)
df$ratio_perp <- df$perp / max(df$perp)
df$ratio_lk <- df$loglk / min(df$loglk)

df <- data.frame(cbind(
    aggregate(df$ratio_perp, by=list(df$k), FUN=mean),
    aggregate(df$ratio_perp, by=list(df$k), FUN=sd)$x,
    aggregate(df$ratio_lk, by=list(df$k), FUN=mean)$x,
    aggregate(df$ratio_lk, by=list(df$k), FUN=sd)$x),
    stringsAsFactors=F)
names(df) <- c("k", "ratio_perp", "sd_perp", "ratio_lk", "sd_lk")

library(reshape)
pd <- melt(df[,c("k","ratio_perp", "ratio_lk")], id.vars="k")
pd2 <- melt(df[,c("k","sd_perp", "sd_lk")], id.vars="k")
pd$sd <- pd2$value
levels(pd$variable) <- c("Perplexity", "LogLikelihood")

library(ggplot2)
library(grid)
pdf("fig2.pdf", 7, 5)
ggplot(pd, aes(x=k, y=value, linetype=variable)) + 
 geom_line() + geom_point(aes(shape=variable), 
        fill="white", shape=21, size=1.40) +
    geom_errorbar(aes(ymax=value+sd, ymin=value-sd)) +
    scale_y_continuous("Ratio wrt worst value") +
    scale_x_continuous("Number of topics", 
        breaks=c(5, 10, 15, 20, 25, 30, 40, 50, 75, 100)) +
    theme_bw() +
    scale_shape_discrete(guide="none") +
    scale_linetype_discrete(guide="none") +
        theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.key.size=unit(0.5, "cm"), legend.position=c(0.70,.90), 
    legend.box.just = "left", legend.direction="horizontal",
    legend.title=element_blank()) +
        ggplot2::annotate(geom="text", x=89, y=0.89, label="Perplexity", size=3) +
        ggplot2::annotate(geom="text", x=90, y=0.97, label="logLikelihood", size=3)

dev.off()





