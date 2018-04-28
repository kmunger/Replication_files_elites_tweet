README:

This repository contains R files to produce each of the graphs in "Elites Tweet to get Feet off the Streets: Measuring Regime Social Media Strategies During Protest". Twitter's Terms of Service prohibits large quantities of raw tweet data from being publicly uploaded, so the file "make_dfm_density.R" takes the raw Twitter data and outputs "anonymized_tweets.Rdata". This file *cannot* be run because the raw tweets are not included in this repository; it is included here so that the text pre-processing steps used to create downstream measures are transparent. 

This also means that Table 1 in the manuscript cannot be replicated with the data provided in this repository; Twitter's Terms of Service prohibits large quantities of raw tweet data from being publicly uploaded, so we cannot provide the tweets from each account under study.

Each other R file takes as its input "anonymized_tweets.Rdata" and outputs the Figure(s) named in the title. They can be run in any order.
