# Started 18 January 2017 - Cat
## Removing duplicate rows and errors

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Load libraries
library(dplyr)
library(tidyr)

# Set working directory: 
setwd("~/Documents/git/ospree/analyses/output")
d<- read.csv("ospree_master_clean.csv")

# ghelardini10 issues - removed 8 rows not affiliated with study
for(i in d){
    df <- d[!(d$datasetID == "ghelardini10" & d$material == "root cuttings") &
             !(d$datasetID == "ghelardini10" & d$Entered.By == "DF"),]
}

# guerriero90 issues - changed respvar.simple from 'phenstageper.probonestudy' to 'leaves'
df <- within(df, respvar[datasetID== 'guerriero90' & respvar == 'percentstage06'] <- 'leaves')
df$respvar.simple[df$respvar == "leaves"] <- "othernums"

# jones12 issues
# phenstage01 - percentbudburst
# phenstage02 - leaves to othernums
# phenstage03 - fruits to othernums
# phenstage04 - flowers to percentflower
df <- within(df, respvar[datasetID== 'jones12' & respvar == 'percentstage01'] <- 'percentbudburst')
df$respvar.simple[df$respvar == "percentbudburst"] <- "percentbudburst"

df <- within(df, respvar[datasetID== 'jones12' & respvar == 'percentstage02'] <- 'leaves')
df$respvar.simple[df$respvar == "leaves"] <- "othernums"

df <- within(df, respvar[datasetID== 'jones12' & respvar == 'percentstage03'] <- 'fruits')
df$respvar.simple[df$respvar == "fruits"] <- "othernums"

df <- within(df, respvar[datasetID== 'jones12' & respvar == 'percentstage04'] <- 'flowers')
df$respvar.simple[df$respvar == "flowers"] <- "percentflower"

write.csv(df, "~/Documents/git/ospree/analyses/output/ospree_master_clean.csv", row.names = FALSE)
