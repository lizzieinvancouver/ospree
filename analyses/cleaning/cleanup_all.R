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

# viheraaarnio06 update respvar.simple from NA to percentbudset
df <- within(df, respvar.simple[datasetID== 'viheraaarnio06' & respvar == 'percentbudset'] <- 'percentbudset')

############# Now fix forcetemp columns ########################
# Using work from Liz Stebbins
setwd("~/Documents/git/ospree/analyses/output")
df<- read.csv("ospree_master_clean.csv")

# charrier11 - change ambient to 25 for both forcetemp and forcetemp_night
df <- within(df, forcetemp[datasetID== 'charrier11' & forcetemp == 'ambient'] <- 25)
# cronje03 - change blank to 25
df <- within(df, forcetemp[datasetID== 'cronje03' & forcetemp == ''] <- 25)
# falusi96 - change exp2 from blank to 16
df <- within(df, forcetemp[datasetID== 'falusi96' & study == 'exp2'] <- 16)
df <- within(df, forcetemp[datasetID== 'falusi96' & study == 'exp3'] <- 24)
# gomory15 - change from ambient to 4.9 (Svarin - 49 deg lat) or 9.2 (Hladomer - 48 deg lat)
df <- within(df, forcetemp[datasetID== 'gomory15' & growing.lat == 48.448201] <- 9.2)
df <- within(df, forcetemp[datasetID== 'gomory15' & growing.lat == 49.017914] <- 4.9)
# guerriero90 - changed blanks to 23
df <- within(df, forcetemp[datasetID== 'guerriero90' & forcetemp == ''] <- 23)
# schnabel87 - change ambient to 24
df <- within(df, forcetemp[datasetID== 'schnabel87' & forcetemp == 'ambient'] <- 24)
# yazdaniha64 - change ambient to ambient to 17
df <- within(df, forcetemp[datasetID== 'yazdaniha64' & forcetemp == 'ambient'] <- 17)

write.csv(df, "~/Documents/git/ospree/analyses/output/ospree_master_clean.csv", row.names = FALSE)
