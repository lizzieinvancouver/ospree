#code to check clean_bbperctodays

dim(d[d$datasetID=="Sanz-Perez09",])#151 rows before running source("bb_analysis/cleaning/clean_bbperctodays.R") # As of 12 Dec 2018: 7385 rows
dim(d.subset[d.subset$datasetID=="Sanz-Perez09",])#151 rows before running source("bb_analysis/cleaning/clean_bbperctodays.R") # As of 12 Dec 2018: 7385 rows
#now only 8 rows- does that seem right?- YES! should be 8 rows for figure 1, which has 2 species x 2 daylengths x 2 photoperiods
d<-d[d$datasetID=="Sanz-Perez09", ]


#run clean_bbperctodays line by line to figure out where mistake is
target.percent=targetvalue