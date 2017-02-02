# Started 2 Feb 2017 ##
# By Ailene ##
#Cleaning/fixing mistakes in chilltemp column (these are just fixing simple mistakes or cleaning obvious typos)

#This code fixes a mistake in the database, where things were misentered in the chilltemp column
d$chilltemp[d$datasetID=="junttila12" & d$figure.table..if.applicable. == "table2"] <- ""#table 2 shows unchilled data= this was misenetered as chilltemp=4 previously

