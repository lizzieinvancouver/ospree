## Started 2 October 2017 ##
## By Lizzie (so far)##

## This code does some basic OSPREE cleaning that didn't fit anywhere else ##
## It is sourced in cleanmerge_all.R ##

# In July 2017 we realized we left year off the Zonher data, but we need it!
# So we’re adding it in here, it should be year of experiment, which is 2014. 
d$year[which(d$datasetID=="zohner16")] <- 2014

# Fix a table that was mis-referenced
d$figure.table..if.applicable.[which(d$datasetID=="campbell75" &
    d$figure.table..if.applicable=="table1")] <- "table2"

# Falusi97 is difficult, the data on X axis in Fig 1 is TRANSFER date, but
# you cannot tell how long they measured them for. Based on my reading I am 90% sure
# Fig 1 shows values at the end of 120 d (see main text below fig and note exp 2 was
# all 60 d) so correcting that here. I still would be cautious of using these data as
# text below fig 1 also refers to 'last chilling cutoff date (December 24)' but it is
# not possible to figure what chilling was (perhaps 'nursery' was outside?)
d$response.time[which(d$datasetID=="falusi97" & d$study=="exp1")] <- 120

#pop2000 (Dan) is wrong on many levels. 1) resp.var is actually percenttwigswithbudburst 
#2) response.time is actually chilling time
#I'm making those changes here, and also deleting the study from anaylsis in clean resp_varmore.R just incase it slips through
d$response.time[which(d$datasetID=="pop2000")] <- NA
d$respvar[which(d$datasetID=="pop2000")] <- "percentoftwigswithbudburst"
