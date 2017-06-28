###The purpose of this script is to find out why we loose data based on forcing for bb analysis
### began June 19, 2017 by Dan
###first run bb_analysis.R to line 51

3485-3077 #=408 rows lost due to lack of forcing
lost.force<-subset(bb, is.na(force)==TRUE)
unique(lost.force$datasetID) ##11 studies
##Not recoverable###############
#"ashby62" Forcing not reported, experiment conducted in greenhouse   
# "skuterud94"  mean of 9, 12, 15, we don't know which individuals were in which chambers base on figure 4.

#####Possibly recoverable from climate data#############################################
#"basler12":meandaily: Temperature was set to cycle±5K around the daily mean temperature, which was increased by0.5K every five days, 
###could maybe extract from figure 1 or climate data
#"fu13" extract from climate data ((Belgium, 51◦19N, 4◦21E)
#"gunderson12" extract ambient from climate data: Oak Ridge, TN, USA (35°54′N, 84°20′W
#"hawkins12":maybe extract from climate but would need to know which individuals were in each garden
###Victoria, BC (southern and coastal;48829?N and 123824?W), Skimikin, BC (central andinterior; 50847?N and 119824?W), and PrinceGeorge, BC (northern and interior; 53845?N and 122843?W).
#"lamb37": could maybe extract climate for University of Minnesota st Paul but seems a huge pain     
#"morin10"could extract from Montpellier, France (4364¢N, 386¢E,   ) 
#"falusi96"  extract ambient temperature for Florence (43°45’N),
#guak98"  could extract abient and elevated T from paper or for corvalis
#sanzperez10"extract from climate data at (Torremocha del Jarama, Madrid, Central Spain, 40500N, 3290W),

