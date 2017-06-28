## Started 28 June 2017 ##
## By Dan
 ### Run Latitude_Studies.R

osp2<-filter(osp, respvar=="daystobudburst")
unique(osp2$datasetID)
View(filter(osp2, datasetID=="heide12"))

##Dan should clean:
#"ashby62"-not multi lat
#"basler12"- more longitude and altitude manipulated
#"biasi12" not multi lat    
#"calme94" multi lats are actually just different species
# ****"campbell75" yes multi lat and multi alt
#"charrier11" slight lat difference in exp 1, mostly multi alt 
 # "gunderson12" different lats are for different species
#***"heide12" yes multi lat