#Picea studies to check:
 # Basler 12- Long day advances bb
#Basler 14- ""

#Gomery 15--Baad
#Laube14a- Basically insensitive
#Partenan01- Also Wrong. There should be a shortening light treatment starting at 16 decrease 6 mins everyday, lengthening starting at 6 and increasing 6 minutes, and constant 6 hour. Not sure how we put this in
#Partenan98 Same problems as above.
#Worrall67- Seems fine, no strong effect
#Zohner16- no stronf effects


#run bbleadin.R
gom<-filter(bb.stan,datasetID=="gomory15")
ggplot(gom,aes(as.character(photo),resp))+geom_boxplot()+ggtitle("gomory: Picea problems")
