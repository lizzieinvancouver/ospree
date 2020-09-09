#################################################
##### Source file for reading in the data #######
#################################################

## select out only the budburst data ...
respvars.thatwewant <- c("daystobudburst", "percentbudburst")
bb.resp <- d[which(d$respvar.simple %in% respvars.thatwewant),]
bb.resp <- subset(bb.resp, respvar != "thermaltime")

## make a bunch of things numeric
bb.resp$forceday <- as.numeric(bb.resp$forcetemp)
bb.resp$forcenight <- as.numeric(bb.resp$forcetemp_night)
bb.resp$photonight <- as.numeric(bb.resp$photoperiod_night)

bb.resp$photo <- as.numeric(bb.resp$photoperiod_day)
bb.resp$force <- bb.resp$forceday
bb.resp$force[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE] <-
    (bb.resp$forceday[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE]*
    bb.resp$photo[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE] +
    bb.resp$forcenight[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE]*
    bb.resp$photonight[is.na(bb.resp$forcenight)==FALSE & is.na(bb.resp$photo)==FALSE &
    is.na(bb.resp$photonight)==FALSE])/24

bb.resp$chill <- as.numeric(bb.resp$Total_Utah_Model) # before 12 March 2018: Total_Chilling_Hours, Total_Chill_portions
bb.resp$chill.hrs <- as.numeric(bb.resp$Total_Chilling_Hours) # before 12 March 2018: Total_Chilling_Hours, Total_Chill_portions
bb.resp$chill.ports <- as.numeric(bb.resp$Total_Chill_portions) # before 12 March 2018: Total_Chilling_Hours, Total_Chill_portions

bb.resp$resp <- as.numeric(bb.resp$response.time)

bb.noNA <- subset(bb.resp, is.na(force)==FALSE & is.na(photo)==FALSE &
                    is.na(chill)==FALSE & is.na(resp)==FALSE)
if(use.chillports) bb.noNA <- subset(bb.resp, is.na(force)==FALSE & is.na(photo)==FALSE &
                    is.na(chill.ports)==FALSE & is.na(resp)==FALSE)

# Vector needed to identify weinberger-design studies
if(FALSE){
weinberg<-c("falusi03", "falusi97", "heide93", "jones12", "partanen05", "ramos99",
            "ashby62","basler14","biasi12","boyer","calme94","charrier11","cook00b",
             "ganset02"  ,"gianfagna85","guerriero90","gunderson12")
}