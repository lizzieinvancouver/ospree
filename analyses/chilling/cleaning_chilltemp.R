#Cleaning columns for chilling calculations:fieldsample.date, chilltemp, and chilldays###############################################
#Started by Ailene January 30, 2017
#Add field sample date to those columns for which it was not entered
d[which(d$datasetID=="cook05" & d$figure.table=="fig 1"),]$fieldsample.date<-"7-Jul-1999"
d[which(d$datasetID=="laube14a"),]$year<-2011
d[which(d$datasetID=="laube14a" & d$chilldays==33),]$fieldsample.date<-"14-Dec-2011"
d[which(d$datasetID=="laube14a" & d$chilldays==73),]$fieldsample.date<-"30-Jan-2012"
d[which(d$datasetID=="laube14a" & d$chilldays==110),]$fieldsample.date<-"14-Mar-2012"
d[which(d$datasetID=="laube14a"),]$continent<-"europe"
d[which(d$datasetID=="thielges75"),]$year<-1974
d[which(d$datasetID=="thielges75"),]$continent<-"Africa"
d[which(d$datasetID=="thielges75"),]$fieldsample.date<-"30-Sep-1974"
d[which(d$datasetID=="zohner16"),]$continent<-"europe"
d[which(d$datasetID=="basler12"),]$fieldsample.date<-"3-Mar-2009"
d[which(d$datasetID=="charrier11" & d$study=="exp2"),]$fieldsample.date<-"29-Nov-2008"#charrier11 (exp 2 only; exp 1 is actually field observations) -- they don't really say but they are doing phenological observations in spring 2008 and 2009 and they clipped on 29 November, so probably 29 Nov 2008 is field sample date
d[which(d$datasetID=="karlsson03" & d$chilldays==15),]$fieldsample.date<-"15-Jan-2001"#
d[which(d$datasetID=="karlsson03" & d$chilldays==45),]$fieldsample.date<-"15-Feb-2001"#
d[which(d$datasetID=="karlsson03" & d$chilldays==75),]$fieldsample.date<-"15-Mar-2001"#
d[which(d$datasetID=="karlsson03" & d$chilldays==105),]$fieldsample.date<-"15-Apr-2001"#
d[which(d$datasetID=="karlsson03" & d$chilldays==130),]$fieldsample.date<-"15-May-2001"#
d[which(d$datasetID=="karlsson03"),]$continent<-"europe"#

#This code fixes a mistake in the database, where things were misentered in the chilltemp column
d$chilltemp[d$datasetID=="junttila12" & d$figure.table..if.applicable. == "table2"] <- ""#table 2 shows unchilled data= this was misenetered as chilltemp=4 previously
#Making some non-numeric entries in chilltemp and chilldays usable
d[which(d$datasetID=="skre08" & d$chilltemp=="High"),]$chilltemp<-"ambient + 4"#this is skre08 + 4 degrees C; does not specify
d[which(d$datasetID=="skre08" & d$chilltemp=="Low"),]$chilltemp<-"ambient"#this is skre08
d[which(d$datasetID=="skuterud94" & d$chilltemp=="mean of 0, 3, 6"),]$chilltemp<-mean(c(0,3))#this is skuterud94, and there is a mistake in the way the data were entered- these data are from figure 5 in the paper, which is just mean of 0 and 3 C treatments, so i list the mean of these two
d[which(d$chilldays=="40,55,60 (mean across these presented)"),]$chilldays<-mean(c(40,55,60))#skuterud94- did the simple, shortcut here- instead could calculate the chilling first, then take the mean chilling...
d[which(d$chilldays=="mean of 39, 55, 67, 98, 111"),]$chilldays<-mean(c(39,55,67,98,111))#skuterud94- did the simple, shortcut here- instead could calculate the chilling first, then take the mean chilling...
d[which(d$chilltemp=="negative 23 to 13 degrees Celsius"),]$chilltemp<-"ambient"#partanen01, after re-reading paper, the plants were just exposed to ambient conditions (which ranged from negative 23 to 13 degrees Celsius)
d[which(d$chilltemp=="elevated"),]$fieldchill<-"yes"#pagter15, this 
d[which(d$chilltemp=="elevated"),]$chilltemp<-"ambient + 0.76"#pagter15, amount of air warming reported in paper
d[which(d$chilltemp=="ambient plus days at 4C"),]$chilltemp<-4#okie11 exp2
d[which(d$chilltemp=="<16"),]$chilltemp <- "ambient" #falusi90 - per lab meeting discussion
d[which(d$chilltemp==">16"),]$chilltemp <- 16 #falusi90 - per lab meeting discussion

#Add new column to flag when chilling should be calculated  "by hand" instead of using simple chilling calculated or ambient climate (or after ambient climate for "amibent +" studies)
d$chillbyhand<-0
d[which(d$chilltemp=="neg 3,2"),]$chillbyhand<-1 #man10 - seedlings were chilled for one month at -3, and one month at 2
d[which(d$chilltemp=="neg 3,2"),]$chilldays<-60 #man10 - both chilltemp treatments occured for one month for a total of 60 chill days- need to do this to make it numeric
d[which(d$chilltemp=="neg 3,2"),]$chilltemp<-"-3,2" #man10 - 
d[which(d$chilltemp=="6,.5"),]$chillbyhand<-1 #li05 - seedlings were chilled for 3 weeks at 6, and 3 weeks at 0.5
d[which(d$chilltemp=="6,.5"),]$chilldays<-42 #li05- both chilltemp treatments occured for 21 chill days for a total of 42 days- need to do this to make it numeric
d[which(d$chilltemp=="-3, 3"),]$chillbyhand<-1 #lamb37 - 26.6F for 8 hr and 37.4 for 16 hr -
d[which(d$chilltemp=="8, 8, -4"),]$chillbyhand<-1 #jones12 Cuttings were exposed to either a 6-week or a 12-week chillingperiod, with each period split into three equal parts at one of the treatment temperatures (-4, 0, 4 or 8C).
d[which(d$chilltemp=="8, 4, 0"),]$chillbyhand<-1 #jones12
d[which(d$chilltemp=="4, 0, -4"),]$chillbyhand<-1 #jones12
d[which(d$chilltemp=="0, 4, 8"),]$chillbyhand<-1#jones12
d[which(d$chilltemp=="-4, 8, 8"),]$chillbyhand<-1#jones12
d[which(d$chilltemp=="-4, 0, 4"),]$chillbyhand<-1 #jones12
d[which(d$chilltemp=="Chilling treatment at 0.7 \xb1 0.7 C interrupted by mild spells of 14 days duration at a constant temperature of 8 or 12 C"),]$chilldays<-161#granhus09: the chilling for these sites were entered in wrong. AFter re-reading the paper, Lizzie and Ailene realized that all treatments had a 161 (=175-1) day chilling periodignoring this for now since it shouldn't affect chilling calculations.


stop("Not an error, just stopping here to say we're now done cleaning the chilltemp column. The d item in your workspace is now all cleaned up and ready to pull climate data in to estimate field chilling. Yay!")
