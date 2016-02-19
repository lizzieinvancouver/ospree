# Budburst Lit review analysis.

library(gdata) # for read.xls
library(scales) # for alpha
library(lme4)
library(sjPlot) # visualizing fixed and random effects

#d <- read.xls("~/Dropbox/Work/Harvard/budburst review/Archive/growthchambers_litreview_2015-09-25.xlsx", sheet = 6)

setwd("~/Documents/git/budreview")

# Run the prep script (slow, uncomment if haven't run in a while)
# source("Prep_all_data.R") 

d <- read.csv("growthchambers_litreview_clean1.csv") # after running Jehane's code (and eventually Beth and Ailene's, too)

d <- d[2:51]
# response ~ photo + temp + (1|species) + (1|study) 

# summary(d)

d$photoperiod_day <- sub("continuous", 24, d$photoperiod_day)
d$photoperiod_day <- sub("ambient", "", d$photoperiod_day)
d$photoperiod_day <- sub("shortday", 10, d$photoperiod_day)
d$photoperiod_day <- sub("longday", 14, d$photoperiod_day)

d$photoperiod_day <- as.numeric(as.character(d$photoperiod_day))


d$forcetemp <- sub("ambient", "", d$forcetemp)
d$forcetemp <- sub("24, 13", 24, d$forcetemp)
d$forcetemp <- sub("meandaily", "", d$forcetemp)

d$forcetemp <- as.numeric(as.character(d$forcetemp))


# data.frame(table(d$respvar))

d$respvar <- sub("days to budbreak (on 50% of plants)", "daystobudburst", d$respvar)
d$respvar <- sub("daysto50perbudburst", "daystobudburst", d$respvar)

d$respvar <- sub("daystodudburst", "daystobudburst", d$respvar)
d$respvar <- sub("daystobudbust", "daystobudburst", d$respvar)


d$response = as.numeric(as.character(d$response))
# one approach: scale each response within study, look at general response
scaledresp <- vector()
for(i in unique(d$respvar)){ # i = "percentbudburst"
  dx <- d[d$respvar == i,]
  
  scaledresp <- c(scaledresp, scale(dx$response))
  
}

d$scaledresp = scaledresp

##########################

m1 <- lm(scaledresp ~ forcetemp * photoperiod_day, data = d)

summary(m1)

d$sp <- paste(d$genus, d$species)
d$exp <- paste(d$datasetID, d$study)


d$response.time <- as.numeric(as.character(d$response.time))
d$forcetemp <- as.numeric(as.character(d$forcetemp))


m2 <- lmer(response ~ forcetemp * photoperiod_day + (1 | sp) + (1|exp), data = d[d$respvar == "percentbudburst",])

summary(m2)

sjp.lmer(m2, type = "re")

sjp.lmer(m2, type = "fe")

sjt.lmer(m2)

# Same, for day to budburst. Response or response.time? .time

m2 <- lmer(response.time ~ forcetemp * photoperiod_day + (1 | sp) + (1|exp), data = d[d$respvar == "daystobudburst",])

summary(m2)

sjp.lmer(m2, type = "re")

sjp.lmer(m2, type = "fe")

sjt.lmer(m2)

m3 <- lmer(response.time ~ forcetemp * photoperiod_day + (1|sp) + (1|datasetID), data = d)

summary(m3)

sjp.lmer(m3, type = "re")

sjp.lmer(m3, type = "fe")

# Now add latitude, chilling

d$provenance.lat = as.numeric(as.character(d$provenance.lat))
d$chilltemp = as.numeric(as.character(d$chilltemp))
d$chilldays = as.numeric(as.character(d$chilldays))

m4 <- lmer(response ~ forcetemp * photoperiod_day + chilltemp + chilldays + abs(provenance.lat) + (1 | sp) + (1|exp), data = d[d$respvar == "daystobudburst",])

summary(m4)
sjp.lmer(m4,type='fe')

m4 <- lmer(response.time ~ forcetemp * photoperiod_day + (1|sp), data = d)

summary(m4)

sjp.lmer(m4, type = "re")

sjp.lmer(m4, type = "fe")


####### How many studies have multiple chilling levels, and are they close to ours?
hist(d$forcetemp)
hist(d$photoperiod_day)
