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

# Sumarizing for climate work
# What years do we need at each continent?

tapply(d$year, toupper(d$continent), function(x) sort(unique(x)))

# Asia: 1958-1959, 1986-2013
# Europe: 1958-1959, 1971-present
# NA: 1936, 1947, 1957, 1959, 1963, 1972-present, 
# downloaded 1957, 1959, 1963, 1972
# SA: 2004-2009

# d <- d[2:51]
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

d$sp <- paste(d$genus, d$species)
d$exp <- paste(d$datasetID, d$study)

# data.frame(table(d$respvar))

d$respvar <- sub("days to budbreak (on 50% of plants)", "daystobudburst", d$respvar)
d$respvar <- sub("daysto50perbudburst", "daystobudburst", d$respvar)

d$respvar <- sub("daystodudburst", "daystobudburst", d$respvar)
d$respvar <- sub("daystobudbust", "daystobudburst", d$respvar)



d$response = as.numeric(as.character(d$response))
d$response.time <- as.numeric(as.character(d$response.time))
d$forcetemp <- as.numeric(as.character(d$forcetemp))

summary(d[d$respvar == "percentbudburst","response"])
# Get rid of 999's!!!!

d$response[which(d$response >= 999)] = NA

# one approach: scale each response within study, look at general response
scaledresp <- vector()
scaledrespt <- vector()

for(i in unique(d$exp)){ # i = "percentbudburst"
  dx <- d[d$exp == i,]
  for(j in unique(dx$respvar)){
    dxx <- dx[dx$respvar == j,]
    
    scaledresp <- c(scaledresp, scale(dxx$response))
    scaledrespt <- c(scaledrespt, scale(dxx$response.time))
  }
}

d$scaledresp = scaledresp
d$scaledrespt = scaledrespt

##########################
# Models: 
# 1. across all responses, no significant effect. Need to look for each response, within each study. Not meaningful if don't distinguish different types of responses

m1 <- lm(scaledresp ~ forcetemp * photoperiod_day, data = d)

summary(m1)

# 2. by species, by study, for just pct bb
m2bb <- lmer(scaledresp ~ forcetemp * photoperiod_day + (1 | sp) + (1|exp), data = d[d$respvar == "percentbudburst",])

summary(m2bb)

sjp.lmer(m2bb, type = "re")

sjp.lmer(m2bb, type = "fe", showIntercept = F)

sjt.lmer(m2bb)


# Same, for day to budburst. Response or response.time? .time

m2d <- lmer(scaledrespt ~ forcetemp * photoperiod_day + (1 | sp) + (1|exp), data = d[d$respvar == "daystobudburst",])

summary(m2d)

sjp.lmer(m2d, type = "re")

sjp.lmer(m2d, type = "fe", showIntercept = F)

sjt.lmer(m2d)

# trying with paper instead of study. Need different models for each response var, not pooled across
m3 <- lmer(scaledresp ~ forcetemp * photoperiod_day + (1|sp) + (1|exp) + (1|respvar), data = d)

summary(m3)

sjp.lmer(m3, type = "re")

sjp.lmer(m3, type = "fe", showIntercept = F)

# Now add latitude, chilling

d$provenance.lat = abs(as.numeric(as.character(d$provenance.lat)))
d$chilltemp = as.numeric(as.character(d$chilltemp))
d$chilldays = as.numeric(as.character(d$chilldays))

m4d <- lmer(response.time ~ forcetemp * photoperiod_day + chilltemp + chilldays + provenance.lat + (1 | sp) + (1|exp), data = d[d$respvar == "daystobudburst",])

summary(m4d)
sjp.lmer(m4d, type='fe', showIntercept = F)

m4bb <- lmer(response ~ forcetemp * photoperiod_day + chilltemp + chilldays + provenance.lat + (1 | sp) + (1|exp), data = d[d$respvar == "percentbudburst",])

summary(m4bb)
sjp.lmer(m4bb, type='fe', showIntercept = F)


####### How many studies have multiple chilling levels, and are they close to ours?
hist(d$forcetemp)
hist(d$photoperiod_day)
