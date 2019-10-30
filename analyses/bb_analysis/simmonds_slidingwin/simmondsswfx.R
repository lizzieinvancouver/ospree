#### Function to run sliding window analyses

# This uses the climwin package
# the current function acts as a 
# sort of wrapper around it to 
# run multiple versions easily
# and save key results

# refday = c(day, mon)
# climate is a datafile that must include col = temp
# datafile = biological data
# default = absolute but can also run relative
run_SW <- function(absolute = TRUE, datafile, climate, refday){
  # first format the dates in climate and datafile
  # they need to be saved as date objects in the same format
  datafile <- as.data.frame(datafile)
  datafile$bb_date <- strptime(datafile$bb_date, format = "%Y-%m-%d")
  climate$date <- strptime(climate$date, format="%Y-%m-%d")
  ifelse(absolute == T, type <- "absolute", type <- "relative")
  
  # then run climwin
  # absolute window
  if(absolute == T){
    results <- slidingwin(
      baseline = lm(datafile$bb_mean ~ 1), # baseline model.
      xvar=list(climate$temp),
      cdate = as.Date(climate$date, format="%d/%m/%Y"), spatial=climate$spatial,
      bdate = datafile$bb_date,
      cmissing=F, cinterval="day", type=type,
      range=c(365,0), refday = refday,
      stat=c("mean", "min", "max", "slope"),
      func="lin"
    )
  }
  # relative window
  if(absolute == F){
    results <- slidingwin(
      baseline = lm(datafile$bb_mean ~ 1), # baseline model.
      xvar=list(climate$temp),
      cdate = climate$date, spatial=climate$spatial,
      bdate = datafile$bb_date,
      cmissing=F, cinterval="day", type=type,
      range=c(365,0), #refday = refday,
      stat=c("mean", "min", "max", "slope"),
      func="lin"
    )
  }
  
  # now find the "best" result - highest R2
  # this done for each aggregate statistic
  # they are in order 1 = mean
  # 2 = max
  # 3 = min
  # 4 = slope
  R2 <- c(summary(results[[1]]$BestModel)$adj.r.squared,
          summary(results[[2]]$BestModel)$adj.r.squared,
          summary(results[[3]]$BestModel)$adj.r.squared,
          summary(results[[4]]$BestModel)$adj.r.squared)
  # choose the highest R2 across these
  marker <- which(R2 == max(R2))
  
  # then extract parameters and data from 'best' model
  window <- c(results[[marker]]$Dataset$WindowOpen[1], 
              results[[marker]]$Dataset$WindowClose[1], 
              marker, # aggregate statistic number
              summary(results[[marker]]$BestModel)$coef[1], 
              summary(results[[marker]]$BestModel)$coef[2],
              summary(results[[marker]]$BestModel)$coef[4],
              summary(results[[marker]]$BestModel)$adj.r.squared)
  return(list(window, results[[marker]]$BestModelData))
}





