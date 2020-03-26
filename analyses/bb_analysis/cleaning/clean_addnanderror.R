## 25 March 2020 - Ailene and others
## Add n and error for sites in need.n.errorcheck.csv
if(is.data.frame(d)){
##Falusi03 (Ailene)  
  d$n[which(d$datasetID=="falusi03")]<-15
##Basler14 (Ailene)  
  d$n[which(d$datasetID=="basler14")]<-20#"1200twigs after the 3rd sampling" divded by 4 species and 3 sampling dates and 5 treatments
  
} else {
  print("Error: d not a data.frame")
}

stop("Not an error, additional sample sizes and error are added! Also, you can ignore the warning message below -- code converts a column to character, but the column is created in a different dataframe that is used and deleted in this source code and should not (that I can imagine) have any other impact.")
