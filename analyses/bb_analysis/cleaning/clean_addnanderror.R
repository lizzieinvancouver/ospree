## 25 March 2020 - Ailene and others
## Add n and error for sites in need.n.errorcheck.csv
if(is.data.frame(d)){
##Falusi03 (Ailene)  
  d$n[which(d$datasetID=="falusi03")]<-15
##Basler14 (Ailene)  
  d$n[which(d$datasetID=="basler14")]<-20#"1200twigs after the 3rd sampling" divded by 4 species and 3 sampling dates and 5 treatments
##caffara11a (Ailene)  
  d$n[which(d$datasetID=="caffarra11a" & d$study=="exp3")]<-8#
  d$n[which(d$datasetID=="caffarra11a" & d$study=="exp2")]<-10#
##myking97 (Ailene)  
  d$n[which(d$datasetID=="myking97")]<-13#
##worrall67 (Ailene)  
  d$n[which(d$datasetID=="worrall67")]<-5#
  ##laube14b (Ailene)  
  d$n[which(d$datasetID=="laube14b")]<-10#
  ##calm94 (Ailene added)  
  d$resp_error[which(d$datasetID=="calme94")]<-0.3230#maximum SE reported by author
  d$error.type[which(d$datasetID=="calme94")]<-"SE"
  ##calm94 (Ailene added)  
  d$resp_error[which(d$datasetID=="webb78" & d$species=="americana" & d$response.time=="7")]<-0.3
  d$resp_error[which(d$datasetID=="webb78" & d$species=="americana" & d$response.time=="4")]<-0.0
  d$resp_error[which(d$datasetID=="webb78" & d$species=="americana" & d$response.time=="5")]<-0
  d$resp_error[which(d$datasetID=="webb78" & d$species=="americana" & d$response.time=="11")]<-1.2
  d$resp_error[which(d$datasetID=="webb78" & d$species=="americana" & d$response.time=="23")]<-1.0
  d$resp_error[which(d$datasetID=="webb78" & d$species=="americana" & d$response.time=="29")]<-2.1
  d$resp_error[which(d$datasetID=="webb78" & d$species=="americana" & d$response.time=="58")]<-3.6
  d$resp_error[which(d$datasetID=="webb78" & d$species=="americana" & d$response.time=="47")]<-4.2
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharinum" & d$response.time=="4" & d$chilltemp=="")]<-1.4
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharinum" & d$response.time=="4" & d$chilltemp=="5")]<-0
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharinum" & d$response.time=="5")]<-0
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharinum" & d$response.time=="7")]<-0.9
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharinum" & d$response.time=="17")]<-1.2
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharinum" & d$response.time=="25")]<-1.6
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharinum" & d$response.time=="35")]<-2.3
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharinum" & d$response.time=="41")]<-2.9
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharinum" & d$response.time=="62")]<-2.9
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharum" & d$response.time=="4")]<-0.3
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharum" & d$response.time=="3")]<-0.0
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharum" & d$response.time=="6")]<-0.0
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharum" & d$response.time=="17")]<-1.22
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharum" & d$chilldays=="92" & d$response.time=="38")]<-2.51
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharum" & d$chilldays=="61" & d$response.time=="38")]<-1.48
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharum" & d$chilltemp=="" & d$response.time=="58")]<-3.29
  d$resp_error[which(d$datasetID=="webb78" & d$species=="saccharum" & d$chilltemp=="5" & d$response.time=="58")]<-2.03
  d$error.type[which(d$datasetID=="webb78")]<-"SE"
  
  #maximum SE reported by author
  d$error.type[which(d$datasetID=="calme94")]<-"SE"
  
  ##basler12 (Ailene added, Nacho checked)  
  d$n[which(d$datasetID=="basler12")]<-10#
  
  
  ##partanan98 (Dan B)
  d$n[which(d$datasetID=="partanen01")]<-20
  
  ##ghelardini10 (Dan B)
  d$n[which(d$datasetID=="ghelardini10" & d$study=="exp1")]<-45
  d$n[which(d$datasetID=="ghelardini10" & d$study=="exp2")]<-30
  
  
  ##myking98 (Dan B)
  d$n[which(d$datasetID=="myking98")]<-4
  
} else {
  print("Error: d not a data.frame")
}

stop("Not an error, additional sample sizes and error are added! Also, you can ignore the warning message below -- code converts a column to character, but the column is created in a different dataframe that is used and deleted in this source code and should not (that I can imagine) have any other impact.")
