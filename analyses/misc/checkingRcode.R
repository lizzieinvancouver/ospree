## Started spring 2017 ##
## Notes how spot checking R code ##

dim(d) # check dimensions of your object when you start, and keep checking it every so often to make sure it makes sense. 

# check the dimensions of objects and inspect them somehow, e.g., for this line of code:
d$response[which(d$response==1 & d$respvar.simple %in% respvar.time)] <- "timeonly"

# I wrote into my R terminal (not into the script):
goober <- d[which(d$response==1 & d$respvar.simple %in% respvar.time), 1:27]
head(goober)
tail(goober)

# I use table alot to check out stuff, for example in multiresp.R the code says there is only one study with >1 response where one of the responses is percbudburst so I wrote this and scanned the output
table(d$datasetIDstudy, d$respvar.simple)
# This is *not* perfect, but a good start for a quick check ... better would be code that does the same thing from scratch using different commands, like this: 
goo <- as.data.frame(table(d$datasetIDstudy, d$respvar.simple))
daystobbID <- subset(goo, Var2=="daystobudburst" & Freq>0)
otherID <- subset(goo, Var2!="daystobudburst" & Freq>0)
otherID[which(otherID$Var1 %in% daystobbID$Var1),]

# Delete (or comment out) any code that is operating system or R system specific (e.g., quartz() or view() or such)

# When you are done with any code you *MUST* run it (ideally from both source -- if it is sourced -- and directly from the code) to make sure it runs. This is good practice for all code. It's easy to fix errors the day you make them, and takes longer to do it later while you're thinking 'but I swear this was all set!' #

# Also, delete any old comments that no longer apply and add any names of people you know have worked a lot on the code (do these things for your own code also)! #
