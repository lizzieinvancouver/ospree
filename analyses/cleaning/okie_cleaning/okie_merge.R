## Read data
okie <- read.csv("okie11tab.csv", header = TRUE, stringsAsFactors = FALSE)
ospree <- read.csv("../../input/ospree.csv", header = TRUE, stringsAsFactors = FALSE)

## Discard cultivar data column from okie (already in ospree)
okie <- okie[, -6]
## Rename columns of okie to line up with ospree (column 5 is already labeled ok)
colnames(okie)[c(1:4, 6)] <- c("response.time", "response", "photoperiod_day", "field.chill.units", "figure.table..if.applicable.")

## Work with okie subset of ospree
ospree.okie <- subset(ospree, datasetID == "okie11", drop = TRUE)
## Replace response, response.day, figure.table..if.applicable columns in subset with -999 (to discard later)
ospree.okie <- ospree.okie[, -c(26, 27, 31)]

## Merge ospree.okie with okie and use ospree column order
ospree.merged <- merge(ospree.okie, okie)[, names(ospree)]

## Save output
write.csv(x = ospree.merged, file = "../../input/okie_merge.csv", row.names = FALSE)
