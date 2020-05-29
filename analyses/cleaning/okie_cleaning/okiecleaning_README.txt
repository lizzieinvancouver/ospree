Read me explaining what Geoff did with the okie11 data:

## Per issue 320, the original figure cell for okie11 in ospree had an error
## e.g., figure = "fig3 -- data are % budbreak ~ day, see okie11 tab for all data"

## Lizzie entered the corrected data in file okie11tab (Nov 2019)

## In file okie_merge.R, I scan file okie11tab and the flawed "ospree" table (with incorrect okie11 entries)

## I then create a subset of the flawed ospree table for okie11

## The corrected entries for okie11 are then merged with the subset
## (Essentially all other entries [e.g., location] in the ospree table are correct, hence the merge)

## Finally, the subset is exported as okie_merge.csv to ../input/okie_merge.csv

## File okie_merge.csv is added to the ospree data set in script clean_addata.r (Lizzie moved the code to this new file from analyses/cleaning/clean_misc.R in late May 2020 since this doesn't exactly fall under clean_misc.R)
