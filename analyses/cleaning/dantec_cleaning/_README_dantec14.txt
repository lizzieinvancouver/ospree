Readme for Dantec14 cleaning. Started 30 June 2020 by Mira Garner.

Work until 30 Jun 2020: Data for Figure 3a (percent budburst) was scrapped by Dan Flynn. Mira Garner and Faith Jones scrapped GDD data for Figures 3b (MDG) and 3c (FJ) during the 2019 ospree retreat. The xlsx sheets for the new data was added to ospree/data/Dantec14add but was never added to the main ospree dataset.


New cleaning steps:
- Created the ospree/analyses/cleaning/dantec_cleaning folder, modeled after the okie_cleaning folder
- Removed pdf of article from Dantec14add folder
- converted MG and FJ xlsx files into csv files. Copied the csv files to the dantec_cleaning folder
-DF data already in ospree cleaning in clean_misc.R script. Cleaning for FJ and MDG datasets takes place in the dantec14_cleaning folder.

- fieldsample.date was changed from "15-Oct-10" to "unclear 15-Oct-10 to 15-Apr-11" in all datasets. The paper lists the sample schedule as about every two weeks mid-Oct 2010 to mid-Apr 2011 without giving specific start or end dates and without specifying two weeks exactly.

- added letters to figure numbers (FJ = 3a, MDG = 3b, DF = 3c). Previously all said "fig 3"

- fixed entry labeled "dantec15" in MDG dataset
- fixed population.altitude.m entries for MDG F. sylvatica (first high elevation entry labeled as low elevation and first low elevation entry labeled as high elevation)

- Chill days from paper were entered in chill.days for MDG dataset. This information was moved to field.chill.units.

- chill units are from the chilling calculation in the paper using a 5 degree threshold, see paper.

- response variable changed to GDDabove5 for FJ data and GDDabove10 for MDG data to reflect the different GDD thresholds for the figures.
