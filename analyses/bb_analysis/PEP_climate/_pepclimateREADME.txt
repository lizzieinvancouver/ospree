Started 23 May 2019
By Cat

PEP Climate info
betpen_chillandgdd.R: script used to find chilling in portions and Utah chill for all sites that have leafout data for Betula pendula for 1950-1960 and 2000-2010. 

Chilling: from September 1st to March 1st
GDDS: from March 1st to June 1st
Mean spring temperature: from March 1st to June 1st

/output/betpenallchillsandgdds_45sites_mat:

chillutah: accumulated Utah chill for that site and year
chillports: accumulated chill portions for that site and year
gdd: accumulated GDDs for that site and year
siteslist: unique code for each latxlong site
year: year
lat: latitude for siteslist 
long: longitude for siteslist
lat.long: latitude by longitude
cc: years (i.e., 1950-1960 is before climate change and 2000-2010 is after climate change)
lo: leafout day of year
mat: mean spring temperature from March 1st to June 1st for that site and year