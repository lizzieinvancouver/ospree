For the files:

bdaymean.hf.csv -- budburst means by treatment for Flynn & Wolkovich experiment for Harvard Forest
bdaymean.sh.csv -- budburst means by treatment for Flynn & Wolkovich experiment for Saint Hippolyte

Created in Pheno Budburst analysis plus.R on 5 Dec 2018
See repo: https://github.com/lizzieinvancouver/buds

TREATCODES:
C: force - 15 C day/5 C night
W: force - 20 C day/10 C night
S: photo - 8 hrs
L: photo - 12 hrs
0: chill - field chill only -- 814.50 for HF; 599.50 for SH
1: chill - field chill + weeks at 4 C -- 2062.50 for HF; 1847.50 for SH 
2: chill - field chill + weeks at 1.5 C -- 1702.50 for HF; 1487.50 SH 

All chill units are Utah model; see Table S2 in supp for chill hours or chill portions)

And for:
spcode.csv 

I did this from 'Pheno Budburst analysis.R' (also in buds repo):

allspp <- c(shrubs, trees)
allspp <- allspp[order(allspp)]
goo <- as.data.frame(allspp)

goo$species <-  c(
      "Acer pensylvanicum",
      "Acer rubrum",
      "Acer saccharum",
      "Alnus incana subsp. rugosa",
      "Aronia melanocarpa",
      "Betula alleghaniensis",
      "Betula lenta",
      "Betula papyrifera",
      "Corylus cornuta",
      "Fagus grandifolia",
      "Fraxinus nigra",
      "Hamamelis virginiana",
      "Ilex mucronatus",
      "Kalmia angustifolia",
      "Lonicera canadensis",
      "Lyonia ligustrina",
      "Nyssa sylvatica",
      "Populus grandidentata",
      "Prunus pensylvanica",
      "Quercus alba",
      "Quercus rubra",
      "Quercus velutina",
      "Rhamnus frangula",
      "Rhododendron prinophyllum",
      "Spiraea alba",
      "Vaccinium myrtilloides",
      "Viburnum cassinoides",
      "Viburnum lantanoides"
  )

write.csv(goo, "spcode.csv")