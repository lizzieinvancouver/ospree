

dat <- read.csv("../input/try_bien_ospree_Nov2020.csv", header = TRUE, stringsAsFactors = FALSE)

dat <- dat[, -c(1, 8, 9)]

species <- unique(dat$new.SpeciesName)

## traits <- unique(dat$TraitName)

traits <- c("Plant_height_vegetative", "Specific_leaf_area", "Leaf_photosynthesis_rate_per_leaf_area", "Leaf_nitrogen_.N._content_per_leaf_dry_mass", "Stem_specific_density", "Leaf_dry_matter_content", "Stem_diameter")

coefficients <- c("b_force", "b_chill", "b_photo")

mat <- matrix(NA, ncol = length(traits) + length(coefficients), nrow = length(species))

for(i in 1:length(species)){
    temp <- subset(dat, new.SpeciesName == species[i])
    for(j in 1:length(traits)){
        mat[i, j] <- subset(temp, TraitName == traits[j])$trait.mean[1]
    }
    for(k in 1:length(coefficients)){
        mat[i, (length(traits) + k)] <- subset(temp, Coefficient == coefficients[k])$mean[1]
    }
}

colnames(mat) <- c("Height", "SLA", "Photosyn",
                   "N", "SSD", "LDMC", "Stem", "force", "chill", "photo")

mat2 <- mat[, c(1, 2, 4:10)]
        
mat2 <- mat2[complete.cases(mat2), ]        

ranges <- list(c(-.4, .4),
               c(-.2, .2),
               c(-.1, .1))

pdf(file = "pca.pdf", width = 12, height = 5)

par(mfrow = c(1, 3), mar = c(5, 5, 2, 2), oma = c(0, 0, 0, 0))
for(i in 1:length(ranges)){
    biplot(prcomp(mat2), xlim = ranges[[i]], ylim = ranges[[i]], cex = c(.7, 1.25), col = "black")
}

dev.off()

pdf(file = "pairs.pdf", width = 8, height = 8)
pairs(mat2)
dev.off()


