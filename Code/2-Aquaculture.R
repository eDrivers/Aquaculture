# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(magrittr)
library(tidyverse)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                    DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./data/rawData/aquaculture.RData')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 AQUACULTURE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# As we only have site location, we only consider the sites themselves and
# use them as binary variables, without intensity. We however divide between
# the types of species farmed, differentiating between fish and
# invertebrates/algae.

# Seperate object for fish and invertebrates farming
aquaFish <- aquaculture[aquaculture$fish == T, ]
aquaInv <- aquaculture[aquaculture$inv == T | aquaculture$algae == T, ]

# Remove species names and add a column with values = 1 for intensity
aquaFish$AquacultureFish <- 1
aquaInv$AquacultureInvertebrates <- 1
aquaFish <- aquaFish[, 'AquacultureFish']
aquaInv <- aquaInv[, 'AquacultureInvertebrates']


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(aquaFish, file = './Data/Driver/AquacultureFish.RData')
save(aquaInv, file = './Data/Driver/AquacultureInvertebrates.RData')



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 VISUALIZE DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png('./Figures/AquacultureFish.png', width = 1280, height = 1000, res = 200, pointsize = 6)
plot(aquaFish[, 'AquacultureFish'], lwd = 2)
dev.off()

png('./Figures/AquacultureInvertebrates.png', width = 1280, height = 1000, res = 200, pointsize = 6)
plot(aquaInv[, 'AquacultureInvertebrates'], lwd = 2)
dev.off()
