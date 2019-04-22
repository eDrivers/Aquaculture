# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                     LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(magrittr)
library(tidyverse)
library(XML)
library(maptools)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                   DOWNLOAD DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The data used to characterize comes from DFO and cannot be shared
# For more information read the repo's README.md document.

# Output location for downloaded data
output <- './Data/RawData'

# Data will need to be archived to Zenodo with restricted access and downloaded
# using an access token.
# Eventually it would ideally be part of the SLGO web portal

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                              IMPORT AND FORMAT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# File name
fileName <- dir(output, pattern = '.zip')

# Unzip aquaculture data
unzip(zipfile = paste0(output, '/', fileName),
      exdir = output)


# --------------------------------------------------- #
#                    New-Brunswick                    #
# --------------------------------------------------- #
# Unzip kmz file
unzip(zipfile = './data/rawData/NB/MSLeases.kmz', exdir = './data/rawData/NB')

# Extract coordinates
nbCoord <- getKMLcoordinates(kmlfile = './data/rawData/NB/doc.kml', ignoreAltitude = T)

# Set colnames
for(i in 1:length(nbCoord)) colnames(nbCoord[[i]]) <- c('longitude','latitude')

# Get attributes tables
nbTable <- readHTMLTable('./data/rawData/NB/doc.kml')

# Modify uneven numbered dataframes
for(i in seq(1,length(nbTable), by = 2)) {
  nbTable[[i]] <- apply(X = nbTable[[i]], MARGIN = 2, FUN = as.character)
  colnames(nbTable[[i]]) <- c('Param','Value')
  nbTable[[i]] <- nbTable[[i]][2:nrow(nbTable[[i]]), ]
}

# Modify even numbered dataframes
for(i in seq(2,length(nbTable), by = 2)) {
  nbTable[[i]] <- apply(X = nbTable[[i]], MARGIN = 2, FUN = as.character)
  nbTable[[i]] <- rbind(colnames(nbTable[[i]]), nbTable[[i]])
  colnames(nbTable[[i]]) <- c('Param','Value')
}

# Verify if every second dataframe is duplicated
# for(i in seq(1,length(nbTable), by = 2)) {
#   print(all.equal(nbTable[[i]], nbTable[[i+1]]))
# }

# Remove duplicated tables
for(i in seq(length(nbTable)-1, 1, by = -2)) nbTable[[i]] <- NULL

# # Check data
# length(nbTable)
# duplicated(nbTable)

# Combine as a single data frame
nbTab <- data.frame(stringsAsFactors = F)
for(i in 1:length(nbTable)) nbTab <- rbind(nbTab, t(nbTable[[i]][, 2]))
colnames(nbTab) <- nbTable[[1]][, 1]
nbTable <- nbTab

# Create polygons
nb <- vector('list', length(nbCoord))
for(i in 1:length(nb)) nb[[i]] <- st_polygon(list(nbCoord[[i]]))

# Create sptial object
nb <- nb %>%
      st_sfc(crs = 4326) %>%
      st_sf(nbTable, .) %>%
      st_transform(crs = 32198) %>%
      mutate(ID = paste0('NB', 1:length(nbCoord)),
             Species = 'American Oyster') %>%
      rename(geometry = '.')


# --------------------------------------------------- #
#                    Newfoundland                     #
# --------------------------------------------------- #
# Import and make spatial object
nl <- read.csv('./data/rawData/NL/licences.csv') %>% # import
      st_as_sf(coords = c('Longitude','Latitude'), crs = 4326) %>% # spatial
      st_transform(crs = 32198) %>% # projection
      mutate(ID = paste0('NL', 1:nrow(.))) %>% # add id
      mutate(sizeM2 = SizeHA * 10000) %>% # Transform in meters^2: 1HA = 10000m^2
      mutate(radius = sqrt(sizeM2 / pi)) %>% #radius of circles: radius = sqrt(A / pi)
      st_buffer(dist = .$radius) # Buffer around points

# Change names
NLsp <- data.frame(accr = c("BM","GS","O","RT","ST","AS","AC","ACH","E","L","CN","LU","TP"),
                   species = c("Blue Mussels","Giant Scallops","Oyster","Rainbow Trout",
                               "Steelhead","Atlantic Salmon","Atlantic Cod",
                               "Arctic Char","Eel","Lobster","Cunner", "Lumpfish",
                               "Tilapia"),
                   type = c('inv','inv','inv','fish','fish','fish','fish','fish','fish','inv','fish','fish','fish'),
                   stringsAsFactors = F)

sp <- nl$Species %>%
      str_split(., ';') %>%
      lapply(FUN = str_trim, side = 'both')

for(i in 1:length(sp)) {
  for(j in 1:length(sp[[i]])) {
    id <- which(NLsp$accr == sp[[i]][j])
    sp[[i]][j] <- NLsp$species[id]
  }
}

nl$Species <- unlist(lapply(sp, paste, collapse = ';'))



# --------------------------------------------------- #
#                     Nova-Scotia                     #
# --------------------------------------------------- #
# Import and transform projection
ns <- st_read('./data/rawdata/NS/geo_export_c82f4975-8f0a-4f07-aaf4-ba5fe2c8a748.shp') %>%
      st_transform(crs = 32198) %>%
      mutate(ID = paste0('NS', 1:nrow(.))) %>% # add id
      rename(Species = aqua_speci) %>% # Change species column name
      mutate(Species = gsub(',', ';', Species)) # There are some ',' instead of ';' in the species list. To Change



# --------------------------------------------------- #
#                 Prince-Edward-Island                #
# --------------------------------------------------- #
pei <- st_read('./data/rawdata/PEI/BottomLeases.shp') %>%
       rbind(st_read('./data/rawdata/PEI/OBLeases.shp')) %>%
       rbind(st_read('./data/rawdata/PEI/SURLeases.shp')) %>%
       st_transform(crs = 32198) %>%
       mutate(ID = paste0('PEI', 1:nrow(.))) # add id

# Change names - Not clean, but it works
# Species data only
spNames <- colnames(pei)[3:7]
spData <- pei[, spNames]
st_geometry(spData) <- NULL
spData <- apply(spData, 2, as.character)

# Change values for species names
for(i in spNames) {
  for(j in 1:nrow(spData)) {
    if (spData[j, i] == 'T') {
      spData[j, i] <- i
    } else {
      spData[j, i] <- ''
    }
  }
}

# Collapse as a single vector
sp <- character(nrow(spData))
for(i in 1:nrow(spData)) {
  x <- spData[i, ]
  x <- x[x != ""]
  sp[i] <- paste(x, collapse = ';')
}

# Incorporate in pei data
pei$Species <- sp


# --------------------------------------------------- #
#                        Québec                       #
# --------------------------------------------------- #
# ------------- Gaspésie
# Import file
gaspe <- read.csv('./data/rawData/QC/gaspesie.csv', stringsAsFactors = F)

# Format
gaspe <- gaspe[!gaspe[,'coin'] == 'C', ] # Remove center coordinates
gaspe[gaspe[,'seq'] == 1, 'longitude'][2] <- "'655959.88'" # Longitudinal error for 1st site Gaspesie
gaspe[gaspe[,'seq'] == 2, 'longitude'][2] <- "'642928.76'" # Longitudinal error for 2nd site Gaspesie
gaspe[gaspe[,'seq'] == 6, c('longitude','latitude')][1:4,] <- gaspe[gaspe[,'seq'] == 6, c('longitude','latitude')][c(1,3,4,2),] # Point order change 6th site Gaspesie
gaspe[gaspe[,'seq'] == 7, c('longitude','latitude')][1:4,] <- gaspe[gaspe[,'seq'] == 7, c('longitude','latitude')][c(1,3,4,2),] # Point order change 7th site Gaspesie
gaspe[gaspe[,'seq'] == 8, 'longitude'][3] <- "'655929.77'" # Longitudinal error for 8th site Gaspesie
gaspe[gaspe[,'seq'] == 11, 'longitude'][2] <- "'660223.52'" # Longitudinal error for 11th site Gaspesie
gaspe[gaspe[,'seq'] == 11, 'longitude'][3] <- "'660300.66'" # Longitudinal error for 11th site Gaspesie
gaspe[gaspe[,'seq'] == 14, c('longitude','latitude')][1:4,] <- gaspe[gaspe[,'seq'] == 14, c('longitude','latitude')][c(1,3,4,2),] # Point order change 14th site Gaspesie
gaspe[gaspe[,'seq'] == 16, c('longitude','latitude')][1:7,] <- gaspe[gaspe[,'seq'] == 16, c('longitude','latitude')][c(1,2,3,4,5,7,6),] # Point order change 16th site Gaspesie

# Transform coordinates
source('./Code/dmsTOdd.R')
gaspe[, 'latitude'] <- dmsTOdd(gaspe[, 'latitude'])
gaspe[, 'longitude'] <- dmsTOdd(gaspe[, 'longitude'], type = 'long')

# Polygons function
multiPoly <- function(coords, IDs, data) {
  listPoly <- vector('list',length(IDs))
  for(i in 1:length(IDs)) {
    listPoly[[i]] <- Polygons(list(Polygon(coords[[i]])), ID = IDs[i])
  }
  poly <- SpatialPolygons(listPoly, proj4string = CRS(st_crs(4326)$proj4string)) %>%
          SpatialPolygonsDataFrame(., data.frame(value=data, row.names=IDs))
  return(poly)
}

# Prepare objects for polygons
colN <- colnames(gaspe) # Colnames
IDs <- unique(paste('ID',gaspe[, 'seq'], sep = '')) # Generate unique ids
polyGaspe <- vector('list',length(IDs)) # Empty list to store polygons

# Extract coordinates for each site from dataset with foor loop
for(i in 1:length(IDs)) {
  polyGaspe[[i]] <- gaspe[gaspe[, 'seq'] == i, c('longitude','latitude')]
}

# Polygons
gaspe <- multiPoly(polyGaspe, IDs, unique(gaspe[,1:7])) # Create multipolygon sp object
colnames(gaspe@data) <- colN[1:7] # Change colnames

# sf object
gaspe <- st_as_sf(gaspe) %>%
         st_transform(crs = 32198) %>%
         rename(Species = spAutorise)


# ------------- Magdalen Islands
# Import file
mi <- read.csv('./data/rawData/QC/IDLM2016.csv', stringsAsFactors = F)

# Format
mi[mi[,'seq'] == 3, 'latitude'][5] <- "'472602.52'" # Latitudinal error for 3rd site IDLM
mi[mi[,'seq'] == 9, 'longitude'][3] <- "'615153.90'" # Point order change 9th site IDLM

# Transform coordinates
mi[, 'latitude'] <- dmsTOdd(mi[, 'latitude'])
mi[, 'longitude'] <- dmsTOdd(mi[, 'longitude'], type = 'long')

# Prepare objects for polygons
colN <- colnames(mi) # Colnames
IDs <- unique(paste('ID',mi[, 'seq'], sep = '')) # Generate unique ids
polymi <- vector('list',length(IDs)) # Empty list to store polygons

# Extract coordinates for each site from dataset with foor loop
for(i in 1:length(IDs)) {
  polymi[[i]] <- mi[mi[, 'seq'] == i, c('longitude','latitude')]
}

# Polygons
mi <- multiPoly(polymi, IDs, unique(mi[,1:7])) # Create multipolygon sp object
colnames(mi@data) <- colN[1:7] # Change colnames

# sf object
mi <- st_as_sf(mi) %>%
      st_transform(crs = 32198) %>%
      rename(Species = spAutorise)



# ------------- Baie de Sept-Îles
# Import
bsi <- read.csv('./data/rawData/QC/aquaculture_BSI.csv', stringsAsFactors = F) %>%
       st_as_sf(coords = c('longitude','latitude'), crs = 4326) %>%
       st_transform(crs = 32198) %>%
       mutate(Species = 'Moule bleue;Petoncle geant;Laminaire succulente')

# Buffer site using mean quebec site size for bsi since we do not have that information
meanArea <- mean(c(st_area(gaspe), st_area(mi))) %>% as.numeric()
buf <- sqrt(meanArea / pi)
bsi <- st_buffer(bsi, buf)


# ------------- Québec
# Combine datasets and add ID
qc <- rbind(gaspe[, 'Species'], mi[, 'Species'], bsi[, 'Species']) %>%
      mutate(ID = paste0('QC', 1:nrow(.)))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 SINGLE DATASET
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aquaculture <- rbind(nb[, 'Species'],
                     nl[, 'Species'],
                     ns[, 'Species'],
                     pei[, 'Species'],
                     qc[, 'Species'])


#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                             FORMAT SPECIES NAMES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# With this, I can now give the proper names to all the species and identify which
# sites are farming fish, invertebrates or algae. This will not be elegant as it
# has to be done mostly manually.
#
# First, here is a table of the species with english, french and scientific names,
# with their species type categorized between fish, algae and invertebrate. I also
# indicate by which name they are identified in the various datasets.
spTable <- data.frame(frName = character(13),
                      enName = character(13),
                      scientific = character(13),
                      type = character(13),
                      stringsAsFactors = F)

spTable[1,] <- c('Moule bleue','Blue mussle','Mytilus edulis','inv')
spTable[2,] <- c('Huître américaine','American oyster','Crassostrea virginica','inv')
spTable[3,] <- c('Palourde américaine','Bay quahaug','Mercenaria mercenaria','inv')
spTable[4,] <- c("Pétoncle géant",'Sea scallop','Placopecten magellanicus','inv')
spTable[5,] <- c('Mye commune','Soft-shell clam','Mya arenaria','inv')
spTable[6,] <- c('Laminaire à long stipe','Hollow-stemmed kelp','Saccharina longicruris','algae')
spTable[7,] <- c("Mactre de l'Atlantique",'Atlantic surf clam','Spisula solidissima','inv')
spTable[8,] <- c('Pétoncle de baie','Atlantic bay scallop','Argopecten irradian','inv')
spTable[9,] <- c('Huître plate européenne','European flat oyster','Ostrea edulis','inv')
spTable[10,] <- c("Couteau de l'Atlantique",'Atlantic razor clam','Ensis leei','inv')
spTable[11,] <- c('Alarie succulente','Badderlocks','Alaria esculenta','inv')
spTable[12,] <- c('Saumon de l\'Atlantique','Atlantic salmon','Salmo salar','fish')
spTable[13,] <- c('Morue de l\'Atlantique','Atlantic cod','Gadus morhua','fish')
spTable[14,] <- c('Truite arc-en-ciel','Rainbow trout','Oncorhynchus mykiss','fish')
spTable[15,] <- c('Truite arc-en-ciel','Steelhead trout','Oncorhynchus mykiss','fish')
spTable[16,] <- c('Omble chevalier','Arctic Char',' Salvelinus alpinus','fish')
spTable[17,] <- c('Anguille d\'Amérique','American eel','Anguilla rostrata','fish')
spTable[18,] <- c('Homard américain','American lobster','Homarus americanus','inv')
spTable[19,] <- c('Tanche-tautogue','Bergall','Tautogolabrus adspersus','fish')
spTable[20,] <- c('Grosse poule de mer','Lumpfish','Cyclopterus lumpus','fish')
spTable[21,] <- c('Tilapia du Nil','Nile tilapia','Oreochromis niloticus','fish')
spTable[22,] <- c('Laminaires','Kelp','Laminariales','algae')
spTable[23,] <- c('Rhodyménie palmé','Dulse','Palmaria palmata','algae')
spTable[24,] <- c('Laminaire digitée','Finger kelp','Laminaria digitata','algae')
spTable[25,] <- c('Ulves','Sea lettuce','Ulva','algae')
spTable[26,] <- c('Laminaire sucrée','Sugar Kelp','Saccharina latissima','algae')
spTable[27,] <- c('Flétan de l\'Atlantique','Atlantic halibut','Hippoglossus hippoglossus','fish')
spTable[28,] <- c('Aiglefin','Haddock','Melanogrammus aeglefinus','fish')
spTable[29,] <- c('Oursin vert','Sea urchin','Strongylocentrotus droebachiensis','inv')
spTable[30,] <- c('Glycera','Blood worm','Glycera','inv')
spTable[31,] <- c('Omble de fontaine','Brook trout','Salvelinus fontinalis','fish')

# List of names used per species
spNamesList <- list(
  c('Mussel','Moule bleue','Blue Mussel', 'Blue Mussels'),
  c('Oyster','Huitre americaine','American Oyster'),
  c('Quahaug', 'Bay Quahaug'),
  c('Scallop','Petoncle geant','Sea Scallop', 'Giant Scallops'),
  c('Clam','Mye commune','Soft-shell Clam'),
  c('Laminaire a long stipe'),
  c('Bar/Surf Clam'),
  c('Bay Scallop'),
  c('European Oyster'),
  c('Razor Clams'),
  c('Laminaire succulente'),
  c('Atlantic Salmon'),
  c('Atlantic Cod', 'Cod'),
  c('Rainbow Trout'),
  c('Steelhead'),
  c('Arctic Char'),
  c('Eel'),
  c('Lobster'),
  c('Cunner'),
  c('Lumpfish'),
  c('Tilapia'),
  c('Kelp'),
  c('Dulse'),
  c('Finger Kelp'),
  c('Sea Lettuce'),
  c('Sugar Kelp'),
  c('Atlantic Halibut'),
  c('Haddock'),
  c('Sea Urchin'),
  c('Blood Worm'),
  c('Brook Trout')
)

# Extract the list of species for each site
sp <- aquaculture$Species %>%
      str_split(., ';') %>%
      lapply(FUN = str_trim, side = 'both')

# For each site, change the name of the species
for(i in 1:length(sp)) {
  for(j in 1:length(sp[[i]])) {
    # Identify in which list the name can be found
    id <- lapply(spNamesList, grepl, pattern = paste0('^', sp[[i]][j], '$')) %>%
          lapply(., any) %>%
          unlist(.) %>%
          which(isTRUE(.))
    sp[[i]][j] <- spTable$scientific[id]
  }
}

# Change the species column in the aquaculture dataset
aquaculture$Species <- sp %>%
                       lapply(., paste, collapse = ';') %>%
                       unlist(.)

# Identify which species are fish or invertebrates (there is no algae farming at the moment)
type <- sp
for(i in 1:length(type)) {
  for(j in 1:length(type[[i]])) {
    id <- which(spTable$scientific == type[[i]][j])
    type[[i]][j] <- spTable$type[id]
  }
}


# Add to aquaculture dataset
aquaculture$inv <- lapply(type, grepl, pattern = '^inv$') %>%
                   lapply(., any) %>%
                   unlist()

aquaculture$fish <- lapply(type, grepl, pattern = '^fish$') %>%
                    lapply(., any) %>%
                    unlist()

aquaculture$algae <- lapply(type, grepl, pattern = '^algae$') %>%
                     lapply(., any) %>%
                     unlist()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(aquaculture, file = './data/rawData/aquaculture.RData')
