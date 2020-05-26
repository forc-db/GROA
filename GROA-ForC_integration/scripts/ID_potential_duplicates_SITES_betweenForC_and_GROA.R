######################################################
# Purpose: Import GROA SITE data into ForC SITES AND Identidfy potential duplicates 
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.4 (2018-03-15)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(RCurl)
library(dplyr)
# Load data ####

## GROA data ####
GROA_measurements <-  read.csv("data/nonsoil_litter_CWD.csv", stringsAsFactors = F)
GROA_sites <-  read.csv("data/sitesf.csv", stringsAsFactors = F)
GROA_litterature <-  read.csv("data/GROA literature.csv", stringsAsFactors = F)

## ForC data ####
ForC_files <- "ForC_sites.csv"

ForC_data <- NULL
for(f in ForC_files) {
  cat("Reading", f, "...\n")
  name <- toupper(gsub("ForC_|\\.csv", "", f))
  ForC_data[[name]] <- read.csv(text=getURL(paste0("https://raw.githubusercontent.com/forc-db/ForC/master/data/", f)), header=T, stringsAsFactors = F)
  
  ForC_data[[name]]$NEW_RECORD <- FALSE
} # for(f in forC_files)

## variable_name_conversion table ####
variable_name_conversion <- read.csv("GROA-ForC mapping/variable_name_conversion.csv", stringsAsFactors = F)

# Settings ####
## set refor.type to plot.name key
refor.type.to.plot.name.key <-  data.frame(plot.name = c("regrowth stand regenerating via spontaneous natural regeneration","regrowth stand regenerating via assisted natural regeneration","regrowth stand regenerating via initial tree planting", "diverse species plantation", "monoculture plantation", "intensive tree monocrop", "multistrata stand", "stand with tree intercropping", "silvopastoral system", "transitional ecosystem", "cropland", "pasture", "intact/ old growth stand", "fire", "harvest", "shifting cultivation"), row.names = c("SNR", "ANR", "ITP", "DP", "MP", "TMC", "MS", "TI", "SP", "TR", "C", "PA", "OG", "F", "H", "SC"), stringsAsFactors = F)

# pre-fixes and preparations ####

## merge non soil and soil tables
# names(GROA_measurements)[!names(GROA_measurements) %in% names(GROA_measurements_soil)]
# 
# GROA_measurements_soil$sites.sitename <- GROA_sites$site.sitename[match(GROA_measurements_soil$site.id, GROA_sites$site.id)]
# GROA_measurements_soil$country <- GROA_sites$site.country[match(GROA_measurements_soil$site.id, GROA_sites$site.id)]
# GROA_measurements_soil$citations.title <- GROA_litterature$study.id[match(GROA_measurements_soil$study.id, GROA_litterature$study.id)]

## create ForC.site.ID column in GROA 
## this will hold the ForC of sites that are duplicates
GROA_sites$ForC.site.ID <- NA


## create citation.ID
GROA_litterature$citation.ID <- paste(GROA_litterature$citations.author, GROA_litterature$citations.year,  substr(gsub('(?<!-)(?<!\')\\b(\\pL)|.','\\L\\1', GROA_litterature$citations.title, perl = T), 1, 4), sep = "_") # create citation ID in the form [last name of first author]_[publication year]_[first letter of first four words of title, when applicable, counting hyphenated words as single words].


## Create measurement.refs ####
# (Find all measurements of each site and append "loaded_from' to SITE table if it is unique in the measurement table)
GROA_sites$measurement.refs <- NULL

for(site in GROA_measurements$site.id) {
  print(site)
  
  meas_study.ids <- GROA_measurements$study.id[GROA_measurements$site.id %in% site]
  litt_Citation.IDs <- GROA_litterature$citation.ID[GROA_litterature$study.id %in% meas_study.ids]
  GROA_sites$measurement.refs[GROA_sites$site.id %in% site] <- paste(unique(litt_Citation.IDs), collapse = "; ")
  
} 


# create GROA.site.ID ####
ForC_data$SITES$GROA.site.ID <- NA


# Create a new record in SITES if one does not already exist ####
true.duplicate.sites <- NULL
potential.duplicate.sites <- NULL
legit.new.sites <- NULL

newForC.siteID <- max(ForC_data$SITES$site.ID)

for( i in 1:nrow(GROA_sites)) {
  newForC.siteID <- newForC.siteID + i
  
  # cat("--------------------", i, "/", nrow(GROA_sites), "\n")
  cat("Record:", paste(GROA_sites[i, 1:4], collapse = " "), "\n")
  
  sites.sitename <- GROA_sites$site.sitename[i]
  measurement.ref <- GROA_sites$measurement.refs[i]
  
  ## put data into forC format ####
  
  #### first, deal wit soil texture separately here
  sand.silt.clay <- GROA_measurements[GROA_measurements$sites.sitename %in% sites.sitename, "sand.silt.clay"]
  
  if(!all(is.na(sand.silt.clay))) {
    ## get character sctring describing soil texture and keeping longest if there is more than one value
    soil.texture <- sand.silt.clay[!grepl(":", sand.silt.clay)]
    soil.texture <- soil.texture[which.max(nchar(soil.texture))]
    soil.texture <- ifelse(length(soil.texture) > 0, soil.texture, "NAC")
    
    ## average numerical values accross plot.id
    sand.silt.clay <- sand.silt.clay[grepl(":", sand.silt.clay)]
    sand.silt.clay <- strsplit(sand.silt.clay, ":")
    sand <- mean(as.numeric(sapply(sand.silt.clay, "[[", 1)))
    silt <- mean(as.numeric(sapply(sand.silt.clay, "[[", 2)))
    clay <- mean(as.numeric(sapply(sand.silt.clay, "[[", 3)))
  } else {
    soil.texture <- "NAC"
    sand <- "NAC"
    silt <- "NAC"
    clay <- "NAC"
  }
  
  #### put in ForC format
  
  GROA_record_forc_format <- data.frame(
    site.ID = newForC.siteID, # max(ForC_data$SITES$site.ID) + 1,
    sites.sitename = GROA_sites$site.sitename[i],
    city = "NAC",
    state = ifelse(is.na(GROA_sites$site.state[i]), "NAC", GROA_sites$site.state[i]),
    country =  ifelse(is.na(GROA_sites$site.country[i]), "NAC", GROA_sites$site.country[i]),
    lat =  ifelse(is.na(GROA_sites$lat_dec[i]), "NAC", GROA_sites$lat_dec[i]),
    lon =  ifelse(is.na(GROA_sites$long_dec[i]), "NAC", GROA_sites$long_dec[i]),
    masl =  ifelse(is.na(GROA_sites$elevation[i]), "NAC", GROA_sites$elevation[i]),
    mat =  ifelse(is.na(GROA_sites$AMT[i]), "NAC", as.character(GROA_sites$AMT[i])),
    min.temp = "NAC",
    max.temp = "NAC",
    map =  ifelse(is.na(GROA_sites$AMP[i]), "NAC", as.character(GROA_sites$AMP[i])),
    soil.texture = soil.texture,
    sand = as.character(sand),
    silt = as.character(silt),
    clay = as.character(clay),
    soil.classification =  ifelse(is.na(GROA_sites$soil.classification[i]), "NAC", GROA_sites$soil.classification[i]),
    measurement.refs = GROA_sites$measurement.refs[i],
    GROA.site.ID = GROA_sites$site.id[i],
    lacks.info.from.ori.pub = 0,
    loaded.from = "[Cook-Patton database citation, in ForC format]",
    loaded.by = "R script by Valentine Herrmann",
    NEW_RECORD = TRUE,
    OLD_RECORD = FALSE
    # tropical.extratropical = 
  )
  
  
  ## add to corresponding table depending on if it is a new site, a duplicate, or a potential duplicate ####
  
  
 
  
  # if sitename exists ####
  
  if(GROA_record_forc_format$sites.sitename %in% ForC_data$SITES$sites.sitename) {
    forC_record <- ForC_data$SITES[ForC_data$SITES$sites.sitename %in% GROA_record_forc_format$sites.sitename, ]
    
    # if same measurement.refs --> add to duplicate table with ForC.site.ID, along with ForC site + add ForC.site.ID into GROA
    if(ForC_data$SITES$measurement.refs[ ForC_data$SITES$sites.sitename %in% sites.sitename] %in% measurement.ref) {

      true.duplicate.sites <- rbind(true.duplicate.sites, 
                                    bind_rows(forC_record, GROA_record_forc_format))
      
      GROA_sites$ForC.site.ID[i] <- ForC_data$SITES$site.ID[ ForC_data$SITES$sites.sitename %in% sites.sitename]
      
      ## compare info to see if GROA has better things
      apply(bind_rows(forC_record, GROA_record_forc_format)[, c("country",	"lat",	"lon",	"masl",	"geography.notes",	"mat",	"min.temp",	"max.temp",	"map",	"climate.notes",	"soil.texture",	"sand",	"silt",	"clay",	"soil.classification",	'soil.notes',	"hydrology.notes",	"site.notes",	"geographic.area",	"biogeog", "Koeppen",	"FAO.ecozone")], 2, function(x) {
        if((is.na(x[1]) | x[1] %in% c("NAC", "NaN", "NRA", "NI", "")) & !(is.na(x[2]) | x[2] %in% c("NAC", "NaN", "NRA", "NI", "")))
          { print(x)
        # readline()
        }
      })
      
      
    } # if same measurement.refs --> add to duplicate table with ForC.site.ID, along with ForC site
    
    # if different measurement.refs --> add to potential duplicates table, along with ForC site
    if(!ForC_data$SITES$measurement.refs[ ForC_data$SITES$sites.sitename %in% sites.sitename] %in% measurement.ref) {
      
      GROA_record_forc_format$site.ID <- GROA_record_forc_format$GROA.site.ID
      
      potential.duplicate.sites <- rbind(potential.duplicate.sites, 
                                         bind_rows(data.frame(From = "ForC", forC_record), data.frame(From = "GROA", GROA_record_forc_format)))
    } # if different measurement.refs --> add to potential duplicates table, along with ForC site
  } # if(GROA_record_forc_format$sites.sitename %in% ForC_data$SITES$sites.sitename)
  
  # if sitename does NOT exists ####
  
  if(!GROA_record_forc_format$sites.sitename %in% ForC_data$SITES$sites.sitename) {
    legit.new.sites <- rbind(legit.new.sites,  bind_rows(ForC_data$SITES[NULL,], GROA_record_forc_format))
  } #if(!GROA_record_forc_format$sites.sitename %in% ForC_data$SITES$sites.sitename)

} # for( i in 1:nrow(GROA_sites))

# save ####

write.csv(true.duplicate.sites, file = "new_data/potential_duplicates/true.duplicate.sites.csv", row.names = F)
write.csv(potential.duplicate.sites, file = "new_data/potential_duplicates/potential.duplicate.sites.csv", row.names = F)
write.csv(potential.duplicate.sites, file = "new_data/new_SITES.csv", row.names = F)

write.csv(GROA_sites, file = 'new_data/GROA_sites.csv', row.names = F)

# ID potential duplicates

# Load libaries ####
# library(lubridate)
library(fields)


# Load data ####
SITES <- ForC_data$SITES # read.csv("data/ForC_sites.csv", stringsAsFactors = F)
# MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC")
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}


# Find groups of potential duplicates ####

## first look for sites with same names and same 

## list of columns to compare (beside lat and lon) - fields related to physical environment ####
# col.to.compare <- c("masl", "geography.notes", "mat", "min.temp", "max.temp", "map", "climate.notes", "soil.texture", "soil.classification", "soil.notes", "hydrology.notes", "site.notes")
col.to.compare <- c("site.ref", "loaded.from")

## Find groups of SITES that are within a 5 km buffer ####

### Set up distance threshold
threshold.in.km = 5  # 5 km

### distance matrix
dist.in.km.matrix <- rdist.earth(SITES[, c("lon", "lat")], miles = F, R = 6378.137)

### get the clusterID of sites
fit <- hclust(as.dist(dist.in.km.matrix), method = "single")
clusters <- cutree(fit,h = threshold.in.km)

table(clusters)
sum(table(clusters) > 1) # 572 clusters of more than one site

clusters_with_potential_duplicates <- names(table(clusters))[table(clusters) > 1]

## Within each cluster of potential duplicates, compare the fields related to physical environement. When missing, consider as potential duplicates. 

SITES$potential_duplicate_group <- 0
SITES$potential_duplicate_group_parsed <- 0

list.all.duplicate.groupings <- list()


for(cluster in clusters_with_potential_duplicates) {
  
  s <- SITES[clusters %in% cluster, ]
  
  group.c <- data.frame( cluster= rep(cluster, nrow(s)))
  
  for(c in col.to.compare) {
    
    # consider as potential duplicates if...
    ## all are NA, 
    ## none are NA and at least 2 different sources
    ## some are NA, consider as potential duplicates 
    
    if (all(my_is.na(s[, c]))) group.c[, c] <- rep(1, length(s[, c]))
    
    if((!any(my_is.na(s[, c])) & length(unique(s[, c])) > 1) | (!all(my_is.na(s[, c])) & any(my_is.na(s[, c])))) {
      
      group.c[, c] <- rep(0, length(s[, c]))
      
      duplicates <- s[, c][duplicated(s[, c])]
      duplicates <- duplicates[!my_is.na(duplicates)]
      
      if ( length(unique(duplicates)) == 1 ) {
        group.c[, c][s[, c] %in% duplicates] <- seq(s[, c][s[, c] %in% duplicates])
        group.c[, c][!s[, c] %in% duplicates] <- paste(seq(s[, c][s[, c] %in% duplicates]), collapse = ",")
      } else {
        group.c[, c] <- rep(1, length(s[, c]))
      }
    }
    
    # if all are not NA and are all the same, then they are not duplicates but we can still put them in a group (mostly to double check code)
    
    if((!any(my_is.na(s[, c])) & length(unique(s[, c])) == 1)) {
      group.c[, c] <- seq(length(s[, c]))
    }
    
  }
  
  list.all.duplicate.groupings[[which(clusters_with_potential_duplicates %in% cluster)]] <- group.c
  
  SITES$potential_duplicate_group[clusters %in% cluster] <- which(clusters_with_potential_duplicates %in% cluster) # create a "cluser ID"
  SITES$potential_duplicate_group_parsed[clusters %in% cluster] <- group.c[,1+ which.max(apply(group.c[,-1], 2, max))] # get the grouping that is the most "informed"
  
  if(length(SITES$potential_duplicate_group_parsed[clusters %in% cluster]) == length(unique(SITES$potential_duplicate_group_parsed[clusters %in% cluster]))) {
    SITES$potential_duplicate_group[clusters %in% cluster] <- 0
    SITES$potential_duplicate_group_parsed[clusters %in% cluster] <- 0
  }
}



# save ####
potential_duplicate_group_including_GROA <- names(which(tapply(SITES$NEW_RECORD, SITES$potential_duplicate_group, sum) > 0))
potential_duplicate_group_including_GROA <- potential_duplicate_group_including_GROA[!potential_duplicate_group_including_GROA %in% "0"]
potential_dup_SITES <- SITES[SITES$potential_duplicate_group %in% potential_duplicate_group_including_GROA, ]
potential_dup_SITES <- potential_dup_SITES[order(potential_dup_SITES$potential_duplicate_group, potential_dup_SITES$sites.sitename),]
View(potential_dup_SITES)


write.csv(potential_dup_SITES, file = "new_data/potential_duplicates/sites.csv", row.names = F)
# 
# old_sites <-  data.frame(FROM = "GROA", filter(ForC_data$SITES, OLD_RECORD==TRUE))
# 
# old_SITES_from_ForC <- data.frame(FROM = "ForC", ForC_data$SITES[ForC_data$SITES$sites.sitename %in% old_sites$sites.sitename & ForC_data$SITES$OLD_RECORD %in% F,])
# 
# old_sites <- rbind(old_SITES_from_ForC, old_sites)
# old_sites <- old_sites[order(old_sites$sites.sitename),]
# old_sites


# write.csv(old_sites, "new_data/old_SITES.csv", row.names = F)

