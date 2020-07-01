######################################################
# Purpose: Import GROA data into ForC, keeping track of sites potential duplicates 
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

#### remove worldclim_bio data
GROA_sites <- GROA_sites[, !grepl("worldclim_", names(GROA_sites))]

#### remove 2 sites (4788 and 5243) in measurements because  @CookPatton said "yes I can confirm that sites 4788 and 5243 should be deleted. The geolocation falls where there is no information in Dinerstein for ecoregion/biome (presumably in the ocean).")

GROA_measurements <- GROA_measurements[!GROA_measurements$site.id %in% c(4788, 5243), ] 

## ForC data ####
na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") # "999"
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}

url <- "https://github.com/forc-db/ForC/tree/master/data"
x <- readLines(url)
goodlines = ".csv"
try = grep(goodlines,x,value=TRUE)
try = try[grepl('title=(.*) id=', try, perl = T)]
try <- gsub('"', "", try)
try.regexec <- regexec('title=(.*) id=', try, perl = T)
try.regmatches <- regmatches(try, try.regexec)

ForC_files <- sapply(try.regmatches, function(x) x[2])

ForC_data <- NULL
for(f in ForC_files) {
  cat("Reading", f, "...\n")
  name <- toupper(gsub("ForC_|\\.csv", "", f))
  ForC_data[[name]] <- read.csv(text=getURL(paste0("https://raw.githubusercontent.com/forc-db/ForC/master/data/", f)), header=T, stringsAsFactors = F)
  
  ForC_data[[name]]$NEW_RECORD <- FALSE
  
  if(f %in% c("ForC_history.csv", "ForC_sites.csv")) ForC_data[[name]]$GROA.site.ID <- NA
  if(f %in% c("ForC_measurements.csv")) ForC_data[[name]]$GROA.measurement.ID <- NA
} # for(f in forC_files)

ForC_data$MEASUREMENTS$plot.name[is.na(ForC_data$MEASUREMENTS$plot.name)] <- "Not named"
ForC_data$HISTORY$plot.name[is.na(ForC_data$HISTORY$plot.name)] <- "Not named"




## GROA vs ForC potential duplicates or true duplicates necessary files ####

## file of sites that were ID-ed as true duplicates by this very script.
true.duplicate.sites <- read.csv("new_data/potential_duplicates/true.duplicate.sites.csv", stringsAsFactors = F)
## file of sites that were ID-ed as potential duplicates by this very script.
# potential.duplicate.sites <-  read.csv("new_data/potential_duplicates/potential.duplicate.sites.csv", stringsAsFactors = F)
## file that someone manunally edited to say wether the potential duplicated sites were legit independant or actual duplicates (manually created looking at potential.duplicate.sites.csv)
ForC.GROA.potential.duplicate.manually.solved <- read.csv("new_data/potential_duplicates/ForC.GROA.potential.duplicate.sites.list.csv")
ForC.GROA.potential.duplicate.manually.solved <- ForC.GROA.potential.duplicate.manually.solved[!duplicated(ForC.GROA.potential.duplicate.manually.solved),]

## variable_name_conversion table ####
variable_name_conversion <- read.csv("GROA-ForC mapping/variable_name_conversion.csv", stringsAsFactors = F)

# pre-fixes and preparations ####
## set refor.type to plot.name key ####
refor.type.to.plot.name.key <-  data.frame(plot.name = c("regrowth stand regenerating via spontaneous natural regeneration","regrowth stand regenerating via assisted natural regeneration","regrowth stand regenerating via initial tree planting", "diverse species plantation", "monoculture plantation", "intensive tree monocrop", "multistrata stand", "stand with tree intercropping", "silvopastoral system", "transitional ecosystem", "cropland", "pasture", "intact/ old growth stand", "fire", "harvest", "shifting cultivation"), row.names = c("SNR", "ANR", "ITP", "DP", "MP", "TMC", "MS", "TI", "SP", "TR", "C", "PA", "OG", "F", "H", "SC"), stringsAsFactors = F)



## merge non soil and soil tables
# names(GROA_measurements)[!names(GROA_measurements) %in% names(GROA_measurements_soil)]
# 
# GROA_measurements_soil$sites.sitename <- GROA_sites$site.sitename[match(GROA_measurements_soil$site.id, GROA_sites$site.id)]
# GROA_measurements_soil$country <- GROA_sites$site.country[match(GROA_measurements_soil$site.id, GROA_sites$site.id)]
# GROA_measurements_soil$citations.title <- GROA_litterature$study.id[match(GROA_measurements_soil$study.id, GROA_litterature$study.id)]


## create citation.ID and measurement.refs ####
GROA_litterature$citation.ID <- paste(GROA_litterature$citations.author, GROA_litterature$citations.year,  substr(gsub('(?<!-)(?<!\')\\b(\\pL)|.','\\L\\1', GROA_litterature$citations.title, perl = T), 1, 4), sep = "_") # create citation ID in the form [last name of first author]_[publication year]_[first letter of first four words of title, when applicable, counting hyphenated words as single words].

### create citation.ID in GROA_measurements
GROA_measurements$citation.ID <- GROA_litterature$citation.ID[match(GROA_measurements$study.id, GROA_litterature$study.id )]

### Create measurement.refs in GROA_sites (Find all measurements of each site and append "citation.ID' to SITE table if it is unique in the measurement table)
GROA_sites$measurement.refs <- NULL

for(site in GROA_measurements$site.id) {
  # print(site)
  
  meas_study.ids <- GROA_measurements$citation.ID[GROA_measurements$site.id %in% site]
  GROA_sites$measurement.refs[GROA_sites$site.id %in% site] <- paste(unique(meas_study.ids), collapse = "; ")
  
} 

## Create plot.name ####

## if site.sitename different than what is in the site table, paste that, otherwise use only refor.type.to.plot.name.key
GROA_measurements$plot.name <- ""
for(i in 1:nrow(GROA_measurements)) {
  if(!GROA_measurements$sites.sitename[i] %in% GROA_sites$site.sitename[GROA_sites$site.id %in% GROA_measurements$site.id[i] & GROA_sites$study.id %in% GROA_measurements$study.id[i]]) {
    s <- GROA_sites$site.sitename[GROA_sites$site.id %in% GROA_measurements$site.id[i] & GROA_sites$study.id %in% GROA_measurements$study.id[i]]
    
    GROA_measurements$plot.name[i] <- gsub(s, "", GROA_measurements$sites.sitename[i])
    GROA_measurements$plot.name[i] <- gsub("^ | $", "",  GROA_measurements$plot.name[i] )
    
    if(s %in% "Porce II chronosequence 2") GROA_measurements$plot.name[i] <- paste0(2,   GROA_measurements$plot.name[i])
    
    # if(grepl(" ([0-9]{1,})$", s)) GROA_measurements$plot.name[i] <- GROA_measurements$sites.sitename[i]
    # if(!grepl(" ([0-9]{1,})$", s)) GROA_measurements$plot.name[i] <- gsub(s, "", GROA_measurements$sites.sitename[i])
    
    # GROA_measurements$plot.name[i] <- gsub("^ | $|^_|^(_[0-9]{1,})", "",  GROA_measurements$plot.name[i] )
    
    
  }
  
}

GROA_measurements$plot.name <- paste(GROA_measurements$plot.name, refor.type.to.plot.name.key[GROA_measurements$refor.type,])
GROA_measurements$plot.name <- gsub("^ | $", "", GROA_measurements$plot.name)
GROA_measurements$plot.name <- ifelse(grepl("regrowth", GROA_measurements$plot.name) & !is.na(GROA_measurements$date) & !is.na(GROA_measurements$stand.age), paste(GROA_measurements$plot.name, "established around", GROA_measurements$date - GROA_measurements$stand.age),GROA_measurements$plot.name )

## if there is multiple plot.id for one plot.name, append a number to have a one to one plot.id/plot.name relationship

for(p in unique(GROA_measurements$plot.name)) {
  # print(p)
  groa_sub <- GROA_measurements[GROA_measurements$plot.name %in% p, ]
  if(length(unique(groa_sub$plot.id)) > 1 ) {
    GROA_measurements[GROA_measurements$plot.name %in% p, ]$plot.name <- paste( GROA_measurements[GROA_measurements$plot.name %in% p, ]$plot.name, c(1:length(unique(groa_sub$plot.id)))[match(groa_sub$plot.id, sort(unique(groa_sub$plot.id)))])
    # c(1:length(unique(groa_sub$plot.id)))[match(groa_sub$plot.id, sort(unique(groa_sub$plot.id)))]
    # sort(unique(groa_sub$plot.id))[match(groa_sub$plot.id, sort(unique(groa_sub$plot.id)))]
    # groa_sub$plot.id
  }
}


### Fix problems in GROA site.id and site.sitenames ####
#### see https://github.com/forc-db/GROA/issues/17#issuecomment-443344303


### ID duplicated sites if there is more
duplicated.site.ids <- sort(unique(GROA_sites$site.id[duplicated( GROA_sites$site.id)])) # should be empty
if(length(duplicated.site.ids) > 0) {
 
  exact.same.site.just.different.study.id <- NULL
  same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil <- NULL
  same.coordiantes.but.different.site.name <- NULL
  same.site.name.but.different.coordiantes <- NULL
  differemt.site.name.and.different.coordinates <- NULL
  
  for(s in duplicated.site.ids) {
    x <- GROA_sites[GROA_sites$site.id %in% s, ]
    
    if(any(duplicated(x[, -c(2)]))) exact.same.site.just.different.study.id <- c(exact.same.site.just.different.study.id, s)
    if(length(unique(x$site.sitename)) == 1 & length(unique(x$lat_dec)) == 1 & length(unique(x$long_dec)) == 1 & sum(duplicated(x[, -c(1:6)])) == 0) same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil <- c(same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil, s)
    if(length(unique(x$site.sitename)) > 1 & length(unique(x$lat_dec)) == 1 & length(unique(x$long_dec)) == 1) same.coordiantes.but.different.site.name <- c(same.coordiantes.but.different.site.name, s)
    if(length(unique(x$site.sitename)) == 1 & (length(unique(x$lat_dec)) != 1 | length(unique(x$long_dec)) != 1)) same.site.name.but.different.coordiantes <- c(same.site.name.but.different.coordiantes, s)
    if(length(unique(x$site.sitename)) != 1 & (length(unique(x$lat_dec)) != 1 | length(unique(x$long_dec)) != 1)) differemt.site.name.and.different.coordinates <- c(differemt.site.name.and.different.coordinates, s)
  }
  
  duplicated.site.ids[!duplicated.site.ids %in% c(exact.same.site.just.different.study.id, same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil, same.coordiantes.but.different.site.name, same.site.name.but.different.coordiantes, differemt.site.name.and.different.coordinates)]
} # if(length(duplicated.site.ids) > 0)

exact.same.site.just.different.study.id
same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil
same.coordiantes.but.different.site.name
same.site.name.but.different.coordiantes
differemt.site.name.and.different.coordinates

#### Sites have the same geographic coordinates and differ by the study that reported them --> keep only one of them (the one with most imformation) ####

#### true duplicated sites (just different study.id)
GROA_sites <- GROA_sites[!(GROA_sites$site.id %in% exact.same.site.just.different.study.id & duplicated(GROA_sites$site.id)), ]

#### close enough to be true duplicates
same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil # these are the site
# these are the lines we want to keep for each of them
GROA_sites[GROA_sites$site.id %in% same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil, ][order(GROA_sites[GROA_sites$site.id %in% same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil, ]$site.id),]

row.to.keep <- data.frame(site.id =same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil,
                          row.to.keep = c(3,2,1,2,1,1,2,1,1,2,2,2,2,1,1,1,2,2,2)) # this was decided looking at output of line above. Keeping the row with the most information of what aseems most accurate

for(s in same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil) {
  x <- GROA_sites[GROA_sites$site.id %in% s, ]
  r.to.keep <- row.to.keep[row.to.keep$site.id %in% s, ]$row.to.keep
  study.id.to.remove <- x$study.id[-r.to.keep]
  GROA_sites <- GROA_sites[!(GROA_sites$site.id %in% s & GROA_sites$study.id %in% study.id.to.remove), ]
}


#### Sites have the same coordinates but a different site.name ####
# - For 274 there is only one site.sitename that exists in nonsoil_liiter_CWD.csv (and soil data does not have site.sitename so I don't know)
# - For 2250, both site.sitename are used in 2250.
# --> since they have the same coordinates and because it seems that the site.sitename look more like a plot name (see next section about the fact the a lot of the site.sitename in nonsoil_liiter_CWD.csv are actually plot names and differ from the site.sitename of sitesf.csv) I'll keep only one line for each and replace site.sitename by _ San Carlos de Rio Negro_ for site.id 274 and Ponderosa for site.id 2250

##### remove one line for site.id 274 and give site.sitename "San Carlos de Rio Negro"
GROA_sites[GROA_sites$site.id %in% 274, ]
GROA_sites[GROA_sites$site.id %in% 274, ]$site.sitename[1] <- "San Carlos de Rio Negro"
GROA_sites <- GROA_sites[!(GROA_sites$site.id %in% 274 & !GROA_sites$site.sitename %in% "San Carlos de Rio Negro"), ]

##### remove one line for site.id 2250 and give site.sitename "Ponderosa"
GROA_sites[GROA_sites$site.id %in% 2250, ]
GROA_sites[GROA_sites$site.id %in% 2250, ]$site.sitename[1] <- "Ponderosa"
GROA_sites <- GROA_sites[!(GROA_sites$site.id %in% 2250 & !GROA_sites$site.sitename %in% "Ponderosa"), ]


#### sites that have different site.sitename and different coordinates ####
# @CookPattong: For the ones where the geolocation do not match, I would still keep 293, 2218, 2219 as a single site. I went back to my notes and I had to make judgement calls and adjustments. For example, for site.id 293 my notes say that for study. id 9362, they include the "same sites as 293-295 but data are aggregated by forest age rather than providing full details, took average lat/long and elevation across sites here." And for site.id 2218/study.id 9222: "averaged Lewis Canyon North/South." And for site.id 2218 & 2219/study.id 11321 "found average of density by young values, geolocation for each from googlemaps." 14006 is a total error. Thank you for catching it. Those should be two sites. That is something I will fix on my end and propagate through the datasheets (and send the cleaned version to you).
# @teixairak: It sounds like perhaps they would be the same site but different plots.


#### For 293 remove rows with study.id 9224 or 9362.
GROA_sites <- GROA_sites[!(GROA_sites$site.id %in% c(293) & GROA_sites$study.id %in% c(9224, 9362)), ]


#### For 2218 and 2219, remove row with site.sitename "Yellowstone National Park - ..." (rows with study.if = 11321) + in measurements, replace "moderate density young" and "high density young" by "Yellowstone National Park - moderate density young" and "Yellowstone National Park - high density young"
GROA_measurements[GROA_measurements$site.id %in% c(2218, 2219) & GROA_measurements$sites.sitename %in% c("moderate density young", "high density young"), ]
GROA_measurements[GROA_measurements$site.id %in% c(2218, 2219) & GROA_measurements$sites.sitename %in% c("moderate density young", "high density young"), ]$sites.sitename <- paste("Yellowstone National Park -", GROA_measurements[GROA_measurements$site.id %in% c(2218, 2219) & GROA_measurements$sites.sitename %in% c("moderate density young", "high density young"), ]$sites.sitename)

GROA_sites <- GROA_sites[!(GROA_sites$site.id %in% c(2218, 2219) & GROA_sites$study.id %in% 11321), ]

### give new site.id to site.id 14006	study.id 2244 --> Suzan did it on her side
# new.site.id <- max(GROA_sites$site.id) + 1
# GROA_sites[GROA_sites$site.id %in% 14006 & GROA_sites$study.id %in% 2244, ]$site.id <- new.site.id
# GROA_measurements[GROA_measurements$site.id %in% 14006 & GROA_measurements$study.id %in% 2244, ]$site.id <- new.site.id

### 11 pairs or trios of sites with different site.id but same site.sitename, of those:
# - 2 pairs of site.id (Sungai Wain 2199/2046 and Perloja Experimental Station 3140/13974) have the exact same coordinates. --> true duplicates ? I think yes and I'll keep only one of them. This is if we agree to ignore the differences in annual temperature, elevation etc... @CookPatton and @teixeirak let me know if you disagree with that.
# - 7 pairs have different coordinates (Agua Salud 2164/243, Cofre de Perote 2271/3871, El Refugio 293/2417, Hubbard Brook Experimental Forest3606/2114, Kambja 3821/2039, Luquillo 9783/11447, Sarapiqui 3901/39) --> legitimate independant sites. I'll rename them as for example Agua Salud and Agua Salud.1 in the code to deal with error created by this problem.
# - 1 trio (La Selva 2113/3907/5577) is a combination of both situation above (1 pair of true duplicates ( 3907/5577) and one legitimate site (2113) --> will treat it like above
# - 1 trio (Luquillo Experimental Forest 100/3817/2414) is more complex. Our intern Abby identified both site.id 100 and 3817 to be duplicates of ForC site.id 1142 AND the measurements references between ForC site 1142 and GROA site 2414 are matching exactly.--> So I think we will merge those 3 sites. @CookPatton, let us know if you feel strongly against this.
# - 1 duo (El Refugio 293/2417) is more complex. Our intern Abby identified both site.id 293and 2417 to be duplicates of ForC site.id 563. --> So I think we will merge those 2 sites. @CookPatton, let us know if you feel strongly against this.
# - 1 duo (Kambja 3821/2039) our intern Abby identified site.id 3821 to be duplicates of ForC site.id 862 AND the measurements references between ForC site 862 and GROA site 2039 are matching exactly --> So I think we will merge those 2 sites.



## only 2046 exist in GROA_measurements so we remove 2199 --> Susan did it on her end
# GROA_sites[GROA_sites$site.id %in% c(2199,2046), ]
# GROA_measurements[GROA_measurements$site.id %in% c(2199,2046), ]
# 
# GROA_sites[GROA_sites$site.id %in% c(2046), c("AMT", "soil.classification")] <- GROA_sites[GROA_sites$site.id %in% c(2199), c("AMT", "soil.classification")]
# GROA_sites <- GROA_sites[!GROA_sites$site.id %in% 2199, ]


## only 3140 exist in GROA_measurements but also exists in soil... so we need to rename 13974 by 3140 in soil data
GROA_sites[GROA_sites$site.id %in% c(3140, 13974), ]
GROA_measurements[GROA_measurements$site.id %in%  c(3140, 13974), ]
warning("RENAME site.id 13974 by 3140 in soil data when we merge it")

GROA_sites <- GROA_sites[!GROA_sites$site.id %in% 13974, ]

## merge 100, 3817 and 2414... remove 3817 and 2414 from GROA_sites and rename 3817 and 2414 by 100 in measurements
GROA_sites[GROA_sites$site.id %in% c(100, 3817, 2414), ]
GROA_measurements[GROA_measurements$site.id %in%  c(100, 3817, 2414), ]
table(GROA_measurements[GROA_measurements$site.id %in%  c(100, 3817, 2414), ]$site.id)

GROA_sites <- GROA_sites[!GROA_sites$site.id %in% c(3817, 2414), ]
GROA_measurements[GROA_measurements$site.id %in%  c(3817, 2414), ]$site.id <- 100
warning("RENAME site.id 3817 and 2414 by 100 in soil data when we merge it")

## merge 293 and 2417 remove 2417 from GROA_sites and rename 2417 by 293 in measurements
GROA_sites[GROA_sites$site.id %in% c(293, 2417), ]
GROA_measurements[GROA_measurements$site.id %in%  c(293, 2417), ]
table(GROA_measurements[GROA_measurements$site.id %in%  c(293, 2417), ]$site.id)

GROA_sites <- GROA_sites[!GROA_sites$site.id %in% 2417, ]
GROA_measurements[GROA_measurements$site.id %in%  c(2417), ]$site.id <- 293

## merge 3821 and 2039 remove 3817 from GROA_sites and rename 3821 by 2039 in measurements
GROA_sites[GROA_sites$site.id %in% c(3821, 2039), ]
GROA_measurements[GROA_measurements$site.id %in%  c(3821, 2039), ]
table(GROA_measurements[GROA_measurements$site.id %in% c(3821, 2039), ]$site.id)
table(GROA_measurements[GROA_measurements$site.id %in% c(3821, 2039), ]$plot.name)

GROA_sites <- GROA_sites[!GROA_sites$site.id %in% 3821, ]
GROA_measurements[GROA_measurements$site.id %in%  c(3821), ]$site.id <- 2039


### ID duplicated sites if there is more
duplicated.site.ids <- sort(unique(GROA_sites$site.id[duplicated( GROA_sites$site.id)])) # should be empty
if(length(duplicated.site.ids) > 0) {
  
  stop("there still are duplicated site.id!") 
  
  exact.same.site.just.different.study.id <- NULL
  same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil <- NULL
  same.coordiantes.but.different.site.name <- NULL
  same.site.name.but.different.coordiantes <- NULL
  differemt.site.name.and.different.coordinates <- NULL
  
  for(s in duplicated.site.ids) {
    x <- GROA_sites[GROA_sites$site.id %in% s, ]
    
    if(any(duplicated(x[, -c(2)]))) exact.same.site.just.different.study.id <- c(exact.same.site.just.different.study.id, s)
    if(length(unique(x$site.sitename)) == 1 & length(unique(x$lat_dec)) == 1 & length(unique(x$long_dec)) == 1 & sum(duplicated(x[, -c(1:6)])) == 0) same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil <- c(same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil, s)
    if(length(unique(x$site.sitename)) > 1 & length(unique(x$lat_dec)) == 1 & length(unique(x$long_dec)) == 1) same.coordiantes.but.different.site.name <- c(same.coordiantes.but.different.site.name, s)
    if(length(unique(x$site.sitename)) == 1 & (length(unique(x$lat_dec)) != 1 | length(unique(x$long_dec)) != 1)) same.site.name.but.different.coordiantes <- c(same.site.name.but.different.coordiantes, s)
    if(length(unique(x$site.sitename)) != 1 & (length(unique(x$lat_dec)) != 1 | length(unique(x$long_dec)) != 1)) differemt.site.name.and.different.coordinates <- c(differemt.site.name.and.different.coordinates, s)
  }
  
  duplicated.site.ids[!duplicated.site.ids %in% c(exact.same.site.just.different.study.id, same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil, same.coordiantes.but.different.site.name, same.site.name.but.different.coordiantes, differemt.site.name.and.different.coordinates)]
} # if(length(duplicated.site.ids) > 0)

exact.same.site.just.different.study.id
same.coordiantes.same.site.name.different.study.id.some.diff.elev.amt.amp.soil
same.coordiantes.but.different.site.name
same.site.name.but.different.coordiantes
differemt.site.name.and.different.coordinates


# # this we don't need to worry about. We won't look at site.sitename in measurements anymore.
# several.site.names.in.measurements <- NULL
# site.name.different.in.measurements <- NULL
# for (s in unique(GROA_sites$site.id)) {
#   x <- GROA_measurements[GROA_measurements$site.id %in% s, ]
#   xs <- GROA_sites[GROA_sites$site.id %in% s, ]
# 
#   if(!all(x$sites.sitename == x$sites.sitename[1])){
#     several.site.names.in.measurements <- c(several.site.names.in.measurements, s)
#     # print(unique(GROA_sites[GROA_sites$site.id %in% s,]))
#     # print(unique(x[, c("site.id", "plot.id", "sites.sitename", "variables.name", "mean_ha")]))
#     # readline("press [enter]")
#   }
# 
#   if(all(!unique(xs$site.sitename) %in% unique(x$sites.sitename))) {
#     site.name.different.in.measurements <- c(site.name.different.in.measurements, s)
#   }
# 
# }


### ID duplicated sites that have different site.id
duplicated.site.sitename <- sort(unique(GROA_sites$site.sitename[duplicated( GROA_sites$site.sitename)]))
if(length(duplicated.site.sitename) > 0) {

  exact.same.site.just.different.site.id.or.study.id <- NULL
  same.coordiantes.different.site.name.different..site.id.or.study.id.some.diff.elev.amt.amp.soil <- NULL
  legitmiate.different.sites <- NULL
  
  if(length(exact.same.site.just.different.site.id.or.study.id) > 0 | length(same.coordiantes.different.site.name.different..site.id.or.study.id.some.diff.elev.amt.amp.soil) > 0) stop("there are still true duplicated site.sitename!")
  
  for(s in duplicated.site.sitename) {
    x <- GROA_sites[GROA_sites$site.sitename %in% s, ]
    # print(x)
    if(any(duplicated(x[, -c(2)]))) exact.same.site.just.different.site.id.or.study.id <- c(exact.same.site.just.different.site.id.or.study.id, s)
    if(length(unique(x$lat_dec)) == 1 & length(unique(x$long_dec)) == 1 & sum(duplicated(x[, -c(1:6)])) == 0) same.coordiantes.different.site.name.different..site.id.or.study.id.some.diff.elev.amt.amp.soil <- c(same.coordiantes.different.site.name.different..site.id.or.study.id.some.diff.elev.amt.amp.soil, s)
    if(length(unique(x$lat_dec)) != 1 | length(unique(x$long_dec)) != 1) legitmiate.different.sites <- c(legitmiate.different.sites, s)
  }
  
} # if(length(duplicated.site.ids) > 0)

exact.same.site.just.different.site.id.or.study.id
same.coordiantes.different.site.name.different..site.id.or.study.id.some.diff.elev.amt.amp.soil
legitmiate.different.sites

### deal with legitimate unique sites that have duplicate site.sitename

deal.with.duplicate.site.name <- data.frame(GROA.site.ID = GROA_sites$site.id, site.sitename = GROA_sites$site.sitename, unique.sites.sitename = make.unique(GROA_sites$site.sitename), stringsAsFactors = F)[GROA_sites$site.sitename %in% legitmiate.different.sites,]
deal.with.duplicate.site.name <- deal.with.duplicate.site.name[order(deal.with.duplicate.site.name$unique.sites.sitename), ]





# Function to format GROA into ForC format ####
GROA_to_ForC_format_SITES <- function(i, newForC.siteID) {
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
    GROA.site.ID = GROA.site.ID,
    lacks.info.from.ori.pub = 0,
    loaded.from = "[Cook-Patton database citation, in ForC format]",
    loaded.by = "R script by Valentine Herrmann",
    NEW_RECORD = TRUE
    # tropical.extratropical = 
  )
  
  return(GROA_record_forc_format)
} 

# --- IMPORT --- ####
# Create a new record in SITES if one does not already exist ####

potential.duplicate.sites <- NULL
# true.duplicate.sites <- NULL
# legit.new.sites <- NULL
problem.of.duplicates.within.GROA <- NULL

newForC.siteID <- max(ForC_data$SITES$site.ID)

for( i in 1:nrow(GROA_sites)) {
  # newForC.siteID <- newForC.siteID + i
  # cat("--------------------", i, "/", nrow(GROA_sites), "\n")
  cat("Site record:", paste(GROA_sites[i, 1:4], collapse = " "), "\n")
  
  GROA.site.ID <- GROA_sites$site.id[i]
  sites.sitename <- GROA_sites$site.sitename[i]
  measurement.ref <- GROA_sites$measurement.refs[i]
  
  ## add to corresponding table depending on if it is a new site, a duplicate, or a potential duplicate ####
  
  # if GROA.site.ID exists in ForC_sites$GROA.site.ID ####
  
  
  if(GROA.site.ID %in% ForC_data$SITES$GROA.site.ID) {
    cat("Site already exists in ForC")
  } # if(GROA.site.ID %in%  ForC_data$SITES$GROA.site.ID)
  
  if(!GROA.site.ID %in%  ForC_data$SITES$GROA.site.ID) {
    
    # site has not been merged into ForC so we need to check if it is a duplicate or not, either by looking atForC.GROA.potential.duplicate.manually.solved or by comparing site names etc...
    
    ### First look at ForC.GROA.potential.duplicate.manually.solved
    if(GROA.site.ID %in% ForC.GROA.potential.duplicate.manually.solved$GROA.site.ID) {
      
      x <- ForC.GROA.potential.duplicate.manually.solved[ForC.GROA.potential.duplicate.manually.solved$GROA.site.ID %in% GROA.site.ID, ]
      print(x)
      
      # make sure the problem has been solved correctly
      if(any(apply(x[, c(2,3)], 1, function(y) sum(is.na(y)) != 1))) stop("the site has not been properly identified as real duplicate or ligit independant")
      
      # if we know it is a duplicate --> add ID into the GROA.site.ID of the corresponding ForC site
      if( any(!is.na(x[, 2]))) {
        
        if(is.na( ForC_data$SITES$GROA.site.ID[ForC_data$SITES$site.ID %in% x[, 2][!is.na(x[, 2])]])) {
          cat("Add ID into  ForC GROA.site.ID field of the corresponding ForC site. Press [enter].")
          ForC_data$SITES$GROA.site.ID[ForC_data$SITES$site.ID %in% x[, 2][!is.na(x[, 2])]] <- GROA.site.ID
        }else{
          cat("this GROA site has a potential duplicate within the very GROA data... keep track of that...")
          problem.of.duplicates.within.GROA <- rbind(problem.of.duplicates.within.GROA, paste("ForC.siteID =",  ForC_data$SITES$site.ID[ForC_data$SITES$site.ID %in% x[, 2][!is.na(x[, 2])]], ", GROA sites:",  ForC_data$SITES$GROA.site.ID[ForC_data$SITES$site.ID %in% x[, 2][!is.na(x[, 2])]], GROA.site.ID))
          
        }
        ForC_data$SITES$GROA.site.ID[ForC_data$SITES$site.ID %in% x[, 2][!is.na(x[, 2])]]
        
      } #  if( !is.na(x[, 2]) ) 
      
      # if we know it is an independant site --> add new record into ForC
      
      if( all(is.na(x[, 2])) & all(!is.na(x[, 3])) ) {
        cat("Add new record into ForC. Press [enter].")
        newForC.siteID <- newForC.siteID + 1
        ForC_data$SITES <- bind_rows(ForC_data$SITES, GROA_to_ForC_format_SITES(i, newForC.siteID))
      } # if( !is.na(x[, 3]) )
      
    } #  if(GROA.site.ID %in% ForC.GROA.potential.duplicate.manually.solved$GROA.site.ID) 
    
    ### Second, if first was not the case, compare site names etc and parse into true duplicate or potential duplicates files
    
    if(!GROA.site.ID %in% ForC.GROA.potential.duplicate.manually.solved$GROA.site.ID) {
      if(sites.sitename %in% ForC_data$SITES$sites.sitename) {
        
        forC_record <- ForC_data$SITES[ForC_data$SITES$sites.sitename %in% sites.sitename, ]
        
        # if same measurement.refs --> add to true duplicate table with ForC.site.ID, along with ForC site
        if((ForC_data$SITES$measurement.refs[ForC_data$SITES$sites.sitename %in% sites.sitename] %in% measurement.ref) & !is.na(measurement.ref)) {
          
          if (!GROA.site.ID %in% true.duplicate.sites$GROA.site.ID) {
            true.duplicate.sites <- rbind(true.duplicate.sites, 
                                          bind_rows(forC_record, GROA_to_ForC_format_SITES(i, NA)))
          } else { cat("already in true.duplicate.sites")
          }
          
          ForC_data$SITES$GROA.site.ID[ForC_data$SITES$sites.sitename %in% sites.sitename] <- GROA.site.ID
          # GROA_sites$ForC.site.ID[i] <- ForC_data$SITES$site.ID[ ForC_data$SITES$sites.sitename %in% sites.sitename]
          
          ## compare info to see if GROA has better things
          apply(bind_rows(forC_record, GROA_to_ForC_format_SITES(i, NA))[, c("country",	"lat",	"lon",	"masl",	"geography.notes",	"mat",	"min.temp",	"max.temp",	"map",	"climate.notes",	"soil.texture",	"sand",	"silt",	"clay",	"soil.classification",	'soil.notes',	"hydrology.notes",	"site.notes",	"geographic.area",	"biogeog", "Koeppen",	"FAO.ecozone")], 2, function(x) {
            if((is.na(x[1]) | x[1] %in% c("NAC", "NaN", "NRA", "NI", "")) & !(is.na(x[2]) | x[2] %in% c("NAC", "NaN", "NRA", "NI", "")))
            { print(x)
              # readline()
            }
          })
          
          
        } # if same measurement.refs --> add to duplicate table with ForC.site.ID, along with ForC site
        
        # if different measurement.refs --> add to potential duplicates table, along with ForC site (unless the potential duplicated comes from GROA already and we know it is 2 different sites - in that case change the site.sitename and import the data into ForC)
        
        if(!ForC_data$SITES$measurement.refs[ ForC_data$SITES$sites.sitename %in% sites.sitename] %in% measurement.ref | is.na(measurement.ref)) {
          
          if(!is.na(forC_record$GROA.site.ID)) {
            newForC.siteID <- newForC.siteID + 1
            GROA_record_forc_format_x <- GROA_to_ForC_format_SITES(i, newForC.siteID)
            GROA_record_forc_format_x$sites.sitename <- deal.with.duplicate.site.name[deal.with.duplicate.site.name$GROA.site.ID %in% GROA.site.ID, ]$unique.sites.sitename
            
            ForC_data$SITES <- bind_rows(ForC_data$SITES, GROA_record_forc_format_x)
            
          } #  if(!is.na(forC_record$GROA.site.ID)) 
          
          if(is.na(forC_record$GROA.site.ID)) {
            GROA_record_forc_format_x <- GROA_to_ForC_format_SITES(i, NA)
            GROA_record_forc_format_x$site.ID <- GROA.site.ID
            
            if (!GROA.site.ID %in% potential.duplicate.sites$GROA.site.ID) {
              potential.duplicate.sites <- rbind(potential.duplicate.sites, 
                                                 bind_rows(data.frame(From = "ForC", forC_record), data.frame(From = "GROA", GROA_record_forc_format_x)))
            } else { cat("already in potential.duplicate.sites")
              
            }
          } #  if(is.na(forC_record$GROA.site.ID))
          
          
          
        } # if different measurement.refs --> add to potential duplicates table, along with ForC site
      } # if sitename does exists ####
      
      
      if(!sites.sitename %in% ForC_data$SITES$sites.sitename) { # if sitename does NOT exists ####
        newForC.siteID <- newForC.siteID + 1
        ForC_data$SITES <- bind_rows(ForC_data$SITES, GROA_to_ForC_format_SITES(i, newForC.siteID))
      } #if(!sites.sitename %in% ForC_data$SITES$sites.sitename)
      
      
    } #if(GROA.site.ID %in% ForC.GROA.potential.duplicate.manually.solved$GROA.site.ID)
    
    
  } # if(!GROA.site.ID %in%  ForC_data$SITES$GROA.site.ID)
  
} # for( i in 1:nrow(GROA_sites))

# Create a new record in CITATIONS if one does not already exist ####

## from GROA_litterature
for(i in 1:nrow(GROA_litterature)) {
  
  cat("--------------------", i, "/", nrow(GROA_litterature), "\n")
  cat("Citation Record:", paste(GROA_litterature[i, 1:4], collapse = " "), "\n")
  
  citation.ID_GROA <- GROA_litterature$citation.ID[i]
  
  if(citation.ID_GROA %in% ForC_data$CITATIONS$citation.ID) {
    cat("Found citation", citation.ID_GROA, "in CITATIONS table\n")
  } else {
    cat("Creating new citation:", citation.ID_GROA, "\n")
    
    ForC_data$CITATIONS <- bind_rows(
      ForC_data$CITATIONS,
      data.frame(
        citation.ID = citation.ID_GROA,
        citation.doi = "NAC",
        citation.author = GROA_litterature$citations.author[i],
        citation.year = GROA_litterature$citations.year[i],
        citation.title = GROA_litterature$citations.title[i],
        NEW_RECORD = TRUE
      )
    )
  }
}
problem.of.duplicates.within.GROA
# used to give this:
# >problem.of.duplicates.within.GROA
# [,1]                                       
# [1,] "ForC.siteID = 563 , GROA sites: 293 2417" 
# [2,] "ForC.siteID = 1142 , GROA sites: 100 3817"
# but we merged 293 with 2417 and 100 with 3817...


## from GROA_measurements --> non need to do it because all(GROA_measurements$citations.title %in% GROA_litterature$citations.title) is TRUE.



# Create a new record in MEASUREMENTS if one does not already exist ####

keeping.track.of.plot.names <- NULL
potential.duplicate.records <- NULL

for( i in 1:nrow(GROA_measurements)) {
  
  cat("--------------------", i, "/", nrow(GROA_measurements), "\n")
  cat("Measurement record:", paste(GROA_measurements[i, 1:4], collapse = " "), "\n")
  
  GROA.site.ID <- GROA_measurements$site.id[i]
  GROA.plot.ID <- GROA_measurements$plot.id[i]
  GROA.measurement.ID <- GROA_measurements$measurement.id[i]
  
  if(exists("add.record.to.forC")) rm(add.record.to.forC) # make sure we start from scratch for each record
  
  if(GROA.measurement.ID %in% ForC_data$MEASUREMENTS$GROA.measurement.ID) { #skip the whole thing since the measurement was already paired with or inported in ForC
    cat("Measurement record already paired with or imported in FroC\n")
  } # if(GROA.measurement.ID %in% ForC_data$MEASUREMENTS$GROA.measurement.ID)
  
  if(!GROA.measurement.ID %in% ForC_data$MEASUREMENTS$GROA.measurement.ID) { # Find out if measurement already exist in ForC, if it is a potential duplicate or if it should be imported
    
    # find corresponding sitename in ForC sites####
    ForC.site.ID <- ForC_data$SITES$site.ID[ForC_data$SITES$GROA.site.ID %in% GROA.site.ID]
    sites.sitename <- ForC_data$SITES$sites.sitename[ForC_data$SITES$GROA.site.ID %in% GROA.site.ID]# sites.sitename <- GROA_measurements$sites.sitename[i]
    
    # find records in the same sites in ForC MEASUREMENTS, idependant of GROA records already entered
    ForC.measurements.at.that.site <- ForC_data$MEASUREMENTS[ForC_data$MEASUREMENTS$sites.sitename %in% sites.sitename & is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID),]
  
    # record some values  that we will have to look at
    stand.age <- GROA_measurements$stand.age[i]
    
    GROA_variables.name <- GROA_measurements$variables.name[i]
    variable.name <- variable_name_conversion[variable_name_conversion$GROA_variables.name %in% GROA_variables.name, ]$ForC_variable.name
    
    citation.ID <- as.character(GROA_measurements$citation.ID[i])
    
    value <- GROA_measurements$mean_ha[i]
    
    # if there is no equivalent for this variable in ForC, do not import
    if(is.na(variable.name)) add.record.to.forC <- F 
    
    # if there is an equivalent for this variable in ForC, move forward with extra steps

    if(!is.na(variable.name)) {
      if(nrow(ForC.measurements.at.that.site) > 0 ) {    # if site already exists in ForC MEASUREMENTS
      ## Find out if the mearsurement is a duplicate. If it is (and if it is clear that the measurement was made in an existing ForC plot), replace the plot.name of all other measurments in that plot by the corresponding ForC plot.name and move on. If it is not, add new data to ForC. If it is unclear, save a file that collects potential duplicates for later manual revision. ####
      ## True duplicates are defines as same site.sitename, stand.age, variable.name, citation.ID and mean as in ForC measurements.
      
      
      
      corresponding.forC.records.looking.at.age <- ForC.measurements.at.that.site[!my_is.na(ForC.measurements.at.that.site$stand.age) & abs(as.numeric(ForC.measurements.at.that.site$stand.age) - stand.age) < 0.4
                                                                                  & abs(as.numeric(ForC.measurements.at.that.site$stand.age) - stand.age) == min(abs(as.numeric(ForC.measurements.at.that.site$stand.age) - stand.age))
                                                                                  & ForC.measurements.at.that.site$citation.ID %in% citation.ID,]
      
      
      unique.corresponding.ForC.plot.name.looking.at.age <- unique(corresponding.forC.records.looking.at.age$plot.name)
      
      
      if(length(unique.corresponding.ForC.plot.name.looking.at.age) == 1) { # if only one possible plot.name
        if(!GROA.plot.ID %in% keeping.track.of.plot.names$GROA.plot.ID) keeping.track.of.plot.names <- rbind(keeping.track.of.plot.names,
                                                                                                             data.frame(GROA.site.ID, GROA.plot.ID, sites.sitename, GROA.automated.plot.name = GROA_measurements$plot.name[i], ForC.plot.name = unique.corresponding.ForC.plot.name.looking.at.age))
        
        GROA_measurements[GROA_measurements$plot.id %in% GROA_measurements$plot.id[i], ]$plot.name <- unique.corresponding.ForC.plot.name.looking.at.age
        
        # then decide if record should be added to ForC
        
        if(!variable.name %in% corresponding.forC.records.looking.at.age$variable.name) add.record.to.forC <- TRUE # this is a new variable so yes, we want the record
        
        if(variable.name %in% corresponding.forC.records.looking.at.age$variable.name & any(abs(corresponding.forC.records.looking.at.age$mean[corresponding.forC.records.looking.at.age$variable.name %in% variable.name] - value) <= 0.4)) { # this is the same variable and the value is very close so we already have the record in ForC. Let's add the measurement.id into GROA.measurement.ID.
          add.record.to.forC <- FALSE 
          ForC_data$MEASUREMENTS$GROA.measurement.ID[ForC_data$MEASUREMENTS$measurement.ID %in% corresponding.forC.records.looking.at.age[abs(corresponding.forC.records.looking.at.age$mean[corresponding.forC.records.looking.at.age$variable.name %in% variable.name] - value) <= 0.4 & abs(corresponding.forC.records.looking.at.age$mean[corresponding.forC.records.looking.at.age$variable.name %in% variable.name] - value) == min(abs(corresponding.forC.records.looking.at.age$mean[corresponding.forC.records.looking.at.age$variable.name %in% variable.name] - value) <= 0.4), ]$measurement.ID] <- GROA.measurement.ID
        }
        
        if(variable.name %in% corresponding.forC.records.looking.at.age$variable.name & !any(abs(corresponding.forC.records.looking.at.age$mean[corresponding.forC.records.looking.at.age$variable.name %in% variable.name] - value) <= 0.4)) { # potential duplicate --> put that in a file for later manual review
          potential.duplicate.records <- rbind(potential.duplicate.records, 
                                              cbind(From = "ForC",  
                                                    corresponding.forC.records.looking.at.age[corresponding.forC.records.looking.at.age$variable.name %in% variable.name, c("measurement.ID", "sites.sitename", "plot.name", "stand.age", "citation.ID", "variable.name", "mean", "GROA.measurement.ID")]),
                                              data.frame(From = "GROA", measurement.ID = GROA.measurement.ID, sites.sitename, plot.name = GROA.plot.ID, stand.age, citation.ID, variable.name, mean = value, GROA.measurement.ID = GROA.measurement.ID))
          add.record.to.forC <- FALSE # don't add that data yet!
        }
        
      } # if(length(unique.corresponding.ForC.plot.name.looking.at.age) == 1)
      
      if(length(unique.corresponding.ForC.plot.name.looking.at.age) > 1) { # if more than one possible plot.name
        
        # is there a record that is exactly the same that we can use to find out what plot we are talking about ?
        
        corresponding.forC.records.looking.at.more.than.age <- corresponding.forC.records.looking.at.age[
          corresponding.forC.records.looking.at.age$citation.ID %in% citation.ID 
          & corresponding.forC.records.looking.at.age$variable.name %in% variable.name 
          & abs(corresponding.forC.records.looking.at.age$mean - value) <= 0.4  # 0.4 to allow rounding
          & abs(corresponding.forC.records.looking.at.age$mean - value) == min(abs(corresponding.forC.records.looking.at.age$mean - value)),]
        
        unique.corresponding.ForC.plot.name.second.pass.looking.at.more.than.age <- unique(corresponding.forC.records.looking.at.more.than.age$plot.name)
        
        if(length(unique.corresponding.ForC.plot.name.second.pass.looking.at.more.than.age) == 1) { # if only one possible plot.name because same variable and same value, we have a true duplicate. so let's keep that record and add the measurement.id into GROA.measurement.ID.
          
          if(!GROA.plot.ID %in% keeping.track.of.plot.names$GROA.plot.ID) keeping.track.of.plot.names <- rbind(keeping.track.of.plot.names,
                                                                                                               data.frame(GROA.site.ID, GROA.plot.ID, sites.sitename, GROA.automated.plot.name = GROA_measurements$plot.name[i], ForC.plot.name = unique.corresponding.ForC.plot.name.second.pass.looking.at.more.than.age))
          
          GROA_measurements[GROA_measurements$plot.id %in% GROA_measurements$plot.id[i], ]$plot.name <- unique.corresponding.ForC.plot.name.second.pass.looking.at.more.than.age
          
          add.record.to.forC <- FALSE 
          ForC_data$MEASUREMENTS$GROA.measurement.ID[ForC_data$MEASUREMENTS$measurement.ID %in% corresponding.forC.records.looking.at.age[abs(corresponding.forC.records.looking.at.age$mean[corresponding.forC.records.looking.at.age$variable.name %in% variable.name] - value) <= 0.4 & abs(corresponding.forC.records.looking.at.age$mean[corresponding.forC.records.looking.at.age$variable.name %in% variable.name] - value) == min(abs(corresponding.forC.records.looking.at.age$mean[corresponding.forC.records.looking.at.age$variable.name %in% variable.name] - value) <= 0.4), ]$measurement.ID] <- GROA.measurement.ID
          
        }
        
        if(length(unique.corresponding.ForC.plot.name.second.pass.looking.at.more.than.age) > 1) {
          stop("still more than one plot possible")
          add.record.to.forC <- FALSE
        }
        
        if(length(unique.corresponding.ForC.plot.name.second.pass.looking.at.more.than.age) == 0 & nrow(corresponding.forC.records.looking.at.age) > 0) {
          potential.duplicate.records <- rbind(potential.duplicate.records, 
                                              cbind(From = "ForC",  
                                                    corresponding.forC.records.looking.at.age[corresponding.forC.records.looking.at.age$variable.name %in% variable.name, c("measurement.ID", "sites.sitename", "plot.name", "stand.age", "citation.ID", "variable.name", "mean", "GROA.measurement.ID")]),
                                              data.frame(From = "GROA", measurement.ID = GROA.measurement.ID, sites.sitename, plot.name = GROA.plot.ID, stand.age, citation.ID, variable.name, mean = value, GROA.measurement.ID = GROA.measurement.ID))
          add.record.to.forC <- FALSE
        }
        
      } # if(length(unique.corresponding.ForC.plot.name) > 1)
      
      if(length(unique.corresponding.ForC.plot.name.looking.at.age) == 0 ) { # leave the automated plotname and add the record to ForC
        add.record.to.forC = TRUE
      } # if(length(unique.corresponding.ForC.plot.name.looking.at.age) == 0)
      
      
      
      
      
      
    } # if(nrow(ForC.measurements.at.that.site) > 0 ) 
    
    if(nrow(ForC.measurements.at.that.site) == 0 ) {# if site does not alrweady exists in ForC MEASUREMENTS
      add.record.to.forC <- TRUE
    } # if(nrow(ForC.measurements.at.that.site) == 0 )
    } # if(!is.na(variable.name))
    
    
    # import data if deemed necessary ####
    
    if(add.record.to.forC) {
      
      cat("Creating new record:",paste(sites.sitename, variable.name), "\n")
      
      # deal with covariates separately here ####
      if(any(GROA_measurements[, c("covar_1","covar_2", "covar_3")][i,] %in% c("min_dbh"))) {
        idx.min_dbh <- which(GROA_measurements[, c("covar_1","covar_2", "covar_3")][i,] %in% "min_dbh")
        min.dbh = as.character(GROA_measurements[, c("coV1_value","coV2_value", "coV3_value")][i,][idx.min_dbh])
        
      } else {
        min.dbh = "NAC"
      }
      
      if(any(GROA_measurements[, c("covar_1","covar_2", "covar_3")][i,] %in% c("max_depth"))) {
        idx.max_depth <- which(GROA_measurements[, c("covar_1","covar_2", "covar_3")][i,] %in% "max_depth")
        depth = as.character(GROA_measurements[, c("coV1_value","coV2_value", "coV3_value")][i,][[idx.max_depth]])
      } else {
        depth = "NAC"
      }
      
      other.covariates.idx <- !GROA_measurements[, c("covar_1","covar_2", "covar_3")][i,] %in% c("min_dbh", "max_depth", "component", "components") # also ignore component and components as they will be transfered to variable name an mean.
      other.covariates <- unlist(GROA_measurements[, c("covar_1","covar_2", "covar_3")][i,][other.covariates.idx], use.names = F)
      other.cov.values <- unlist(GROA_measurements[, c("coV1_value","coV2_value", "coV3_value")][i,][other.covariates.idx], use.names = F)
      
      component.idx <- GROA_measurements[, c("covar_1","covar_2", "covar_3")][i,] %in% c("component", "components")
      GROA_component <- unlist(GROA_measurements[, c("coV1_value","coV2_value", "coV3_value")][i,][component.idx], use.names = F)
      
      if(!is.null(GROA_component)) {
        
        if(variable.name %in% c("total.ecosystem_2_C") & !GROA_component %in% c("forest floor + CWD", "litter + dead woody debris")) include_record = FALSE
        
        if(variable.name %in% c("organic.layer_OM")) {
          if(GROA_component %in% c("litter", "forest floor")) include_record = TRUE
          if(GROA_component %in% c("CWD", "FWD + CWD", "FWD+CWD")) { include_record = TRUE ; variable.name = "deadwood_down_OM" }
          if(GROA_component %in% c("forest floor + CWD", "litter + dead woody debris")) include_record = FALSE
        }
        
        if(variable.name %in% c("organic.layer_C")) {
          if(GROA_component %in% c("litter", "forest floor")) include_record = TRUE
          if(GROA_component %in% c("CWD", "FWD + CWD", "FWD+CWD")) { include_record = TRUE ; variable.name = "deadwood_down_C" }
          if(GROA_component %in% c("forest floor + CWD", "litter + dead woody debris")) include_record = FALSE
        }
        
      } # if(!is.na(GROA_component))
      
      # append to measurements table now ####
      ForC_data$MEASUREMENTS <-  bind_rows( #####
                                            ForC_data$MEASUREMENTS,
                                            data.frame( #####
                                                        measurement.ID = c(max(ForC_data$MEASUREMENTS$measurement.ID)+1, max(ForC_data$MEASUREMENTS$measurement.ID) + 2)[c(TRUE, !is.na(GROA_measurements$density[i]))],
                                                        sites.sitename,
                                                        plot.name = GROA_measurements$plot.name[i],
                                                        stand.age = as.character(GROA_measurements$stand.age[i]),
                                                        dominant.life.form = ifelse(GROA_measurements$refor.type[i] %in% "PA", "2GW",
                                                                                    ifelse(GROA_measurements$refor.type[i] %in% "C", "NAC", "woody")),
                                                        dominant.veg = "NAC",
                                                        veg.notes = GROA_measurements$Species[i],
                                                        variable.name = c(variable.name, "stand.density")[c(TRUE, !is.na(GROA_measurements$density[i]))],
                                                        date = as.character(ifelse(is.na(GROA_measurements$date[i]), "NI", GROA_measurements$date[i])),
                                                        date.loc = ifelse(is.na(GROA_measurements$date[i]), 9, 8),
                                                        start.date.loc = 9,
                                                        end.date.loc = 9,
                                                        mean = c(GROA_measurements$mean_ha[i], GROA_measurements$density[i])[c(TRUE, !is.na(GROA_measurements$density[i]))],
                                                        n = as.character(ifelse(is.na(GROA_measurements$n[i]), "NI", GROA_measurements$n[i])),
                                                        area.sampled = as.character(ifelse(is.na(GROA_measurements$n[i] * GROA_measurements$plot.size[i]), "NI", GROA_measurements$n[i] * GROA_measurements$plot.size[i])),
                                                        notes = paste0(ifelse(is.na(GROA_measurements$allometry[i]), "", paste0("Biomass allometries: ", GROA_measurements$allometry[i], ".")),
                                                                       ifelse(is.na(GROA_measurements$sub_n[i]), "", paste0(" ", GROA_measurements$sub_n[i], " sample(s) per plot.")),
                                                                       ifelse(is.null(GROA_component), "", paste0(" GROA component: ", GROA_component, "."))),
                                                        
                                                        allometry_1 = "NAC",
                                                        allometry_2 = NA,
                                                        
                                                        min.dbh,
                                                        depth,
                                                        
                                                        covariate_1 = other.covariates[1],
                                                        coV_1.value = as.character(other.cov.values[1]),
                                                        covariate_2 = other.covariates[2],
                                                        coV_2.value = as.character(other.cov.values[2]),
                                                        
                                                        citation.ID = GROA_litterature$citation.ID[GROA_litterature$study.id %in% GROA_measurements$study.id[i]],
                                                        source.notes = paste0("GROA measurement.ID #", GROA.measurement.ID),
                                                        loaded.from = "[Cook-Patton database citation, in ForC format]",
                                                        loaded.by = paste("R script by Valentine Herrmann"),
                                                        checked.ori.pub = ifelse(grepl("Guo|Krankina", GROA_litterature$citation.ID[GROA_litterature$study.id %in% GROA_measurements$study.id[i]]), 0, 1), # "1" for all studies, except "0" for Guo and Krankina
                                                        ForC.investigator = "Dr. Susan Cook-Patton",
                                                        required.citations = "[Cook-Patton database citation, in ForC format]",
                                                        flag.suspicious = 0,
                                                        GROA.measurement.ID,
                                                        NEW_RECORD = TRUE
                                            )
      )
      
    } # if(add.record.to.forC)
    
  } # if(!GROA.measurement.ID %in% ForC_data$MEASUREMENTS$GROA.measurement.ID) 
  
} # for( i in 1:nrow(GROA_measurements)) 
 
tail(ForC_data$MEASUREMENTS)
dim(keeping.track.of.plot.names)
dim(potential.duplicate.records)


# Create a new record in HISTORY if one does not already exist ####

# group GROA_measurements by sites.sitename and plotname (but first get the right site names)
GROA_measurements$new.sites.sitename <- ForC_data$SITES$sites.sitename[match(GROA_measurements$site.id, ForC_data$SITES$GROA.site.ID)]


# site_plots <- unique(paste(ForC_data$MEASUREMENTS[ForC_data$MEASUREMENTS$NEW_RECORD == 1,]$sites.sitename, ForC_data$MEASUREMENTS[ForC_data$MEASUREMENTS$NEW_RECORD == 1,]$plot.name))
# g_m_id <- ForC_data$MEASUREMENTS[ForC_data$MEASUREMENTS$NEW_RECORD == 1,]$GROA.measurement.ID[paste(ForC_data$MEASUREMENTS[ForC_data$MEASUREMENTS$NEW_RECORD == 1,]$sites.sitename, ForC_data$MEASUREMENTS[ForC_data$MEASUREMENTS$NEW_RECORD == 1,]$plot.name) %in% s_p]


site_plots <- unique(paste(ForC_data$MEASUREMENTS[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID),]$sites.sitename, ForC_data$MEASUREMENTS[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID),]$plot.name))

s_p.with.age.date.discrepancy <- NULL
s_p.with.chronosequence.to.fix <- NULL
s_p.with.other.date.age.issues.to.fix <- NULL

something.not.unique.when.it.should <- NULL

for(s_p in site_plots) {
  cat(paste("creating HISTORY for", s_p, "\n"))
  
  
  g_m_id <- ForC_data$MEASUREMENTS$GROA.measurement.ID[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID)][paste(ForC_data$MEASUREMENTS[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID),]$sites.sitename, ForC_data$MEASUREMENTS[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID),]$plot.name) %in% s_p]
  groa_sub <- GROA_measurements[GROA_measurements$measurement.id %in% g_m_id, ]
  
  # groa_sub <- GROA_measurements[paste(GROA_new_site.sitenames, GROA_measurements$plot.name) %in% s_p, ]
  
  sites.sitename <- unique(groa_sub$new.sites.sitename)
  site.id <- unique(groa_sub$site.id)
  GROA.plot.ID <- unique(groa_sub$plot.id)
  plot.name <- unique(groa_sub$plot.name)
  plot.area <- as.character(ifelse(is.na(unique(groa_sub$n * groa_sub$plot.size)), "NAC", unique(groa_sub$n * groa_sub$plot.size)))
  refor.type <- unique(groa_sub$refor.type)
  prior <- unique(groa_sub$prior)
  prior.duration <- unique(groa_sub$prior.disturbance.notes)
  
  if(s_p %in% paste(ForC_data$HISTORY$sites.sitename, ForC_data$HISTORY$plot.name)) {
    cat("Found site", sites.sitename, "\n")
  } else {
    
    if(any(sapply(list(sites.sitename, site.id, GROA.plot.ID, plot.name, plot.area, refor.type, prior, prior.duration), length) != 1)) something.not.unique.when.it.should <- rbind(something.not.unique.when.it.should, data.frame(variable = c("sites.sitename", "site.id", "GROA.plot.ID", "plot.name", "plot.area", "refor.type", "prior", "prior.duration")[ which(sapply(list(sites.sitename, site.id, GROA.plot.ID, plot.name, plot.area, refor.type, prior, prior.duration), length) >1)], values = sapply(list(sites.sitename, site.id, GROA.plot.ID, plot.name, plot.area, refor.type, prior, prior.duration), paste, collapse = ",")[ which(sapply(list(sites.sitename, site.id, GROA.plot.ID, plot.name, plot.area, refor.type, prior, prior.duration), length) >1)], site.plot.name = s_p, site.id, plot.id = GROA.plot.ID))
    
    if(!any(sapply(list(sites.sitename, site.id, plot.name, plot.area, refor.type, prior, prior.duration), length) != 1)) {
      
      
      date <- unique(groa_sub$date)
      stand.age <- unique(groa_sub$stand.age)
      
      ## date age discrepancy
      if(length(date) > 1 & any(!is.na(stand.age) & length(stand.age) == 1)) { # Here lets assume that the age applies to the first year of measurements
        s_p.with.age.date.discrepancy <- c(s_p.with.age.date.discrepancy, s_p)
        Study_midyear <- Study_midyear[1]
      }
      
      ## chronosequence issue
      if(length(date) == 1 & (length(stand.age) > 1) ) { 
        s_p.with.chronosequence.to.fix <- c(s_p.with.chronosequence.to.fix, s_p)
        warning("chronosequence issue")
        stand.age <- stand.age[1]
      }
      
      
      ## other issues
      if((length(date) > 1 & any(is.na(date))) | (length(stand.age) > 1 & any(is.na(stand.age)))) { 
        s_p.with.other.date.age.issues.to.fix <- c(s_p.with.other.date.age.issues.to.fix, s_p)
        warning("chronosequence issue")
        date <- date[!is.na(date)][1]
        stand.age <- stand.age[!is.na(stand.age)][1]
      }
      
      
      
      
      if(refor.type %in% "SNR") {
        
        if(prior %in% "") { # if prior is empty
          event.sequence <- 1:2
          hist.cat <- c("Establishment", "Regrowth")
          hist.type <- c("Establishment of oldest trees", "Initiation of post-disturbance cohort (planted or natural)")
          
          
          percent.mortality <- NA
          
          est.regrowth.assumed.same.year <- "1"
          
          percent.mortality <- NA
          
          hist.notes <- NA
          
          level <- NA

          
        }
        
        if(!prior %in% "") { # if prior is NOT empty
          
          hist.type <-  c(ifelse(prior %in% "C", 'Cultivation', 
                                 ifelse(prior %in% "SC", "Shifting cultivation",
                                        ifelse(prior %in% "H", "Harvest",
                                               ifelse(prior %in% "F", "Burned",
                                                      ifelse(prior %in% "D", "NAC[TKA1]",
                                                             ifelse(prior %in% "PA", "Grazed",
                                                                    ifelse(prior %in% c("C/PA", "PA/C", 
                                                                                        "SC/PA/H", 
                                                                                        "PA/C", "SC/PA", "SC,PA", "SC & PA", "SC,PA", "SC, PA"), 'Agriculture_generic', 
                                                                           ifelse(prior %in% c("H/D", "F/D", "H/F", "F/H", "M"), "StandClearing",
                                                                                  ifelse(prior %in% c("SC/C", "TMC "), "Cultivation", NA))))))))))
          
          
          percent.mortality <-  c(ifelse(prior %in% "C", '100', 
                                         ifelse(prior %in% "SC", "100",
                                                ifelse(prior %in% "H", "100",
                                                       ifelse(prior %in% "F", "NAC",
                                                              ifelse(prior %in% "D", "NAC",
                                                                     ifelse(prior %in% "PA", "100",
                                                                            ifelse(prior %in% c("C/PA", "PA/C", 
                                                                                                "SC/PA/H", 
                                                                                                "PA/C", "SC/PA", "SC,PA", "SC & PA", "SC,PA", "SC, PA"), '100',
                                                                                   ifelse(prior %in% c("H/D", "F/D", "H/F", "F/H", "M"), "100",
                                                                                          ifelse(prior %in% c("SC/C", "TMC "), "100", NA))))))))))
          
          if(prior %in% "PA/H") {
            hist.type <- c("Harvest", "Grazed")
            percent.mortality <- "100"
          }
          
          if(prior %in% "PA/F") {
            hist.type <- c("Burned", "Grazed")
            percent.mortality <- "100"
          }
          
          if(prior %in% "C/H") {
            hist.type <- c("Harvest", "Cultivation")
            percent.mortality <- "100"
          }
          
          if(prior %in% "SC/F") {
            hist.type <- c("Fire", "Shifting cultivation")
            percent.mortality <- "100"
          }
          
          
          
          event.sequence <- c(1:length(hist.type), (length(hist.type) +1) :(length(hist.type) +2))
          
          hist.cat <- c(rep("Disturbance", length(hist.type)), c("Establishment", "Regrowth"))
          est.regrowth.assumed.same.year <- rep(NA, length(hist.type), "1", "1")
          level <- c(rep("NAC", length(hist.type)), NA, NA)
          percent.mortality <-  c(rep(percent.mortality, length(hist.type)), NA, NA)
          
          hist.notes <- c(rep(prior.duration, length(hist.type)), NA, NA)
          
          hist.type <- c(hist.type, "Establishment of oldest trees", "Initiation of post-disturbance cohort (planted or natural)")
          
          hist.notes <- NA
          
        }
        
        
      } # if(refor.type %in% "SNR"
      
      if( !refor.type %in% "SNR") {
        event.sequence <- 1
        hist.cat <- ifelse(refor.type %in% "OG", "No.disturbance", "Disturbance")
        hist.type <-  c(ifelse(refor.type %in% "C", 'Cultivation', 
                               ifelse(refor.type %in% "SC", "Shifting cultivation",
                                      ifelse(refor.type %in% "H", "Harvest",
                                             ifelse(refor.type %in% "F", "Burned",
                                                    ifelse(refor.type %in% "D", "NAC",
                                                           ifelse(refor.type %in% "PA", "Grazed",
                                                                  ifelse(refor.type %in% "OG", "No severe disturbance", NA))))))))
        percent.mortality <-  c(ifelse(refor.type %in% "C", '100', 
                                       ifelse(refor.type %in% "SC", "100",
                                              ifelse(refor.type %in% "H", "100",
                                                     ifelse(refor.type %in% "F", "NAC",
                                                            ifelse(refor.type %in% "D", "NAC",
                                                                   ifelse(refor.type %in% "PA", "100",
                                                                          ifelse(refor.type %in% "OG", NA, NA))))))))
        est.regrowth.assumed.same.year <- NA
        level <- NA
        hist.notes <- "[prior.duration]"
        
      }  # if( !refor.type %in% "SNR")
      
      
      date <- as.character(unique(ifelse(!is.na(date - stand.age), floor(date - stand.age), "NAC")))
      date.loc <- ifelse(date %in% "NAC", 9, 8)
      
      units = NA
      
      
      rows.to.add <- data.frame( #####
                                 history.ID = ceiling(max(ForC_data$HISTORY$history.ID)) + seq(event.sequence) / 100,
                                 sites.sitename,
                                 plot.name,
                                 plot.area,
                                 event.sequence,
                                 date,
                                 date.loc,
                                 hist.cat = unlist(hist.cat),
                                 hist.type = unlist(hist.type),
                                 est.regrowth.assumed.same.year,
                                 level,
                                 units,
                                 percent.mortality = unlist(percent.mortality),
                                 hist.notes,
                                 NEW_RECORD = TRUE,
                                 GROA.plot.ID,
                                 stringsAsFactors = F
      )
      
      
      ForC_data$HISTORY <-  bind_rows(ForC_data$HISTORY, rows.to.add)
    } # if(length(manip) == 1)
  } #  if(sites.sitename %in% forc_data$HISTORY$sites.sitename)
  
} # for(s_p in site_plots) {

s_p.with.age.date.discrepancy
s_p.with.chronosequence.to.fix
s_p.with.other.date.age.issues.to.fix
nrow(something.not.unique.when.it.should)
head(something.not.unique.when.it.should)
table(something.not.unique.when.it.should$variable)
something.not.unique.when.it.should[something.not.unique.when.it.should$variable %in% "plot.area",]
something.not.unique.when.it.should[something.not.unique.when.it.should$variable %in% "prior",]


site.id.plot.id.more.than.one.prior<- NULL
for(s_p in something.not.unique.when.it.should[something.not.unique.when.it.should$variable %in% "prior",]$site.plot.name) {
  g_m_id <- ForC_data$MEASUREMENTS$GROA.measurement.ID[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID)][paste(ForC_data$MEASUREMENTS[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID),]$sites.sitename, ForC_data$MEASUREMENTS[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID),]$plot.name) %in% s_p]
  groa_sub <- GROA_measurements[GROA_measurements$measurement.id %in% g_m_id, ]
  print(groa_sub)
  readline("press[enter]")
  site.id.plot.id.more.than.one.prior <- rbind(site.id.plot.id.more.than.one.prior,  data.frame(unique(groa_sub[, c("site.id", "plot.id")]), measurement.id = paste(groa_sub$measurement.id, collapse = ", ")))
}

site.id.plot.id.more.than.one.prior.duration <- NULL
for(s_p in something.not.unique.when.it.should[something.not.unique.when.it.should$variable %in% "prior.duration",]$site.plot.name) {
  g_m_id <- ForC_data$MEASUREMENTS$GROA.measurement.ID[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID)][paste(ForC_data$MEASUREMENTS[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID),]$sites.sitename, ForC_data$MEASUREMENTS[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID),]$plot.name) %in% s_p]
  groa_sub <- GROA_measurements[GROA_measurements$measurement.id %in% g_m_id, ]
  print(groa_sub)
  readline("press[enter]")
  site.id.plot.id.more.than.one.prior.duration <- rbind(site.id.plot.id.more.than.one.prior.duration, data.frame(unique(groa_sub[, c("site.id", "plot.id")]), measurement.id = paste(groa_sub$measurement.id, collapse = ", ")))
}


site.id.plot.id.more.than.one.plot.area <- NULL
for(s_p in something.not.unique.when.it.should[something.not.unique.when.it.should$variable %in% "plot.area",]$site.plot.name) {
  g_m_id <- ForC_data$MEASUREMENTS$GROA.measurement.ID[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID)][paste(ForC_data$MEASUREMENTS[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID),]$sites.sitename, ForC_data$MEASUREMENTS[!is.na(ForC_data$MEASUREMENTS$GROA.measurement.ID),]$plot.name) %in% s_p]
  groa_sub <- GROA_measurements[GROA_measurements$measurement.id %in% g_m_id, ]
  # print(groa_sub)
  # readline("press[enter]")
  site.id.plot.id.more.than.one.plot.area <- rbind(site.id.plot.id.more.than.one.plot.area, data.frame(unique(groa_sub[, c("site.id", "plot.id")]), measurement.id = paste(groa_sub$measurement.id, collapse = ", ")))
}


cat("All done.\n")

# double check a few things ####

all(ForC_data$MEASUREMENTS$sites.sitename[ForC_data$MEASUREMENTS$NEW_RECORD] %in% ForC_data$SITES$sites.sitename)
sum(!(ForC_data$MEASUREMENTS$sites.sitename[ForC_data$MEASUREMENTS$NEW_RECORD] %in% ForC_data$HISTORY$sites.sitename))
sum(!(ForC_data$MEASUREMENTS$plot.name[ForC_data$MEASUREMENTS$NEW_RECORD] %in% ForC_data$HISTORY$plot.name))

all(paste(ForC_data$MEASUREMENTS$sites.sitename, ForC_data$MEASUREMENTS$plot.name)[ForC_data$MEASUREMENTS$NEW_RECORD][!ForC_data$MEASUREMENTS$plot.name[ForC_data$MEASUREMENTS$NEW_RECORD] %in% ForC_data$HISTORY$plot.name] %in% something.not.unique.when.it.should$site.plot.name) # ok good

# save ####

write.csv(true.duplicate.sites, file = "new_data/potential_duplicates/true.duplicate.sites.csv", row.names = F)
# write.csv(potential.duplicate.sites, file = "new_data/potential_duplicates/potential.duplicate.sites.csv", row.names = F)
# write.csv(potential.duplicate.records, file = "new_data/potential_duplicates/potential.duplicate.records.csv", row.names = F)

sum(ForC_data$CITATIONS$NEW_RECORD)
sum(ForC_data$MEASUREMENTS$NEW_RECORD)
sum(ForC_data$SITES$NEW_RECORD)
sum(ForC_data$HISTORY$NEW_RECORD)

write.csv(filter(ForC_data$SITES, NEW_RECORD==TRUE), "new_data/new_SITES.csv", row.names = F)
write.csv(filter(ForC_data$CITATIONS, NEW_RECORD==TRUE), "new_data/new_CITATIONS.csv", row.names = F)
write.csv(filter(ForC_data$MEASUREMENTS, NEW_RECORD==TRUE), "new_data/new_MEASUREMENTS.csv", row.names = F)
write.csv(filter(ForC_data$HISTORY, NEW_RECORD==TRUE), "new_data/new_HISTORY.csv", row.names = F)



# Sites.to.save <- ForC_data$SITES[ForC_data$SITES$sites.sitename %in% ForC_data$MEASUREMENTS$sites.sitename[ForC_data$MEASUREMENTS$NEW_RECORD],]
# History.to.save <- ForC_data$HISTORY[paste(ForC_data$HISTORY$sites.sitename, ForC_data$HISTORY$plot.name) %in% paste(ForC_data$MEASUREMENTS$sites.sitename, ForC_data$MEASUREMENTS$plot.name)[ForC_data$MEASUREMENTS$NEW_RECORD],]
# 
# write.csv(Sites.to.save, "new_data/new_SITES.csv", row.names = F)
# write.csv(History.to.save, "new_data/new_HISTORY.csv", row.names = F)



