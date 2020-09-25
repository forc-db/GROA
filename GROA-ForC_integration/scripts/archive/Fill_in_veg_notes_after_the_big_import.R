######################################################
# Purpose: add  veg.notes = GROA_measurements$Species[i], after the fact (first big import ignored that field mistakenly)
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.4 (2018-03-15)
######################################################

# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd("C:/Users/HerrmannV/Dropbox (Smithsonian)/GitHub/forc-db/GROA") # needs to be the GROA repo (not GROA-ForC_integration sub-folder)

# Load libaries ####
library(RCurl)
library(dplyr)

# Load data ####

## GROA data ####
GROA_measurements <-  read.csv("data/biomass_litter_CWD.csv", stringsAsFactors = F)

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
ForC_files <- ForC_files[!ForC_files %in% "ForC_sites_missing_coordinates.csv"]

ForC_data <- NULL
for(f in c("ForC_measurements.csv")) {
  cat("Reading", f, "...\n")
  name <- toupper(gsub("ForC_|\\.csv", "", f))
  ForC_data[[name]] <- read.csv(text=getURL(paste0("https://raw.githubusercontent.com/forc-db/ForC/master/data/", f)), header=T, stringsAsFactors = F)
  
  ForC_data[[name]]$NEW_RECORD <- FALSE
  
  if(f %in% c("ForC_history.csv", "ForC_sites.csv")) ForC_data[[name]]$GROA.site.ID <- NA
  if(f %in% c("ForC_measurements.csv")) ForC_data[[name]]$GROA.measurement.ID <- NA
} # for(f in forC_files)


## measurement mapping ####
measurementID_mapping <- read.csv("GROA-ForC_integration/GROA-ForC mapping/GROA-ForC_measurements_mapping.csv")


# fill in veg.note ####

for(i in 1:nrow(measurementID_mapping)) {
  ID_ForC <- measurementID_mapping$ForC.measurement.ID[i]
  ID_GROA <- measurementID_mapping$GROA.measurement.ID[i]
  
  idx_ForC <- ForC_data$MEASUREMENTS$measurement.ID %in% ID_ForC
  idx_GROA <- GROA_measurements$measurement.id %in% ID_GROA
  
  veg.notes_ForC <- ForC_data$MEASUREMENTS$veg.notes[idx_ForC]
  species_GROA <- GROA_measurements$species[idx_GROA]
  
  if(!species_GROA %in% "" & length(veg.notes_ForC)>0) { # GROA species is not empty
    if(veg.notes_ForC %in%  "tropical wet forest") stop()
    new_veg.notes_ForC <- paste(ifelse(my_is.na(veg.notes_ForC) | veg.notes_ForC == "", "", veg.notes_ForC), paste0("GROA species = ", species_GROA), sep = ifelse(my_is.na(veg.notes_ForC) | veg.notes_ForC == "", "", "; "))
    
    ForC_data$MEASUREMENTS$veg.notes[idx_ForC] <- new_veg.notes_ForC
  } 

  
}

write.csv(select(ForC_data$MEASUREMENTS, -NEW_RECORD, -GROA.measurement.ID), paste0(dirname(getwd()), "/forc/data/ForC_measurements.csv"), row.names = F)




