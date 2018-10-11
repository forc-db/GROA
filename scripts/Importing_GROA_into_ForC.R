######################################################
# Purpose: Import GROA data into ForC
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
# GROA_measurements <-  read.csv("example data/GROA_measurements.csv", stringsAsFactors = F)
# GROA_sites <-  read.csv("example data/GROA_sites.csv", stringsAsFactors = F)
# GROA_litterature <-  read.csv("example data/GROA_litterature.csv", stringsAsFactors = F)

GROA_measurements <-  read.csv("data/non_soil_data.csv", stringsAsFactors = F)
GROA_sites <-  read.csv("data/sitesf.csv", stringsAsFactors = F)
GROA_litterature <-  read.csv("data/GROA literature.csv", stringsAsFactors = F)

## ForC data ####
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
} # for(f in forC_files)

## variable_name_conversion table ####
variable_name_conversion <- read.csv("GROA-ForC mapping/variable_name_conversion.csv", stringsAsFactors = F)

# Settings ####
## set refor.type to plot.name key
refor.type.to.plot.name.key <-  data.frame(plot.name = c("regrowth stand regenerating via spontaneous natural regeneration","regrowth stand regenerating via assisted natural regeneration","regrowth stand regenerating via initial tree planting", "diverse species plantation", "monoculture plantation", "intensive tree monocrop", "multistrata stand", "stand with tree intercropping", "silvopastoral system", "transitional ecosystem", "cropland", "pasture", "intact/ old growth stand"), row.names = c("SNR", "ANR", "ITP", "DP", "MP", "TMC", "MS", "TI", "SP", "TR", "C", "PA", "OG"), stringsAsFactors = F)

# pre-fixes and preparations ####
## create citation.ID
GROA_litterature$citation.ID <- paste(GROA_litterature$citations.author, GROA_litterature$citations.year,  substr(gsub('(?<!-)(?<!\')\\b(\\pL)|.','\\L\\1', GROA_litterature$citations.title, perl = T), 1, 4), sep = "_") # create citation ID in the form [last name of first author]_[publication year]_[first letter of first four words of title, when applicable, counting hyphenated words as single words].


# Create a new record in SITES if one does not already exist ####
for( i in 1:nrow(GROA_sites)) {
  
  cat("--------------------", i, "/", nrow(GROA_sites), "\n")
  cat("Record:", paste(GROA_sites[i, 1:4], collapse = " "), "\n")
  
  sites.sitename <- GROA_sites$site.sitename[i]
  if(sites.sitename %in% ForC_data$SITES$sites.sitename) {
    cat("Found site", sites.sitename, "\n")
  } else {
    cat("Creating new site:", sites.sitename, "\n")
    
    # deal wit soil texture separately here
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
    
    # deal with the easier stuff here
    ForC_data$SITES <- bind_rows(
      ForC_data$SITES ,
      data.frame(
        sites.sitename = sites.sitename,
        site.ID = max(ForC_data$SITES$site.ID) + 1,
        state = GROA_sites$site.state[i],
        country = GROA_sites$site.country[i],
        lat = GROA_sites$lat_dec[i],
        lon = GROA_sites$long_dec[i],
        masl = as.character(GROA_sites$elevation[i]),
        mat = as.character(GROA_sites$AMT[i]),
        map = as.character(GROA_sites$AMP[i]),
        soil.texture = soil.texture,
        sand = as.character(sand),
        silt = as.character(silt),
        clay = as.character(clay),
        soil.classification = GROA_sites$soil.classification[i],
        # site.notes = GROA_sites$site.notes[i],
        ref.notes = as.character(GROA_sites$site.id[i]),
        loaded.from = "[Cook-Patton database citation, in ForC format]",
        loaded.by = "R script by Valentine Herrmann",
        NEW_RECORD = TRUE
        # tropical.extratropical = 
      )
    )
  }
}


# Create a new record in CITATIONS if one does not already exist ####

## from GROA_litterature
for(i in 1:nrow(GROA_litterature)) {
  
  cat("--------------------", i, "/", nrow(GROA_litterature), "\n")
  cat("Record:", paste(GROA_litterature[i, 1:4], collapse = " "), "\n")
  
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

## from GROA_measurements (if citation.title comes back)

# Create a new record in MEASUREMENTS if one does not already exist ####
for( i in 1:nrow(GROA_measurements)) {
  
  cat("--------------------", i, "/", nrow(GROA_measurements), "\n")
  cat("Record:", paste(GROA_measurements[i, 1:4], collapse = " "), "\n")
  
  sites.sitename <- GROA_measurements$sites.sitename[i]
  GROA_variables.name <- GROA_measurements$variables.name[i]
  
  variable.name <- variable_name_conversion[variable_name_conversion$GROA_variables.name %in% GROA_variables.name, ]$ForC_variable.name
  
  if(paste(sites.sitename, variable.name) %in% paste(ForC_data$MEASUREMENTS$sites.sitename, ForC_data$MEASUREMENTS$variable.name)) {
    cat("Found record", paste(sites.sitename, variable.name), "in MEASUREMENTS table\n")
  } else {
    cat("Creating new record:",paste(sites.sitename, variable.name), "\n")
    include_record  = TRUE
    
    # deal with covariates separately here
    if(any(GROA_measurements[, c("covar_1","covar_2", "covar_3")][i,] %in% c("min_dbh"))) {
      idx.min_dbh <- which(GROA_measurements[, c("covar_1","covar_2", "covar_3")][i,] %in% "min_dbh")
      min.dbh = GROA_measurements[, c("coV1_value","coV2_value", "coV3_value")][i,][idx.min_dbh]
     
    } else {
      min.dbh = "NAC"
    }
    
    if(any(GROA_measurements[, c("covar_1","covar_2", "covar_3")][i,] %in% c("max_depth"))) {
      idx.max_depth <- which(GROA_measurements[, c("covar_1","covar_2", "covar_3")][i,] %in% "max_depth")
      depth = GROA_measurements[, c("coV1_value","coV2_value", "coV3_value")][i,][idx.max_depth]
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
    
    if(include_record) ForC_data$MEASUREMENTS <-  bind_rows( #####
      ForC_data$MEASUREMENTS,
      data.frame( #####
        measurement.ID = c(max(ForC_data$MEASUREMENTS$measurement.ID)+1, max(ForC_data$MEASUREMENTS$measurement.ID) + 2)[c(TRUE, !is.na(GROA_measurements$density[i]))],
        sites.sitename,
        plot.name = refor.type.to.plot.name.key[GROA_measurements$refor.type[i],],
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
        coV_1.value = other.cov.values[1],
        covariate_2 = other.covariates[2],
        coV_1.value = other.cov.values[2],
     
        citation.ID = GROA_litterature$citation.ID[GROA_litterature$study.id %in% GROA_measurements$study.id[i]],
        source.notes = paste0("GROA measurement.ID #", GROA_measurements$measurement.id[i]),
        loaded.from = "[Cook-Patton database citation, in ForC format]",
        loaded.by = paste("R script by Valentine Herrmann"),
        checked.ori.pub = ifelse(grepl("Guo|Krankina", GROA_litterature$citation.ID[GROA_litterature$study.id %in% GROA_measurements$study.id[i]]), 0, 1), # "1" for all studies, except "0" for Guo and Krankina
        ForC.investigator = "Dr. Susan Cook-Patton",
        required.citations = "[Cook-Patton database citation, in ForC format]",
        flag.suspicious = 0,
        NEW_RECORD = TRUE
        )
    )
  }
}


# Create a new record in METHODOLOGY if one does not already exist

# Create a new record in HISTORY if one does not already exist

cat("All done.\n")




# save ####

sum(ForC_data$CITATIONS$NEW_RECORD)
sum(ForC_data$MEASUREMENTS$NEW_RECORD)
sum(ForC_data$METHODOLOGY$NEW_RECORD)
sum(ForC_data$SITES$NEW_RECORD)

write.csv(filter(ForC_data$SITES, NEW_RECORD==TRUE), "new_data/new_SITES.csv", row.names = F)
write.csv(filter(ForC_data$CITATIONS, NEW_RECORD==TRUE), "new_data/new_CITATIONS.csv", row.names = F)
write.csv(filter(ForC_data$MEASUREMENTS, NEW_RECORD==TRUE), "new_data/new_MEASUREMENTS.csv", row.names = F)
