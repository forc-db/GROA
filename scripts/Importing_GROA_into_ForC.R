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

GROA_measurements <-  read.csv("data/nonsoil_litter_CWD.csv", stringsAsFactors = F)
GROA_measurements_soil <-  read.csv("data/soil.csv", stringsAsFactors = F)
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
  ForC_data[[name]]$OLD_RECORD = FALSE
} # for(f in forC_files)

## variable_name_conversion table ####
variable_name_conversion <- read.csv("GROA-ForC mapping/variable_name_conversion.csv", stringsAsFactors = F)

# Settings ####
## set refor.type to plot.name key
refor.type.to.plot.name.key <-  data.frame(plot.name = c("regrowth stand regenerating via spontaneous natural regeneration","regrowth stand regenerating via assisted natural regeneration","regrowth stand regenerating via initial tree planting", "diverse species plantation", "monoculture plantation", "intensive tree monocrop", "multistrata stand", "stand with tree intercropping", "silvopastoral system", "transitional ecosystem", "cropland", "pasture", "intact/ old growth stand", "fire", "harvest", "shifting cultivation"), row.names = c("SNR", "ANR", "ITP", "DP", "MP", "TMC", "MS", "TI", "SP", "TR", "C", "PA", "OG", "F", "H", "SC"), stringsAsFactors = F)

# pre-fixes and preparations ####

## merge non soil and soil tables
names(GROA_measurements)[!names(GROA_measurements) %in% names(GROA_measurements_soil)]

GROA_measurements_soil$sites.sitename <- GROA_sites$site.sitename[match(GROA_measurements_soil$site.id, GROA_sites$site.id)]
GROA_measurements_soil$country <- GROA_sites$site.country[match(GROA_measurements_soil$site.id, GROA_sites$site.id)]
GROA_measurements_soil$citations.title <- GROA_litterature$study.id[match(GROA_measurements_soil$study.id, GROA_litterature$study.id)]



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


## Create plot.name ####
GROA_measurements$plot.name <-  refor.type.to.plot.name.key[GROA_measurements$refor.type,]
GROA_measurements$plot.name <- ifelse(grepl("regrowth", GROA_measurements$plot.name) & !is.na(GROA_measurements$date) & !is.na(GROA_measurements$stand.age), paste(GROA_measurements$plot.name, "established around", GROA_measurements$date[i] - GROA_measurements$stand.age[i]),GROA_measurements$plot.name )



# Create a new record in SITES if one does not already exist ####
for( i in 1:nrow(GROA_sites)) {
  
  cat("--------------------", i, "/", nrow(GROA_sites), "\n")
  cat("Record:", paste(GROA_sites[i, 1:4], collapse = " "), "\n")
  
  sites.sitename <- GROA_sites$site.sitename[i]
  if(sites.sitename %in% ForC_data$SITES$sites.sitename) {
    cat("Found site", sites.sitename, "\n")
    
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
        site.ID = max(ForC_data$SITES$site.ID) + 1,
        sites.sitename = sites.sitename,
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
        measurement.ref = GROA_sites$measurement.ref[i],
        ref.notes = paste("GROA site.ID =", GROA_sites$site.id[i]),
        lacks.info.from.ori.pub = 0,
        loaded.from = "[Cook-Patton database citation, in ForC format]",
        loaded.by = "R script by Valentine Herrmann",
        NEW_RECORD = FALSE,
        OLD_RECORD = TRUE
        # tropical.extratropical = 
      )
    )
   
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
        site.ID = max(ForC_data$SITES$site.ID) + 1,
        sites.sitename = sites.sitename,
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
        ref.notes = paste("GROA site.ID =", GROA_sites$site.id[i]),
        lacks.info.from.ori.pub = 0,
        loaded.from = "[Cook-Patton database citation, in ForC format]",
        loaded.by = "R script by Valentine Herrmann",
        NEW_RECORD = TRUE,
        OLD_RECORD = FALSE
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

## from GROA_measurements --> non need to do it because all(GROA_measurements$citations.title %in% GROA_litterature$citations.title) is TRUE.

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
    
    # deal with covariates separately here ####
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

    # append to measurements table now ####
    if(include_record) ForC_data$MEASUREMENTS <-  bind_rows( #####
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


# Create a new record in HISTORY if one does not already exist ####
site_plots <- unique(paste(ForC_data$MEASUREMENTS[ForC_data$MEASUREMENTS$NEW_RECORD == 1,]$sites.sitename, ForC_data$MEASUREMENTS[ForC_data$MEASUREMENTS$NEW_RECORD == 1,]$plot.name))

s_p.with.age.date.discrepancy <- NULL
s_p.with.chronosequence.to.fix <- NULL
s_p.with.other.date.age.issues.to.fix <- NULL

for(s_p in site_plots) {
  cat(paste("creating HISTORY for", s_p, "\n"))
  
  groa_sub <- GROA_measurements[paste(GROA_measurements$sites.sitename, GROA_measurements$plot.name) %in% s_p, ]
  sites.sitename <- unique(groa_sub$sites.sitename)
  plot.name <- unique(groa_sub$plot.name)
  plot.area <- ifelse(is.na(unique(groa_sub$n * groa_sub$plot.size)), "NAC", unique(groa_sub$n * groa_sub$plot.size))
  refor.type <- unique(groa_sub$refor.type)

  if(s_p %in% paste(forc_data$HISTORY$sites.sitename, forc_data$HISTORY$plot.name)) {
    cat("Found site", sites.sitename, "\n")
  } else {
    
    if(length(refor.type) > 1) stop("there is more than 1 refor.type for this site")
    
    if(length(refor.type) == 1) {
      
    
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
        
        event.sequence <- 1:3
        hist.cat <- c("Disturbance", "Establishment", "Regrowth")
        hist.type <- c(ifelse(refor.type %in% "C", 'Cultivation', 
                              ifelse(refor.type %in% "SC", "Shifting cultivation",
                                     ifelse(refor.type %in% "H", "Harvest",
                                            ifelse(refor.type %in% "F", "Burned",
                                                   ifelse(refor.type %in% "D", "NAC",
                                                          ifelse(refor.type %in% "PA", "Grazed",
                                                                 ifelse(refor.type %in% "OG", "No severe disturbance", NA))))))))
      } # if(refor.type %in% "SNR"
      
      if(refor.type %in% "OG") {
        
      } # if(refor.type %in% "OG")
      
      hist.cat = c("Establishement", "Regrowth", manip_conversion[, c("Hist.cat", "Hist.cat2", "Hist.cat3")])
      hist.type = c("Establishment of oldest trees", "Initiation of post-disturbance cohort (planted or natural)", manip_conversion[, c("Hist.type", "Hist.type2", "Hist.type3")])
      
      event.sequence = seq(hist.cat)
      
      date = unique(ifelse(!is.na(Study_midyear - Age_disturbance), floor(Study_midyear - Age_disturbance),
                           ifelse(!is.na(Study_midyear - Age_ecosystem), floor((Study_midyear - Age_ecosystem)), "NAC")))
      
      date = c(rep(date, 2), rep("NAC", length(hist.cat) - 2))
      
      date.loc = "NAC"
      
      est.regrowth.assumed.same.year = c(1, 1, rep(NA, length(hist.cat) - 2))
      
      level = c(NA, NA, rep("NAC", length(hist.cat) - 2))
      units = NA
      
      percent.mortality = c(NA, NA, manip_conversion[, c("percent.mortality", "percent.mortality2", "percent.mortality3")])
      hist.notes = c(NA, NA, rep(unique(paste(groa_sub$Manipulation, groa_sub$Manipulation_level)), length(hist.cat) -2))
  

      rows.to.add <- data.frame( #####
                                 history.ID = ceiling(max(forc_data$HISTORY$history.ID)) + seq(event.sequence) / 100,
                                 sites.sitename,
                                 plot.name,
                                 plot.area,
                                 event.sequence = as.character(event.sequence),
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
                                 stringsAsFactors = F
      )
      
      rows.to.add <- rows.to.add[!is.na(rows.to.add$hist.cat),]
      
      
      forc_data$HISTORY <-  bind_rows(forc_data$HISTORY, rows.to.add)
    } # if(length(manip) == 1)
  } #  if(sites.sitename %in% forc_data$HISTORY$sites.sitename)
  
} # for(s_p in site_plots) {






cat("All done.\n")




# save ####

sum(ForC_data$CITATIONS$NEW_RECORD)
sum(ForC_data$MEASUREMENTS$NEW_RECORD)
sum(ForC_data$SITES$NEW_RECORD)
sum(ForC_data$SITES$OLD_RECORD)

write.csv(filter(ForC_data$SITES, NEW_RECORD==TRUE), "new_data/new_SITES.csv", row.names = F)
write.csv(filter(ForC_data$CITATIONS, NEW_RECORD==TRUE), "new_data/new_CITATIONS.csv", row.names = F)
write.csv(filter(ForC_data$MEASUREMENTS, NEW_RECORD==TRUE), "new_data/new_MEASUREMENTS.csv", row.names = F)

old_sites <-  data.frame(FROM = "GROA", filter(ForC_data$SITES, OLD_RECORD==TRUE))

old_SITES_from_ForC <- data.frame(FROM = "ForC", ForC_data$SITES[ForC_data$SITES$sites.sitename %in% old_sites$sites.sitename & ForC_data$SITES$OLD_RECORD %in% F,])

old_sites <- rbind(old_SITES_from_ForC, old_sites)
old_sites <- old_sites[order(old_sites$sites.sitename),]
old_sites
write.csv(old_sites, "new_data/old_SITES.csv", row.names = F)

