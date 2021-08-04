# Code to set up the SEPH table data frames and save them as rds files
# July 30, 2021.

# Make sure the working directory is where the Table_specs.R file is

setwd("/Users/philipsmith/Documents/R/SEPHbrowser")
savespot <- "/Users/philipsmith/Documents/R/SEPHbrowser/"

pkgs <- c("cansim","tidyverse","stringr","gt","rlist")
inst <- lapply(pkgs,library,character.only=TRUE)

file_refresh <- TRUE

#(01)===========================================================================
table01_id <- "14-10-0201-01" # SEPH employment by ind and prov, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,Date,GEO,"KIN"="Type of employee",VALUE,
  "NAICS"="North American Industry Classification System (NAICS)")
q0$Date <- as.Date(q0$Date)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))
#(02)===========================================================================
table01_id <- "14-10-0203-01" # SEPH AWE by ind and prov, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,Date,GEO,"KIN"="Type of employees","OVR"="Overtime",VALUE,
  "NAICS"="North American Industry Classification System (NAICS)")
q0$Date <- as.Date(q0$Date)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))
#(03)===========================================================================
table01_id <- "14-10-0205-01" # SEPH AHE by ind and prov, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,Date,GEO,"OVR"="Overtime",VALUE,
  "NAICS"="North American Industry Classification System (NAICS)")
q0$Date <- as.Date(q0$Date)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))
#(04)===========================================================================
table01_id <- "14-10-0209-01" # SEPH AHE for salaried empl by ind and prov, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,Date,GEO,VALUE,
  "NAICS"="North American Industry Classification System (NAICS)")
q0$Date <- as.Date(q0$Date)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))
#(05)===========================================================================
table01_id <- "14-10-0211-01" # SEPH std workweek for salaried empl by ind and prov, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,Date,GEO,VALUE,
  "NAICS"="North American Industry Classification System (NAICS)")
q0$Date <- as.Date(q0$Date)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))
#(06)===========================================================================
table01_id <- "14-10-0213-01" # SEPH fixed-weight index of AHE, by ind, by prov
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,Date,GEO,VALUE,
  "NAICS"="North American Industry Classification System (NAICS)")
q0$Date <- as.Date(q0$Date)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))
#(07)===========================================================================
table01_id <- "14-10-0220-01" # SEPH empl and AWE by ind, SA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,Date,"EST"="Estimate",VALUE,
  "NAICS"="North American Industry Classification System (NAICS)")
q0$Date <- as.Date(q0$Date)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))
#(08)===========================================================================
table01_id <- "14-10-0221-01" # SEPH empl AHE, AWE, AWH by ind, SA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,Date,"TYP"="Type of employee","EST"="Estimate",VALUE,
  "NAICS"="North American Industry Classification System (NAICS)")
q0$Date <- as.Date(q0$Date)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))
#(09)===========================================================================
table01_id <- "14-10-0222-01" # SEPH empl AHE, AWE, AWH by prov, SA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,Date,GEO,"EST"="Estimate",VALUE)
q0$Date <- as.Date(q0$Date)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))
#(10)===========================================================================
table01_id <- "14-10-0223-01" # SEPH empl and AWE, by prov, SA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,Date,GEO,"EST"="Estimate",VALUE,
  "NAICS"="North American Industry Classification System (NAICS)")
q0$Date <- as.Date(q0$Date)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))
#(11)===========================================================================
table01_id <- "14-10-0255-01" # SEPH AWH, by prov, NSA
table01 <- get_cansim(table01_id,refresh=file_refresh)
q0 <- select(table01,Date,GEO,"OVT"="Overtime",VALUE,
  "NAICS"="North American Industry Classification System (NAICS)")
q0$Date <- as.Date(q0$Date)
saveRDS(q0,paste0(savespot,"rds/",table01_id,".rds"))


