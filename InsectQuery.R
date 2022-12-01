library(tidyverse)
library(readxl)

rm(list = ls())

df_scanned_ladybug <- read.csv("Data/ScanLadybugData.csv")
df_scanned_ladybug_species <- read_xlsx("Data/Ladybug Data.xlsx", .name_repair = "universal")
df_scanned_wings <- read_xlsx("Data/Cleaned Data LWA .xlsx", .name_repair = "universal")
df_scanned_butterfly <- read_xlsx("Data/CompletePierisData_2022-03-09.xlsx", .name_repair = "universal")

#renaming all columns to camelCase for consistency

df_butterfly <- df_scanned_butterfly %>%
  rename_with(~ sub("(?:dwc.:?)?(.)", "\\L\\1", .x, perl = TRUE)) %>%
  rename(coreId = coreid) %>%
  rename(lengthLW = lWingLength) %>%
  rename(widthLW = lWingWidth) %>%
  rename(apexLW = lBlackPatchApex) %>%
  rename(anteriorSpotLW = lAnteriorSpotM3) %>%
  rename(posteriorSpotLW = lPosteriorSpotCu2) %>%
  rename(interspotLW = lInterspotPA) %>%
  rename(lengthRW = rWingLength) %>%
  rename(widthRW = rWingWidth) %>%
  rename(apexRW = rBlackPatchApex) %>%
  rename(anteriorSpotRW = rAnteriorSpotM3) %>%
  rename(posteriorSpotRW = rPosteriorSpotCu2) %>%
  rename(interspotRW = rInterspotPA)
  

df_wings <- df_scanned_wings %>%
  rename(coreId = core.ID) %>%
  rename(lengthLW = LW.length) %>%
  rename(widthLW = LW.width) %>%
  rename(apexLW = LW.apex.A) %>%
  rename(lengthRW = RW.length) %>%
  rename(widthRW = RW.width) %>%
  rename(apexRW = RW.apex.A) %>%
  drop_na() #shouldn't have sparse entries

df_ladybug <- df_scanned_ladybug

df_ladybug_species <- df_scanned_ladybug_species %>%
  rename(catalogNumber = SCAN.CODE)

df_ladybug = df_ladybug %>%
  mutate(dateScanned = as.Date(eventDate, "%m/%d/%Y"))

#Selecting & Pivoting Columns from df_ladybug that we will use for our analysis 
df_pivoted_ladybug <- df_ladybug %>%
  select(catalogNumber, genus, specificEpithet, dateScanned, country, stateProvince, county) %>%
  group_by(catalogNumber)

#Selecting & Pivoting Columns from df_ladybug_species that we will use for our analysis
df_pivoted_ladybug_species <- df_ladybug_species %>%
  select(catalogNumber, Species, coordinates) %>%
  group_by(catalogNumber)

#Joining the df_ladybug & df_ladybug_species pivot tables 
df_joined_ladybug_pivot_tables <- df_pivoted_ladybug %>%
  left_join(df_pivoted_ladybug_species, by = c("catalogNumber")) 
  
