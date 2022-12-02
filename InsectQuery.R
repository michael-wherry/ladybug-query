library(tidyverse)
library(readxl)
library(usmap)
library(magrittr)

rm(list = ls())

df_scanned_ladybug <- read.csv("Data/ScanLadybugData.csv",na.strings = c("", "//s+","N/A", "n/a","N/a", "n/A", "NA", "UNKNOWN"))
df_scanned_ladybug_species <- read_xlsx("Data/Ladybug Data.xlsx", .name_repair = "universal", na = c("", "//s+","N/A", "n/a","N/a", "n/A","NA","UNKNOWN")) 
df_scanned_wings <- read_xlsx("Data/Cleaned Data LWA .xlsx", .name_repair = "universal", na = c("", "//s+","N/A", "n/a","N/a", "n/A", "NA","UNKNOWN"))
df_scanned_butterfly <- read_xlsx("Data/CompletePierisData_2022-03-09.xlsx", .name_repair = "universal", na = c("", "//s+","N/A", "n/a","N/a", "n/A","NA","UNKNOWN"))

# Renaming all columns to camelCase for consistency

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


#Selecting Columns from df_ladybug that we will use for our analysis 
str_to_title(df_ladybug$genus)
str_to_title(df_ladybug$specificEpithet)
df_clean_ladybug <- df_ladybug %>%
  select(catalogNumber, genus, specificEpithet, dateScanned, country, stateProvince, county) %>%
  filter(startsWith(tolower(country), "u")) %>%
  mutate(country = "United States") %>%
  mutate(county = if_else(county == "Rcok Island", "Rock Island", county)) %>%
  filter(county != "") %>%
  mutate(species = paste(genus, specificEpithet, sep = " ")) %>%
  mutate(species = ifelse(species == "NA NA", NA_character_, species)) %>%
  mutate(species = as.factor(species))
  
 #Selecting & Pivoting Columns from df_ladybug_species that we will use for our analysis
df_clean_ladybug_species <- df_ladybug_species %>%
  select(catalogNumber, Species, coordinates) %>%
  mutate(longitude = substr(coordinates, 0, regexpr("-", coordinates)-1)) %>%
  mutate(latitude = substr(coordinates, regexpr("-", coordinates)+1, length(coordinates)))
  
df_butter_location <- df_butterfly %>%
  select(coreId, decimalLatitude, decimalLatitudeUpdated, decimalLongitude, decimalLongitudeUpdated) %>%
  mutate(latitude = coalesce(decimalLatitude, as.numeric(decimalLatitudeUpdated))) %>%
  mutate(longitude = coalesce(decimalLongitude, as.numeric(decimalLongitudeUpdated))) %>%
  select(coreId, latitude, longitude)

df_butter_date <- df_butterfly %>%
  select(coreId, year, yearUpdated, month, day, dayOfYearUpdated, startDayofYearUpdated, endDayofYearUpdated) %>%
  mutate(avgDayOfYear = floor(rowMeans(cbind(startDayofYearUpdated, endDayofYearUpdated)))) %>%
  mutate(dayOfYear = coalesce(dayOfYearUpdated, avgDayOfYear)) %>%
  mutate(mergedYear = coalesce(as.character(yearUpdated), year)) %>%
  mutate(monthFromDayOfYear = lubridate::month(as.Date(dayOfYear, origin = paste0(mergedYear, "-01-01")))) %>%
  mutate(dayFromDayOfYear = lubridate::day(as.Date(dayOfYear, origin = paste0(mergedYear, "-01-01")))) %>%
  mutate(mergedMonth = coalesce(monthFromDayOfYear, month)) %>%
  mutate(mergedDay = coalesce(as.character(dayFromDayOfYear), day)) %>%
  mutate(date =  as.Date(paste(mergedYear, mergedMonth, mergedDay, sep = "/")))
  
df_butter_date <- df_butter_date %>%
  select(coreId, date) # still missing a small handful of dates

# Test cases ; ctrl+shift+c to toggle comments
# 
# (!any(duplicated(df_butterfly$coreId))) %>%
#   paste0("coreId is distinct in df_butterfly: ", .)
# 
# (!any(duplicated(df_wings$coreId))) %>%
#   paste0("coreId is distinct in df_wings: ", .)
# 
# (nrow(full_join(df_butterfly, df_wings, "coreId")) == nrow(df_butterfly)) %>%
#   paste0("All observations in df_wings occur in df_butterfly: ", .)
# 
# (!any(duplicated(df_ladybug$catalogNumber))) %>%
#   paste0("catalogNumber is distinct in df_ladybug: " , .)
# 
# (!any(duplicated(df_ladybug_species$catalogNumber))) %>%
#   paste0("catalogNumber is distinct in df_ladybug_species: " , .)
# 
# (nrow(full_join(df_ladybug, df_ladybug_species, "catalogNumber")) == nrow(df_ladybug)) %>%
#   paste0("All observations in df_ladybug_species occur in df_ladybug: ", .)

write.csv(df_clean_ladybug,"Data/CleanLadyBugData.csv")
write.csv(df_clean_ladybug_species, "Data/CleanLadyBugSpeciesData.csv")
