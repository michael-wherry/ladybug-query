library(tidyverse)
library(readxl)

rm(list = ls())

na_placeholders <- c("", "//s+","N/A", "n/a","N/a", "n/A", "NA", "UNKNOWN", "unknown")
df_scanned_ladybug <- read.csv("Data/Unclean/ScanLadybugData.csv", na.strings = na_placeholders)
df_scanned_ladybug_species <- read_xlsx("Data/Unclean/Ladybug Data.xlsx", .name_repair = "universal", na = na_placeholders) 
df_scanned_wings <- read_xlsx("Data/Unclean/Cleaned Data LWA .xlsx", .name_repair = "universal", na = na_placeholders)
df_scanned_butterfly <- read_xlsx("Data/Unclean/CompletePierisData_2022-03-09.xlsx", .name_repair = "universal", na = na_placeholders)
rm(na_placeholders)

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
  drop_na() # Shouldn't have sparse entries

df_ladybug <- df_scanned_ladybug

df_ladybug_species <- df_scanned_ladybug_species %>%
  rename(catalogNumber = SCAN.CODE) %>%
  rename(plotType = plot)

rm(df_scanned_butterfly)
rm(df_scanned_wings)
rm(df_scanned_ladybug)
rm(df_scanned_ladybug_species)

# Ladybugs

df_clean_ladybug <- df_ladybug %>%
  select(catalogNumber, genus, specificEpithet, eventDate, country, stateProvince, county) %>%
  mutate(eventDate = as.Date(eventDate, format = "%m/%d/%Y")) %>%
  filter(startsWith(tolower(country), "u")) %>%
  mutate(country = "United States") %>%
  mutate(county = if_else(county == "Rcok Island", "Rock Island", county)) %>%
  filter(county != "") %>%
  mutate(species = paste(genus, specificEpithet, sep = " ")) %>%
  mutate(species = ifelse(species == "NA NA", "Unknown", species)) %>%
  mutate(species = str_to_sentence(paste(genus, specificEpithet, sep = " "))) %>%
  mutate(species = ifelse(species == "NA NA", NA_character_, species)) %>%
  mutate(species = as.factor(species)) %>%
  mutate(commonName = ifelse(species == "Adalia bipunctata", "Two-spot Ladybird", "Unknown")) %>%
  mutate(commonName = ifelse(species == "Anatis labiculata", "Fifteen-spotted Lady beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Anatis mali", "Eye spotted lady beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Brachiacantha ursina", "Ursine Spurleg Lady Beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Chilocorus stigma", "Twice stabbed ladybug", commonName)) %>%
  mutate(commonName = ifelse(species == "Coccinella septempunctata", "Seven-Spot ladybird", commonName)) %>%
  mutate(commonName = ifelse(species == "Coccinella transversoguttata", "Transverse Ladybird", commonName)) %>%
  mutate(commonName = ifelse(species == "Coleomegilla maculata", "Spotted lady beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Cycloneda munda", "Polished lady beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Cycloneda sanguinea", "Spotless ladybird", commonName)) %>%
  mutate(commonName = ifelse(species == "Epargyreus clarus", "Silver spotted skipper", commonName)) %>%
  mutate(commonName = ifelse(species == "Epilachna varivestis", "Mexican bean beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Harmonia axyridis", "Asain lady beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Hippodamia caseyi", "Caseys lady beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Hippodamia convergens", "Convergent lady beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Hippodamia parenthesis", "Parentheses lady beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Hippodamia tredecimpunctata", "Thirteen-spotted lady beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Hippodamia variegata", "Adonis ladybird", commonName)) %>%
  mutate(commonName = ifelse(species == "Hyperaspis signata", "Red-Marked ladybug", commonName)) %>%
  mutate(commonName = ifelse(species == "Hyperaspis undulata", "Undulate lady beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Olla v-nigrum", "Ashy-Gray lady beetle", commonName)) %>%
  mutate(commonName = ifelse(species == "Propylea quatuordecimpunctata", "Fourteen-spotted ladybug", commonName)) %>%
  mutate(commonName = ifelse(species == "Psyllobora vigintimaculata", "Twenty-spotted lady beetle", commonName))

df_clean_ladybug_species <- df_ladybug_species %>%
  select(catalogNumber, Species, coordinates, plotType) %>%
  mutate(plotType = substr(plotType, 0, regexpr("-", plotType)+2)) %>%
  mutate(ifelse(plotType == "Lp-PR", "LP-PR", plotType))

# Butterflies

df_butter_location <- df_butterfly %>%
  select(coreId, decimalLatitude, decimalLatitudeUpdated, decimalLongitude, decimalLongitudeUpdated) %>%
  mutate(latitude = coalesce(decimalLatitude, as.numeric(decimalLatitudeUpdated))) %>%
  mutate(longitude = coalesce(decimalLongitude, as.numeric(decimalLongitudeUpdated))) %>%
  select(coreId, latitude, longitude)

df_butter_date <- df_butterfly %>%
  select(coreId, year, yearUpdated, month, day, dayOfYearUpdated, startDayofYearUpdated, endDayofYearUpdated) %>%
  mutate(avgDayOfYear = floor(rowMeans(cbind(startDayofYearUpdated, endDayofYearUpdated)))) %>%
  mutate(dayOfYear = coalesce(dayOfYearUpdated, avgDayOfYear)) %>%
  mutate(mergedYear = coalesce(gsub("![0-9]", "", year), as.character(yearUpdated))) %>%
  mutate(monthFromDayOfYear = lubridate::month(as.Date(dayOfYear, origin = paste0(mergedYear, "-01-01")))) %>%
  mutate(dayFromDayOfYear = lubridate::day(as.Date(dayOfYear, origin = paste0(mergedYear, "-01-01")))) %>%
  mutate(mergedMonth = coalesce(monthFromDayOfYear, month)) %>%
  mutate(mergedDay = coalesce(as.character(dayFromDayOfYear), day)) %>%
  mutate(date =  as.Date(paste(mergedYear, mergedMonth, mergedDay, sep = "/")))

df_butter_date <- df_butter_date %>%
  select(coreId, date, mergedYear) %>%
  rename(year = mergedYear) # Still missing a small handful of dates

df_butter_stats <- df_butterfly %>%
  select(coreId, sexUpdated,
         lengthLW, widthLW, apexLW, posteriorSpotLW, anteriorSpotLW, interspotLW,
         lengthRW, widthRW, apexRW, posteriorSpotRW, anteriorSpotRW, interspotRW) %>%
  mutate(across(!c(coreId, sexUpdated, interspotLW, interspotRW), ~as.numeric(.x))) %>%
  left_join(df_wings) %>%
  mutate(sex = coalesce(sex, sexUpdated)) %>%
  mutate(sex = gsub("(?=f|F)[^\\?]*", "female", sex, perl = TRUE)) %>%
  mutate(sex = gsub("(?=m|M)[^\\?]*", "male", sex, perl = TRUE)) %>% 
  select(!sexUpdated)

df_clean_butter <- df_butter_date %>%
  left_join(df_butter_stats, by = "coreId") %>%
  left_join(df_butter_location, by = "coreId")



write_csv(df_clean_ladybug,"Data/Clean/Ladybug.csv")
write_csv(df_clean_ladybug_species, "Data/Clean/LadybugSpecies.csv")
write_csv(df_clean_butter, "Data/Clean/Butterfly.csv")

# Test cases
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