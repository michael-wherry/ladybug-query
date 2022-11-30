library(tidyverse)
library(readxl)

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
