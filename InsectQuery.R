library(tidyverse)
library(readxl)

df_scanned_ladybug <- read.csv("Data/ScanLadybugData.csv")
df_ladybug_species <- read_xlsx("Data/Ladybug Data.xlsx", .name_repair = "universal")
df_scanned_wings <- read_xlsx("Data/Cleaned Data LWA .xlsx", .name_repair = "universal")
df_scanned_butterfly <- read_xlsx("Data/CompletePierisData_2022-03-09.xlsx", .name_repair = "universal")

df_scanned_butterfly <- rename_with(df_scanned_butterfly, ~ sub("dwc.(.)", "\\U\\1", .x, perl = TRUE))
df_scanned_wings <- df_scanned_wings %>% drop_na() 

#addition