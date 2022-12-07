df_scanned_ladybug <- read.csv("Data/Unclean/ScanLadybugData.csv", na.strings = na_placeholders)
df_scanned_ladybug_species <- read_xlsx("Data/Unclean/Ladybug Data.xlsx", .name_repair = "universal", na = na_placeholders) 

rm(na_placeholders)

# Renaming all columns to camelCase for consistency


df_ladybug <- df_scanned_ladybug

df_ladybug_species <- df_scanned_ladybug_species %>%
  rename(catalogNumber = SCAN.CODE) %>%
  rename(plotType = plot)

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

write_csv(df_clean_ladybug,"Data/Clean/Ladybug.csv")
write_csv(df_clean_ladybug_species, "Data/Clean/LadybugSpecies.csv")