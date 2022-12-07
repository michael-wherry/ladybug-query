library(tidyverse)
library(shiny)
library(party)
library(magrittr)
rm(list = ls())

df_clean_ladybug_analysis <- read.csv("Data/CleanLadyBugData.csv",) %>%
  mutate(eventDate = as.Date(eventDate))
df_clean_ladybug_species_analysis <- read.csv("Data/CleanLadyBugSpeciesData.csv")

options(scipen = 999)

df_joined_ladybug_dataframes <- df_clean_ladybug_analysis %>%
  left_join(df_clean_ladybug_species_analysis, by =c("catalogNumber")) %>%
  mutate(plotType = ifelse(is.na(plotType), "Unknown", plotType))

df_plotType_by_species <- df_joined_ladybug_dataframes %>%
  select(commonName, plotType) %>%
  group_by(commonName, plotType) %>%
  summarise(count = length(commonName)) %>%
  mutate(environment = ifelse(plotType == "LP-AG", "Agricultural","Unknown")) %>%
  mutate(environment = ifelse(plotType == "LP-GA", "Garden",environment)) %>%
  mutate(environment = ifelse(plotType == "LP-GF", "Forested",environment)) %>%
  mutate(environment = ifelse(plotType == "LP-GM", "Mowed Grass",environment)) %>%
  mutate(environment = ifelse(plotType == "LP-GU", "Unmowed Grass",environment)) %>%
  mutate(environment = ifelse(plotType == "LP-IC", "Industrial",environment)) %>%
  mutate(environment = ifelse(plotType == "LP-PR", "Prairie",environment)) %>%
  filter(plotType != "Unknown")

# bar is created with the help of
# grom_bar() and ggplot() function
plotType_by_species_ggp <- (ggplot(df_plotType_by_species, aes(commonName, count, fill = environment)) +   
  geom_col(stat = "identity", aes(y=log(count))) +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  theme_dark() +
  coord_flip() +
  labs(title = "Environments Per Species", x = "Ladybug Species", y = "Count of Species")) %T>%
  plot()

df_species_by_month <- df_joined_ladybug_dataframes %>%
  select(eventDate,commonName) %>%
  group_by(commonName, eventDate) %>%
  summarise(count = length(commonName))

species_by_month_ggp <- (ggplot(df_species_by_month, aes(commonName, lubridate::yday(x = eventDate))) +   
  geom_boxplot(stat = "boxplot", position = "dodge2") +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  theme_dark() +
  scale_y_continuous(limits = c(0, 365),
                     breaks = c(31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365), 
                     labels = c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  coord_flip() +
  labs(title = "Observations Over Time", x = "Ladybug Species", y = "Observation Date")) %T>%
  plot()

df_plots_by_month <- df_joined_ladybug_dataframes %>%
  select(eventDate,commonName) %>%
  mutate(eventDate = substr(eventDate, 0, regexpr("-", eventDate)-1)) %>%
  mutate(eventDate = substr(eventDate, regexpr("-", eventDate)-4, length(eventDate))) %>%
  #mutate(eventDate = as.POSIXlt.character(eventDate)) %>%
  group_by(eventDate, commonName) %>%
  summarise(count = length(commonName)) 
