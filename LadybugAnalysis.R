library(tidyverse)
library(patchwork)
library(party)
library(magrittr)
rm(list = ls())

df_clean_ladybug_analysis <- read.csv("Data/Clean/Ladybug.csv",) %>%
  mutate(eventDate = as.Date(eventDate))
df_clean_ladybug_species_analysis <- read.csv("Data/Clean/LadybugSpecies.csv")

# Removing all possibilities of scientific notation for ticks
options(scipen = 999)

#Joining csv files
df_joined_ladybug_dataframes <- df_clean_ladybug_analysis %>%
  left_join(df_clean_ladybug_species_analysis, by =c("catalogNumber")) %>%
  mutate(plotType = ifelse(is.na(plotType), "Unknown", plotType))

# start of creating a stacked bar chart to display the proportion
# of each species that were found in the surveyed environments
df_species_plotTypes <- df_joined_ladybug_dataframes %>%
  select(commonName, plotType) %>%
  group_by(commonName, plotType) %>%
  summarise(speciesCount = length(commonName)) %>%
  #Renaming all plotTypes to what environment they are measuring
  mutate(environment = if_else(plotType == "LP-AG", "Agricultural","Unknown")) %>%
  mutate(environment = if_else(plotType == "LP-GA", "Garden",environment)) %>%
  mutate(environment = if_else(plotType == "LP-GF", "Forested",environment)) %>%
  mutate(environment = if_else(plotType == "LP-GM", "Mowed Grass",environment)) %>%
  mutate(environment = if_else(plotType == "LP-GU", "Unmowed Grass",environment)) %>%
  mutate(environment = if_else(plotType == "LP-IC", "Industrial",environment)) %>%
  mutate(environment = if_else(plotType == "LP-PR", "Prairie",environment)) %>%
  filter(plotType != "Unknown")

# bar is created with the help of
# geom_col() and ggplot() function
plotType_by_species_ggp <- (ggplot(df_species_plotTypes, aes(commonName, speciesCount, fill = environment)) +   
  geom_col(stat = "identity") +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  theme_dark() +
  coord_flip() +
  labs(title = "Environments Per Species", x = "Ladybug Species", y = "Count of Species")) %T>%
  plot()

# start of boxplot which will show in what months the largest 
# proportion of a species population was found
df_species_months <- df_joined_ladybug_dataframes %>%
  select(eventDate,commonName) %>%
  group_by(commonName, eventDate) %>%
  summarise(speciesCount = length(commonName))

# boxplot is created with the help of 
# geom_boxplot() and ggplot()
species_months_ggp <- (ggplot(df_species_months, aes(commonName, lubridate::yday(x = eventDate), fill = commonName)) +   
  geom_boxplot(stat = "boxplot", position = "dodge2", show.legend = FALSE) +
  theme_dark() + 
  scale_y_continuous(limits = c(0, 365),
                     breaks = c(31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365), 
                     labels = c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  coord_flip() +
  labs(title = "Observations Over Time", x = "Ladybug Species", y = "Observation Date")) %T>%
  plot()

# start of line charts that will show how each environment was
# performing during the course of its use by using ggplot()
# geom_line()

df_month_placeholder <- as.data.frame(c(1:12)) %>%
  cbind(0)

colnames(df_month_placeholder) <- c('month', 'countMonth')

df_plotType_months <- as.data.frame(cbind(c("LP-AG", "LP-GA", "LP-GF","LP-GM", "LP-GU", "LP-IC", "LP-PR", "Unknown"))) %>%
  slice(rep(1:n(), each = 12)) %>%
  cbind(c(1:96)) %>%
  rename(month = "c(1:96)") %>% 
  rename(plotType = "V1") %>%
  mutate(month = 1 + as.integer(month) %% 12) %>%
  arrange(plotType, month)

df_month_activity <- df_joined_ladybug_dataframes %>%
  select(plotType, eventDate) %>%
  mutate(month = lubridate::month(eventDate)) %>%
  group_by(plotType, month) %>%
  summarise(countMonth = length(eventDate))

df_plotType_months_activity <- df_plotType_months %>%
  left_join(df_month_activity, by = c("plotType", "month")) %>%
  mutate(countMonth = replace_na(countMonth, 0))

plotType_activity_line <- (ggplot(df_plotType_months_activity, aes(x = month, y = countMonth, color = plotType))  +
  geom_line(show.legend = FALSE) +
  scale_x_continuous(limits = c(0, 12),
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                     labels = c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_dark() +
  labs(title = "Plot Recordings Over Time", x = "Observation Dates", y = "Ladybug Recordings")) 

plotType_activity_area <- (ggplot(df_plotType_months_activity, aes(x = month, y = countMonth, color = plotType))  +
                            geom_area(aes(fill = plotType)) +
                            scale_x_continuous(limits = c(0, 12),
                                               breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                                               labels = c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
                            theme_dark() +
                            labs(y = "Ladybug Recordings", x = "Observation Dates", legend = "Plot Type"))

plot(plotType_activity_line / plotType_activity_area) 


# t test to see if the plotType had an any discernible impact
# on how many ladybugs they were able to find during their studies
df_count_plotType <- df_joined_ladybug_dataframes %>%
  select(plotType) %>%
  group_by(plotType) %>%
  summarise(countPlotType = length(plotType)) %>%
  filter(plotType != "Unknown")

t.test(df_count_plotType$countPlotType)


