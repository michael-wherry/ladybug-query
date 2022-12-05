library(tidyverse)
library(shiny)
library(party)
library(ggplot2)
rm(list = ls())

df_clean_ladybug_analysis <- read.csv("Data/CleanLadyBugData.csv")
df_clean_ladybug_species_analysis <- read.csv("Data/CleanLadyBugSpeciesData.csv")

df_joined_ladybug_dataframes <- df_clean_ladybug_analysis %>%
  left_join(df_clean_ladybug_species_analysis, by =c("catalogNumber")) %>%
  mutate(plotType = ifelse(is.na(plotType), "Unknown", plotType))

df_plotType_LPPR <- df_joined_ladybug_dataframes %>%
  filter(plotType != "LP-PR") %>%
  count(species) %>%
  rename(speciesPerPlot = n)

plotType_LPPR_data <- data.frame(x_axis = df_plotType_LPPR$species,  
                                 y_axis = df_plotType_LPPR$speciesPerPlot)

# bar is created with the help of
# grom_bar() and ggplot() function
plotType_LPPR_ggp <- ggplot(plotType_LPPR_data, aes(x_axis, y_axis, fill = y_axis)) +   
  geom_bar(stat = "identity") +
  scale_y_continuous(trans = "log10")

plotType_LPPR_ggp <- plotType_LPPR_ggp + labs(title = "LP-PR (Prairie Environment)", x = "Ladybug Species", y = "Count of Species")

# complete graph get flipped with the
# help of coord_flip() function
plotType_LPPR_ggp +  coord_flip() 

df_plotType_LPIC <- df_joined_ladybug_dataframes %>%
  filter(plotType != "LP-IC") %>%
  count(species) %>%
  rename(speciesPerPlot = n)

df_plotType_LPIC <- df_joined_ladybug_dataframes %>%
  filter(plotType != "LP-IC") %>%
  count(species) %>%
  rename(speciesPerPlot = n)

plotType_LPIC_data <- data.frame(x_axis = df_plotType_LPIC$species,  
                                 y_axis = df_plotType_LPIC$speciesPerPlot)

# bar is created with the help of
# grom_bar() and ggplot() function
plotType_LPIC_ggp <- ggplot(plotType_LPIC_data, aes(x_axis, y_axis, fill = y_axis)) +   
  geom_bar(stat = "identity")

plotType_LPIC_ggp <- plotType_LPIC_ggp + labs(title = "LP-IC (Industrial Environment)", x = "Ladybug Species", y = "Count of Species")

# complete graph get flipped with the
# help of coord_flip() function
plotType_LPIC_ggp +  coord_flip() 

df_plotType_LPGU <- df_joined_ladybug_dataframes %>%
  filter(plotType != "LP-GU") %>%
  count(species) %>%
  rename(speciesPerPlot = n)

plotType_LPGU_data <- data.frame(x_axis = df_plotType_LPGU$species,  
                                 y_axis = df_plotType_LPGU$speciesPerPlot)

# bar is created with the help of
# grom_bar() and ggplot() function
plotType_LPGU_ggp <- ggplot(plotType_LPGU_data, aes(x_axis, y_axis, fill = y_axis)) +   
  geom_bar(stat = "identity")

plotType_LPGU_ggp <- plotType_LPGU_ggp + labs(title = "LP-GU (Unmowed Grass Environment)", x = "Ladybug Species", y = "Count of Species")

# complete graph get flipped with the
# help of coord_flip() function
plotType_LPGU_ggp +  coord_flip() 

df_plotType_LPGM <- df_joined_ladybug_dataframes %>%
  filter(plotType != "LP-GM") %>%
  count(species) %>%
  rename(speciesPerPlot = n)

plotType_LPGM_data <- data.frame(x_axis = df_plotType_LPGM$species,  
                                 y_axis = df_plotType_LPGM$speciesPerPlot)

# bar is created with the help of
# grom_bar() and ggplot() function
plotType_LPGM_ggp <- ggplot(plotType_LPGM_data, aes(x_axis, y_axis, fill = y_axis)) +   
  geom_bar(stat = "identity")

plotType_LPGM_ggp <- plotType_LPGM_ggp + labs(title = "LP-GM (Mowed Grass Environment)", x = "Ladybug Species", y = "Count of Species")

# complete graph get flipped with the
# help of coord_flip() function
plotType_LPGM_ggp +  coord_flip() 

df_plotType_LPGF <- df_joined_ladybug_dataframes %>%
  filter(plotType != "LP-GF") %>%
  count(species) %>%
  rename(speciesPerPlot = n)

plotType_LPGF_data <- data.frame(x_axis = df_plotType_LPGF$species,  
                                 y_axis = df_plotType_LPGF$speciesPerPlot)

# bar is created with the help of
# grom_bar() and ggplot() function
plotType_LPGF_ggp <- ggplot(plotType_LPGF_data, aes(x_axis, y_axis, fill = y_axis)) +   
  geom_bar(stat = "identity")

plotType_LPGF_ggp <- plotType_LPGF_ggp + labs(title = "LP-GF (Forested Environment)", x = "Ladybug Species", y = "Count of Species")

# complete graph get flipped with the
# help of coord_flip() function
plotType_LPGF_ggp +  coord_flip() 

df_plotType_LPGA <- df_joined_ladybug_dataframes %>%
  filter(plotType != "LP-GA") %>%
  count(species) %>%
  rename(speciesPerPlot = n)

plotType_LPGA_data <- data.frame(x_axis = df_plotType_LPGA$species,  
                                 y_axis = df_plotType_LPGA$speciesPerPlot)

# bar is created with the help of
# grom_bar() and ggplot() function
plotType_LPGA_ggp <- ggplot(plotType_LPGA_data, aes(x_axis, y_axis, fill = y_axis)) +   
  geom_bar(stat = "identity")

plotType_LPGA_ggp <- plotType_LPGA_ggp + labs(title = "LP-GA (Garden Environment)", x = "Ladybug Species", y = "Count of Species")

# complete graph get flipped with the
# help of coord_flip() function
plotType_LPGA_ggp +  coord_flip() 

df_plotType_LPAG <- df_joined_ladybug_dataframes %>%
  filter(plotType != "LP-AG") %>%
  count(species) %>%
  rename(speciesPerPlot = n)

plotType_LPAG_data <- data.frame(x_axis = df_plotType_LPAG$species,  
                                 y_axis = df_plotType_LPAG$speciesPerPlot)

# bar is created with the help of
# grom_bar() and ggplot() function
plotType_LPAG_ggp <- ggplot(plotType_LPAG_data, aes(x_axis, y_axis, fill = y_axis)) +   
  geom_bar(stat = "identity")

plotType_LPAG_ggp <- plotType_LPAG_ggp + labs(title = "LP-AG (Agricultural Environment)", x = "Ladybug Species", y = "Count of Species")

# complete graph get flipped with the
# help of coord_flip() function
plotType_LPAG_ggp +  coord_flip() 


