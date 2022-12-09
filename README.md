# LadybugQuery.R
An R query built upon a dataset of ladybugs collected by students at Augustana College. This query was used to clean all of the data in the dataset for later use

# LadybugAnalysis.R
LadybugAnalysis was used to take in the cleaned data from LadybugQuery.R and then create visuals and a t test that help us gain valuable insight on the research conducted by our fellow Augustana
Students.

# Questions we asked of our data
**1) How were species represented acrossed the different environments surveyed?**
  * To answer this we used a stacked bar chart created with the code

  ``` r
  plotType_by_species_ggp <- (ggplot(df_species_plotTypes, aes(commonName, speciesCount, fill = environment)) +   
  geom_col(stat = "identity") +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  theme_dark() +
  coord_flip() +
  labs(title = "Environments Per Species", x = "Ladybug Species", y = "Count of Species")) %T>%
  plot()
  ```
  
  Resulting Image
  
  <img src="images/Question 1 Visual.png" alt="Question 1 Visual" width="600" height="300">
  
* This shows that what proportions of each species was found in which environment
* This helps us to better understands the species of ladybugs within our research and what environments they reside within the most
    
**2) What months were responsible for the largest proportions of each species?**
  * To answer this we used a boxplot created with the code 
  ```r
  species_months_ggp <- (ggplot(df_species_months, aes(commonName, lubridate::yday(x = eventDate))) +   
  geom_boxplot(stat = "boxplot", position = "dodge2") +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  theme_dark() +
  scale_y_continuous(limits = c(0, 365),
                     breaks = c(31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365), 
                     labels = c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  coord_flip() +
  labs(title = "Observations Over Time", x = "Ladybug Species", y = "Observation Date")) %T>%
  plot()
  ```
  
  Resulting Image
  
  <img src="images/Question 2 Visual.png" alt="Question 2 Visual" width="600" height="300">
  
  * This shows in what months / time of months the greatest proportion of each species was collected and when paired with the findings
    from Question 1 we now have a better understanding of where and when these species of ladybugs are most active
    
**3) When was each plot the most useful over the course of the research?**
  * To answer this we used a line chart as well as an area chart which were created with the code
  ```r
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
  ```
  Resulting Image
  
  <img src="images/Question 3 Visual.png" alt="Question 3 Visual" width="600" height="300">
  
  * This shows how each plot type performed through the months and compares each ones performance against one another
  * We see that the Industrial (LP-IC) and Unmowed Grass (LP-GU) environments performed the best amongst all of the environments
  * When paired with the results of the first two questions our researchers now have a better understanding of the where and when these species 
    are moving they are now able to leverage this with when each plot is the most useful so they can now be more effective in their collection practices

# T-Test
Based on our visualizations we wanted to see if the plotType used to collect the ladybugs

```r
df_count_plotType <- df_joined_ladybug_dataframes %>%
  select(plotType) %>%
  group_by(plotType) %>%
  summarise(countPlotType = length(plotType)) %>%
  filter(plotType != "Unknown")

t.test(df_count_plotType$countPlotType)
```

The Results of the one sample T-Test:

data:  df_count_plotType$countPlotType
**t = 5.2268, df = 6, p-value = 0.001963
alternative hypothesis: true mean is not equal to 0
     95 percent confidence interval:
          37.76126 104.23874
sample estimates: mean of x 71** 

* As we can see the plot types does have a significant difference on how many lady bugs you are able to find
* This adds extra value to our visuals
