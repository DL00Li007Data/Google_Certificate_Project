---
title: "Google_Capstone_Project_Bellabeat_Case_withR"
author: "Danning_Li_007"
date: "2/7/2022"
output: html_document
---

###

```{r}
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(janitor)
library(readr)
library(skimr)
library(kableExtra)
library(wesanderson)
library(Hmisc)
library(knitr)
library(sqldf)
library(glue)
library(ggtext)
library(ggridges)
library(RColorBrewer)
```
## # import csv files from Downloads file 
# daily_Calories's data content is include in daily_activity

```{r}
daily_activity <- read_csv("../Downloads/Capstone/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_Calories <- read_csv("../Downloads/Capstone/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
weight_log <- read_csv("../Downloads/Capstone/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
sleep_Day <- read_csv("../Downloads/Capstone/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
daily_Steps<- read_csv("../Downloads/Capstone/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
daily_Intensities<- read_csv("../Downloads/Capstone/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
hourlyCalories <- read_csv("../Downloads/Capstone/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourlyIntensities_merged <- read_csv("../Downloads/Capstone/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hourlySteps_merged <- read_csv("../Downloads/Capstone/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")

```
## A.1 

```{r}
# Assign use function/ category
daily_activity_users <- daily_activity %>% 
  drop_na(Id) %>% 
  arrange(Id) %>% 
  distinct(Id) %>% 
  cbind(User_category = "Activity") %>% 
  select(Id, User_category)


daily_Intensities_users <- daily_Intensities %>% 
  drop_na(Id) %>% 
  arrange(Id) %>% 
  distinct(Id) %>% 
  cbind(User_category = "Intensities") %>% 
  select(Id, User_category)


daily_Steps_users <- daily_Steps %>% 
  drop_na(Id) %>% 
  arrange(Id) %>% 
  distinct(Id) %>% 
  cbind(User_category = "Steps") %>% 
  select(Id, User_category)


sleep_Day_users <- sleep_Day %>% 
  drop_na(Id) %>% 
  arrange(Id) %>% 
  distinct(Id) %>% 
  cbind(User_category = "Sleeps") %>% 
  select(Id, User_category)


weight_log_users <- weight_log %>% 
  drop_na(Id) %>% 
  arrange(Id) %>% 
  distinct(Id) %>% 
  cbind(User_category = "Weight") %>% 
  select(Id, User_category)

```


```{r}
# Create overall dataframe 
User_distribution <- rbind( daily_activity_users, 
                            daily_Intensities_users,
                            daily_Steps_users,
                            sleep_Day_users, 
                            weight_log_users)
glimpse(User_distribution)

```
### Percentage of Activities

```{r}
# A.1 The total number of unique users is 131, from this combination result,
# we could intuitively detect which catagory has the most users.

# Percentage of Usage
count_users <- length(unique(User_distribution$Id))

Percentage_of_Usage <- User_distribution %>% 
  group_by(User_category) %>% 
  summarise(count_User_category = n()) %>%
  mutate(Total_Percent_active = round((count_User_category / count_users),4)*100) 
  

ggplot(Percentage_of_Usage, aes(x=reorder(User_category,-Total_Percent_active), y=Total_Percent_active, fill=User_category)) +
  geom_bar(stat="identity")+
  labs(title = "Percentage_of_Activity", x="", y="Percentage_activities", fill="User_category")+
  scale_y_continuous(expand=c(0, 0)) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(panel.grid=element_blank(), panel.background=element_rect(color='black', fill='transparent')) +
  geom_text(aes(x = User_category, y = Total_Percent_active, label = Total_Percent_active), colour = "white", size = 3, vjust = 2, fontface = "bold")

```

### TotalMinutesAsleep vs Activity_levels

```{r}
# A.2 The total number of unique users is 131, from this combination result, we could intuitively detect which category has the most users.


# Reorganize the date formate of Activity and Sleeps

Activity2 <- daily_activity %>% 
  mutate(ActivityDate = mdy(ActivityDate)) %>% 
  rename(Date = ActivityDate)

Sleeps2 <- sleep_Day %>% 
  mutate(SleepDay = mdy_hms(SleepDay)) %>% 
  rename(Date = SleepDay)

Intensities2 <- daily_Intensities %>%
  mutate(ActivityDay = mdy(ActivityDay)) %>%
  rename(Date = ActivityDay)

Steps2 <- daily_Steps %>%
  mutate(ActivityDay = mdy(ActivityDay)) %>%
  rename(Date = ActivityDay)


# Adding Activeness column
Activity_levels <- Activity2 %>%
  mutate(Day = weekdays(as.Date(Date)),
         Activity_levels = case_when(.$TotalSteps <= 5000 ~ "Sedentary",
                                     .$TotalSteps <= 7500 ~ "Low_Activities",
                                     .$TotalSteps <= 10000 ~ "Medium_Activities",
                                     .$TotalSteps <= 12500 ~ "Activities",
                                     .$TotalSteps > 12500 ~ "High_Activities"))

# Adding Sleep column
Sleeps_levels <- Sleeps2 %>%
  mutate(Sleeps_levels = case_when(.$TotalMinutesAsleep <= 360 ~ "Low_Sleep",
                                   .$TotalMinutesAsleep > 360 & TotalMinutesAsleep <= 510 ~ "Reasonalble_Sleep",
                                   .$TotalMinutesAsleep > 510 ~ "More_Sleep"))


#Sort Day levels
Activity_levels$Day <- factor(Activity_levels$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) 



# Merging the Activity and sleeps dataset
Activity_Sleep <- merge(Activity_levels, Sleeps_levels, by=c("Id", "Date"))

```

#### Plot for TotalMinutesAsleep vs Activity_levels 1

```{r}
# A.3 TotalMinutesAsleep vs Activity_levels 1

ggplot(Activity_Sleep, aes(x= TotalMinutesAsleep, fill = Activity_levels)) +
  geom_density(position = "fill") 
```

#### Plot of TotalMinutesAsleep vs Activity_levels 2

```{r}
# A.3 TotalMinutesAsleep vs Activity_levels 2
ggplot(Activity_Sleep) +
  ggridges:: geom_density_ridges(aes(x = TotalMinutesAsleep, y = Activity_levels, fill = Activity_levels))

```

#### BoxPlot of ActivityLevels_vs_TotalMinutesAsleep

```{r}

# A.4 BoxPlot_ActivityLevels_vs_TotalMinutesAsleep 
ggplot(Activity_Sleep, aes(x = Activity_levels, y = TotalMinutesAsleep, color = Activity_levels)) +
  geom_boxplot(width = 0.65) +
  geom_point(position = position_jitter(seed = 200, width = 0.25))+
  labs(
    title = "BoxPlot_ActivityLevels_vs_TotalMinutesAsleep",
    subtitle = "Total_Users = 413",
    caption = "Source: FitBit Fitness Tracker Data",
    x = "Activity_levels"
  )

```
#### Plot of Activity_Sleep vs Calories vs Day

```{r}
# A.5 Plot of Activity_Sleep vs Calories vs Day
p <- ggplot(data=Activity_Sleep,
  mapping = aes(
    x = Calories,
    y = TotalMinutesAsleep,
    color = Day,
    fill = Day))
p + geom_point() +
  geom_smooth(method="loess") +
    labs(
    title = "Activity_Sleep vs Calories vs Day",
    subtitle = "Total_Users = 413",
    caption = "Source: FitBit Fitness Tracker Data",
    x = "Calories"
  )

```

#### Plot of TotalSteps_vs_WeekDay

```{r}

# A.6 TotalSteps_vs_WeekDay
ggplot(Activity_Sleep, aes(x = TotalSteps, y = Day, col= factor(Day)), alpha= 0.4) +
 geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "TotalSteps") +
  coord_cartesian(clip = "off") +
  labs(
    title = 'TotalSteps_vs_WeekDay',
    subtitle = "Total_Users = 413",
    caption = "Source: FitBit Fitness Tracker Data",
    x = "Total_Steps",
    fill = "# Cylinders"
  ) +
  theme_ridges(font_size = 13, grid = TRUE) + 
  theme(axis.title.y = element_blank())
```

### Daily Activity Usage Data Analysis

```{r}

# B.1 Daily Activity Usage - Data Analysis

# set min and max date value
max_date <- as.character(max(Activity_levels$Date))
min_date <- as.character(min(Activity_levels$Date))

# calculate different days between min and max date
different_days = length(unique(Activity_levels$Date))

# set max user value
max_users <- length(unique(Activity_levels$Id))

# set min and max TotalSteps value
max_TotalSteps <- max(Activity_levels$TotalSteps)
min_TotalSteps <- min(Activity_levels$TotalSteps)

# set avg user value, sum/(33*31)
Average_steps_Daily <- round(sum(Activity_levels$TotalSteps) / (max_users*different_days), digits = 0)


```

#### Correlation for TotalSteps vs TotalMinutesAsleep vs Calories
```{r}
# B.2 SQL for Correlation of 3 parameters

Corr_SQL <- sqldf::sqldf('
   SELECT TotalSteps, Calories, TotalMinutesAsleep
   FROM Activity_Sleep')

head(Corr_SQL,3)
```

```{r}
# B.3 Correlation for 3 parameters

corr_data <- as.data.frame(round(cor(Corr_SQL), 2))
corr_data$var1 <- rownames(corr_data)
data <- gather(corr_data, key = "var2", value = "corr", -var1)

head(data,3)
```

```{r}
# B.4 Correlation for TotalSteps vs TotalMinutesAsleep vs Calories

my_color <- brewer.pal(3, "Dark2")

ggplot(data, aes(var1, var2, fill = corr)) +
  geom_tile(colour = "black") +
  scale_fill_gradientn(colours = my_color)+
  geom_text(aes(label = corr), size = 5, colour = "black", alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.title = element_blank()) +
  labs(title = "Correlation for TotalSteps vs TotalMinutesAsleep vs Calories", 
       subtitle = "Total_Users = 413",
       caption = "Source: FitBit Fitness Tracker Data") 

```


### TotalMinutesAsleep vs TotalSteps
```{r}
# "Step_level = 0" means TotalSteps less than Average_steps_Daily, "Step_level = 1" means TotalSteps more than Average_steps_Daily.

Activity_levels_SQL <- sqldf::sqldf('
     SELECT Date, Steps_level, (SELECT count(Id) from Activity_levels2 GROUP BY Date) as TotalId_daily
        , count(Id) as Diff_level_steps
     FROM Activity_levels2
     GROUP BY Date, Steps_level
     ')
head(Activity_levels_SQL)

```
### Plot for Steps_level Daily Through one Month

```{r}
# Plot for Steps_level Daily Through one Month

p2 <- ggplot(data = Activity_levels_SQL, aes(x = Date, y = Diff_level_steps, fill = Steps_level)) + 
  geom_col(position = "dodge") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 36), breaks = seq(0,33, by = 2)) +
  scale_x_date(date_labels="%d %b '%y", breaks = "1 day", expand = c(.01, .01)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.title = element_blank()) +
  labs(title = "Steps_level Daily Through one Month", 
       subtitle = paste0(" Steps < Ave_steps is Dark Blue,   Steps >= Ave_steps is Light Blue "),
       caption = "Source: FitBit Fitness Tracker Data, Mobius") +
  xlab("Date") +
  ylab("Number of Users") +
  geom_hline(yintercept = max_users, linetype = "dashed")

  
p2

```


```{r}
# Plot for Steps_level Daily Through one Month？?????????

ggplot(Activity_levels_SQL, aes(x = Steps_level, fill= Steps_level)) +
  geom_density(aes(fill = factor(Steps_level)), alpha = 0.8) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 36), breaks = seq(0,33, by = 2)) +
  scale_x_date(date_labels="%d %b '%y", breaks = "1 day", expand = c(.01, .01))+

  labs(
    title = "Density plot of Steps_level Daily Through one Month",
    subtitle = "Steps < Ave_steps is 0,   Steps >= Ave_steps is 1",
    caption = "Source: FitBit Fitness Tracker Data, Mobius",
    x = "Date",
    fill = "Steps_level"
  )




```

```{r}

p3 <- ggplot(data = Activity_levels_SQL, aes(x = Date, y = Diff_level_steps, fill = Steps_level)) + 
  geom_col(position = "dodge") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 36), breaks = seq(0,33, by = 2)) +
  scale_x_date(date_labels="%d %b '%y", breaks = "1 day", expand = c(.01, .01)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.title = element_blank()) +
  labs(title = "Steps_level Daily Through one Month", 
       subtitle = paste0(" Steps < Ave_steps is Dark Blue,   Steps >= Ave_steps is Light Blue "),
       caption = "Source: FitBit Fitness Tracker Data, Mobius") +
  xlab("Date") +
  ylab("Number of Users") +
  geom_hline(yintercept = max_users, linetype = "dashed")

  
p3
```


```{r}

# WeekDays vs TotalMinutesAsleep vs TotalSteps

ggplot(Activity_Sleep, mapping = aes(x = TotalMinutesAsleep, y = TotalSteps, color = TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Day) +
  labs(title = "Total Steps vs Sleep_Minutes", x = "TotalMinutesAsleep") +
  scale_color_gradient(low = "#0000FF", high = "#FF1493") +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.title = element_blank()) +
  labs(title = "Week days for TotalMinutesAsleep vs TotalSteps", 
       subtitle = paste0("Data for ", min_date, " to ", max_date, "\nAverage_steps_Daily: ", Average_steps_Daily, "\nmax_TotalSteps:", max_TotalSteps),
       caption = "Source: FitBit Fitness Tracker Data") +
  geom_hline(yintercept = Average_steps_Daily, linetype = "dashed", col = "red")+
  xlab("Total Minutes Asleep") + ylab("Total Steps Daily")



```

```{r}


# Overall of steps

Activity_levels2 <- Activity_levels %>%
  mutate(Steps_level = case_when(.$TotalSteps > Average_steps_Daily ~ 1, 
                                     .$TotalSteps <= Average_steps_Daily ~ 0), 
                                      Average_steps_Daily)

Activity_levels4 <- Activity_levels %>%
        mutate(Avg_steps_level = case_when(.$TotalSteps > Average_steps_Daily ~ "More than Avg Steps Daily", 
                                           .$TotalSteps <= Average_steps_Daily ~"Less than Avg Steps Daily")) 


# total usage per day
Activity_levels3 <- Activity_levels2%>% 
  group_by(Date) %>% 
  summarise(total_users = length(unique(Activity_levels2$Id))) %>% 
  mutate(total_users) 

```

```{r}

Activity_levels_SQL <- sqldf::sqldf('
     SELECT Date, Steps_level, (SELECT count(Id) from Activity_levels2 GROUP BY Date) as TotalId_daily
        , count(Id) as Diff_level_steps
     FROM Activity_levels2
     GROUP BY Date, Steps_level
     ')

```

```{r}

# C.1 Hours Part -Excel cleaning for the time columns


merge_1 <-merge(hourlyCalories, hourlyIntensities_merged, by = c("Id","Date","Hours-24"))
Hours_merge_all <-merge(merge_1, hourlySteps_merged, by = c("Id","Date","Hours-24")) %>%
  select(Id, Date, 'Hours-24', Calories, TotalIntensity, StepTotal)
  

# C.2 Different Hours Parts

# set min and max date value
max_Calories_hourly <- as.character(max(Hours_merge_all$Calories))
min_Calories_hourly <- as.character(min(Hours_merge_all$Calories))


# set min and max TotalIntensity value
max_TotalIntensity_hourly <- max(Hours_merge_all$TotalIntensity)
min_TotalIntensity_hourly <- min(Hours_merge_all$TotalIntensity)


# set min and max TotalSteps value
max_TotalSteps_hourly <- max(Hours_merge_all$StepTotal)
min_TotalSteps_hourly <- min(Hours_merge_all$StepTotal)

# Visualization
Hours_merge_all$`Hours-24` <-as.numeric(Hours_merge_all$`Hours-24`)


```


```{r}

# Hours_of_Calories
p1_Calories <- ggplot(data = Hours_merge_all, aes(x=`Hours-24`, y=Calories, color= Calories)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 24)) +
  scale_color_gradient(low = "#9AFF9A" , high = "#FF0000", na.value = "white") +
  geom_smooth(method = "gam", color = "blue") +
  labs(titles = "Hours_of_Calories", x= "24_Hours", y= "Calories(k)")+
  
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1), 
        legend.title = element_blank()) +
  labs(title = "Calories for Each Hour", 
       subtitle = ("Data Points = 22099"),
       caption = "Source: FitBit Fitness Tracker Data") 

  
p1_Calories


```

##

```{r}

# Hours_of_TotalIntensity
p2_TotalIntensity <- ggplot(data = Hours_merge_all, aes(x=`Hours-24`, y=TotalIntensity, color= TotalIntensity)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 24)) +
  scale_color_gradient(low = "#87CEEB" , high = "#FF0000", na.value = "white") +
  geom_smooth(method = "gam", color = "blue") +
  labs(titles = "Hours_of_TotalIntensity", x= "24_Hours", y= "TotalIntensity") +
  
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1), 
        legend.title = element_blank()) +
  labs(title = "TotalIntensity for Each Hour", 
       subtitle = ("Data Points = 22099"),
       caption = "Source: FitBit Fitness Tracker Data") 

p2_TotalIntensity

```

###

```{r}

# Hours_of_TotalIntensity
p3_TotalSteps <- ggplot(data = Hours_merge_all, aes(x=`Hours-24`, y=StepTotal, color= StepTotal)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 24)) +
  scale_color_gradient(low = "#CAFF70" , high = "#0000FF", na.value = "white") +
  geom_smooth(method = "gam", color = "blue") +
  labs(titles = "Hours_of_TotalSteps", x= "24_Hours", y= "TotalSteps") +
  
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1), 
        legend.title = element_blank()) +
  labs(title = "TotalSteps for Each Hour", 
       subtitle = ("Data Points = 22099"),
       caption = "Source: FitBit Fitness Tracker Data") 

p3_TotalSteps


```

