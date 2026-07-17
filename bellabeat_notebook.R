# bellabeat study case notebook


# setup -------------------------------------------------------------------


library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(snakecase)
library(janitor)


# creating master table ---------------------------------------------------


master_activity_raw <- c(
  list.files(path="Fitabase Data 3.12.16-4.11.16", pattern = "dailyActivity", full.names = TRUE),
  list.files(path="Fitabase Data 4.12.16-5.12.16", pattern = "dailyActivity", full.names = TRUE)
) %>% 
lapply(read_csv) %>% 
  bind_rows()
  
master_act_engage_raw <- c(
  list.files(path="Fitabase Data 3.12.16-4.11.16", pattern = "hourlyIntensities|hourlySteps", full.names = TRUE),
  list.files(path="Fitabase Data 4.12.16-5.12.16", pattern = "hourlyIntensities|hourlySteps", full.names = TRUE)
) %>% 
  lapply(read_csv) %>% 
  bind_rows()

# The code below is not needed cause can't find a way to correlate sleep minute with engagement
# master_engage_raw <- c(
  # list.files(path="Fitabase Data 3.12.16-4.11.16", pattern = "minuteSleep", full.names = TRUE),
# list.files(path="Fitabase Data 4.12.16-5.12.16", pattern = "minuteSleep", full.names = TRUE)
# ) %>%
  # lapply(read_csv) %>%
  # bind_rows()

# cleaning activity and engage----------------------------------------------------------------

## Master table for activity and engage
glimpse(master_act_engage_raw)

### Adding day and formatting date time & cleaning name consistency
master_act_engage <- master_act_engage_raw %>%
  mutate(
    ActivityHour = if (is.character(ActivityHour)) mdy_hms(ActivityHour) else ActivityHour,
    Date = as_date(ActivityHour),
    Time = format(ActivityHour, "%H:%M:%S"),
    Weekday = wday(ActivityHour, label = TRUE, abbr = FALSE),
  ) %>% 
clean_names() %>% 
  distinct()
  
### Easily find NA's (NA's will be ignored)
summary(master_act_engage)

### Unique ID |35
master_act_engage %>% count(id, sort = TRUE)
n_distinct(master_activity$id)

###Last check glimpse
glimpse(master_act_engage)


# cleaning activity -------------------------------------------------------
glimpse(master_activity_raw)

### adding weekday format, changing date chr to dttm and adding ordinal weekday | Using if else to make it safe for re-run code
master_activity <- master_activity_raw %>% 
  mutate(
    date = if (is.character(ActivityDate)) mdy(ActivityDate) else ActivityDate,
    weekday = wday(date, label = TRUE, abbr = FALSE)
  ) %>% 
  clean_names() %>% 
  distinct()

### Checking NA's | No NA's appeared
summary(master_activity)
colSums(is.na(master_activity))

### Unique ID | 35
master_activity %>% count(id, sort = TRUE)
n_distinct(master_activity$id)

# cleaning engage ---------------------------------------------------------
#glimpse(master_engage_raw)
# Dropping this master table, haven't find a peer reviewed research on wearing device while asleep proves more engagemment therefore not using it.


# analysis ----------------------------------------------------------------

#creating analysis table
master_activity_analysis <- master_activity %>%
  select(
    id, date, weekday,
    total_steps,
    calories,
    very_active_minutes,
    fairly_active_minutes,
    very_active_distance,
    moderately_active_distance,
  )

master_act_engage_analysis <- master_act_engage %>% 
  select(
    id, date, time,
    weekday, total_intensity
  )

# TODO create k means progress (elbow point,clustering)
