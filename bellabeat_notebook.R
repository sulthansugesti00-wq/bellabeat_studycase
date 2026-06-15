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

master_engage_raw <- c(
  list.files(path="Fitabase Data 3.12.16-4.11.16", pattern = "minuteSleep", full.names = TRUE),
  list.files(path="Fitabase Data 4.12.16-5.12.16", pattern = "minuteSleep", full.names = TRUE)
) %>%
  lapply(read_csv) %>%
  bind_rows()

# cleaning ----------------------------------------------------------------

## Master table for activity and engage
glimpse(master_act_engage_raw)

# Adding day and formatting date time & cleaning name consistency
master_act_engage <- master_act_engage_raw %>%
  mutate(
    ActivityHour = if (is.character(ActivityHour)) mdy_hms(ActivityHour) else ActivityHour,
    Date = as_date(ActivityHour),
    Time = format(ActivityHour, "%H:%M:%S"),
    Weekday = wday(ActivityHour, label = TRUE, abbr = FALSE),
  ) %>% 
clean_names() %>% 
  distinct()
  
# Easily find NA's (NA's will be ignored)
summary(master_act_engage)
# Unique ID
master_act_engage %>% count(id, sort = TRUE)
#Last check glimpse
glimpse(master_act_engage)


## Master table for activity
glimpse(master_activity_raw)


## Master table for engage
glimpse(master_engage_raw)