# libraries
library(dplyr)
library(lubridate)

# original data
d <- readRDS("data/NEW_DATA.rds")

# initial cleaning
data <- d %>% 
  select(WL_ID_CODE, WLREG_AUDIT_ID_CODE, INIT_STAT, INIT_DATE, CHG_DATE,
         UNOS_CAND_STAT_CD, END_DATE, REM_CD, FTIME, FSTATUS, 
         DAYSWAIT_CHRON, INIT_AGE) %>%
  mutate(INIT_DATE = ymd(INIT_DATE),
         END_DATE = ymd(END_DATE),
         CHG_DATE = ymd(CHG_DATE),
         REM_CD_NEW = case_when(
           END_DATE > as.Date("2024-12-31") ~ 9999, # censoring anyone with end date after dec 31
           TRUE ~ REM_CD),
         END_DATE_NEW = case_when(
           END_DATE > as.Date("2024-12-31") ~ as.Date("2024-12-31"),
           TRUE ~ END_DATE), # keeping original end date/status if end date is not after dec 31
         INITIAL_STATUS = case_when( # making statuses easier to read
           INIT_STAT == 2110 ~ 1,
           INIT_STAT == 2120 ~ 2,
           INIT_STAT == 2130 ~ 3,
           INIT_STAT == 2140 ~ 4,
           INIT_STAT == 2150 ~ 5,
           INIT_STAT == 2160 ~ 6,
           TRUE ~ NA_real_),
         STATUS = case_when( # same as above comment
           UNOS_CAND_STAT_CD == 2110 ~ 1,
           UNOS_CAND_STAT_CD == 2120 ~ 2,
           UNOS_CAND_STAT_CD == 2130 ~ 3,
           UNOS_CAND_STAT_CD == 2140 ~ 4,
           UNOS_CAND_STAT_CD == 2150 ~ 5,
           UNOS_CAND_STAT_CD == 2160 ~ 6,
           UNOS_CAND_STAT_CD == 2999 ~ 2999,
           TRUE ~ NA_real_))

# FILTERING
# creating groups
status2_dates <- data %>% # finding the first status 2 date for each candidate
  filter(STATUS == 2) %>%
  group_by(WL_ID_CODE) %>%
  summarise(first_status2 = min(CHG_DATE),
            .groups = "drop")

# keeping candidates who reached status 2 eventually
data_extra <- data %>%
  inner_join(status2_dates, by = "WL_ID_CODE") %>%
  group_by(WL_ID_CODE) %>%
  mutate(status1_before_status2 = as.integer(any(STATUS == 1 & CHG_DATE < first_status2, na.rm = TRUE))) %>%
  ungroup()

# keeping rows starting at first status 2
data2 <- data_extra %>%
  arrange(WL_ID_CODE, CHG_DATE, WLREG_AUDIT_ID_CODE) %>%
  filter(CHG_DATE >= first_status2) %>%
  mutate(STAT2_INIT = first_status2)

# creates one row per candidate with initial status 2 date + time at status 2
groups <- data2 %>%
  group_by(WL_ID_CODE) %>%
  summarise(first_status = first(INITIAL_STATUS),
            init_date = first(INIT_DATE),
            time0 = first(STAT2_INIT),
            status1_before_status2 = first(status1_before_status2),
            .groups = "drop") %>%
  mutate(group = case_when(
    first_status == 2 ~ 0,
    TRUE ~ 1),
    n_days_to_status2 = as.numeric(time0 - init_date))

# JOINS
# choosing the first event after status 2
event_rows <- data2 %>%
  mutate(event_date = case_when(
    is.na(END_DATE_NEW) ~ CHG_DATE,
    is.na(CHG_DATE) ~ END_DATE_NEW,
    TRUE ~ pmin(CHG_DATE, END_DATE_NEW)),
    event_source = case_when(
      !is.na(END_DATE_NEW) & (is.na(CHG_DATE) | END_DATE_NEW <= CHG_DATE) ~ "removal",
      !is.na(CHG_DATE) & (is.na(END_DATE_NEW) | CHG_DATE < END_DATE_NEW) ~ "status_change",
      TRUE ~ NA_character_)) %>%
  filter(STATUS != 2 | END_DATE_NEW <= CHG_DATE) %>%
  group_by(WL_ID_CODE) %>%
  slice_min(event_date, with_ties = FALSE) %>%
  ungroup()

# coding the event type
heart_events <- event_rows %>%
  mutate(event = case_when(
    event_source == "removal" & REM_CD_NEW %in% c(5, 8, 13) ~ 1, # death/deterioration
    event_source == "removal" & REM_CD_NEW %in% c(2, 3, 4, 14, 21, 22) ~ 2, # transplant
    event_source == "removal" & REM_CD_NEW == 12 ~ 3, # recovery
    event_source == "status_change" & STATUS == 1 ~ 4, # upgrade
    event_source == "status_change" & STATUS %in% 3:6 ~ 5, # downgrade
    event_source == "status_change" & STATUS == 2999 ~ 7, # delisted
    TRUE ~ 6)) %>% # censored
  select(WL_ID_CODE, event_date, event_source, event)

# merging back all original variables with updated censoring variables
sub <- d %>%
  select(-END_DATE_NEW, -REM_CD_NEW) %>%
  distinct(WL_ID_CODE, .keep_all = TRUE) %>%
  left_join(data %>% distinct(WL_ID_CODE, END_DATE_NEW, REM_CD_NEW), by = "WL_ID_CODE")

heart_extra <- groups %>% # this one includes the people with status 1 to 2
  left_join(heart_events, by = "WL_ID_CODE") %>%
  left_join(sub, by = "WL_ID_CODE") %>%
  mutate(time_to_event = as.numeric(event_date - time0), # creating time till event variable
         age_at_status2 = INIT_AGE + n_days_to_status2 / 365.25) %>% # creating adjusted age variable
  filter(is.na(time_to_event) | time_to_event >= 0) %>%
  select(WL_ID_CODE, time0, event_date, event, event_source,
         time_to_event, group, n_days_to_status2, age_at_status2,
         status1_before_status2, first_status, init_date, INIT_AGE, everything())

# final analytic dataset (excluding the status 1 to 2)
heart3 <- heart_extra %>%
  filter(status1_before_status2 == 0)