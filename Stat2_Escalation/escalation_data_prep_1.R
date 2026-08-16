rm(list = ls())
setwd("/Users/ekawaguc/OneDrive - Keck Medicine of USC/")
library(dplyr) # for data manipulation
library(magrittr) # for data manipulation
library(haven) # for reading sas files into R

# Students can't run this portion of the code since it requires the original datafiles from UNOS
source("CVM/Data/source/convertVariables.R")
data_dictionary <- read.csv("CVM/Data/source/dictionary.csv")

# Data source:Summer 25 Adult Cohort
dat0 <- readRDS("CVM/Data/generalAdultThoracicCohort/Su25_Adult_HRT_cohort_2018-10-18_2025-06-30.rds")  %>%
  convertVariables()


# Getting all the status data
STAT1 <- haven::read_sas("/Users/ekawaguc/Library/CloudStorage/OneDrive-KeckMedicineofUSC/UNOS/062025/Thoracic/Waiting List History/Status Justification Data (Heart 1A_ 1B)/thoracic_stat1.sas7bdat") %>%
  select(WL_ID_CODE, WLREG_AUDIT_ID_CODE, UNOS_CAND_STAT_CD, UNOS_CAND_STAT_DT, FORMEFFECTIVEDT, EXCEPTION)
STAT2 <- haven::read_sas("/Users/ekawaguc/Library/CloudStorage/OneDrive-KeckMedicineofUSC/UNOS/062025/Thoracic/Waiting List History/Status Justification Data (Heart 1A_ 1B)/thoracic_stat2.sas7bdat") %>%
  select(WL_ID_CODE, WLREG_AUDIT_ID_CODE, UNOS_CAND_STAT_CD, UNOS_CAND_STAT_DT, FORMEFFECTIVEDT, EXCEPTION)
STAT3 <- haven::read_sas("/Users/ekawaguc/Library/CloudStorage/OneDrive-KeckMedicineofUSC/UNOS/062025/Thoracic/Waiting List History/Status Justification Data (Heart 1A_ 1B)/thoracic_stat3.sas7bdat") %>%
  select(WL_ID_CODE, WLREG_AUDIT_ID_CODE, UNOS_CAND_STAT_CD, UNOS_CAND_STAT_DT, FORMEFFECTIVEDT, EXCEPTION)
STAT4 <- haven::read_sas("/Users/ekawaguc/Library/CloudStorage/OneDrive-KeckMedicineofUSC/UNOS/062025/Thoracic/Waiting List History/Status Justification Data (Heart 1A_ 1B)/thoracic_stat4.sas7bdat") %>%
  select(WL_ID_CODE, WLREG_AUDIT_ID_CODE, UNOS_CAND_STAT_CD, UNOS_CAND_STAT_DT, FORMEFFECTIVEDT, EXCEPTION)
STAT_DAT <- rbind(STAT1, STAT2, STAT3, STAT4) %>% arrange(WL_ID_CODE, WLREG_AUDIT_ID_CODE)

table(STAT_DAT$EXCEPTION, useNA = "ifany")

# Get filtering values
table(dat0$NUM_PREV_TX, dat0$PREV_TX)

# Filter to only consider:
# 1) Those listed between 2019-10-18 to 2025-03-30
# 2) First time candidates (no "re-dos")
# 3) Single organ candidates
dat <- dat0 %>%
  filter(
    INIT_DATE >= "2019-10-18" & INIT_DATE <= "2025-03-30",
    # First timers
    NUM_PREV_TX == 0, PREV_TX != "Y",
    # No heart+lung
    WLHL != "Y" & WLLU != "Y",
    # Single organ
    N_WL_ORG == 1,
  )

dim(dat)

# Get waitlist data 
wl_dat0 <- haven::read_sas("/Users/ekawaguc/Library/CloudStorage/OneDrive-KeckMedicineofUSC/UNOS/062025/Thoracic/Waiting\ List\ History/thoracic_wlhistory_data.sas7bdat") %>%
  filter(WL_ID_CODE %in% dat$WL_ID_CODE) %>%
  select(WL_ID_CODE, WLREG_AUDIT_ID_CODE, UNOS_CAND_STAT_CD, CHG_DATE, CHG_TY) %>%
  group_by(WL_ID_CODE, CHG_DATE) %>%
  mutate(CHG_n = row_number()) %>%
  ungroup()

length(dat$WL_ID_CODE)
table(dat$WL_ID_CODE %in% STAT_DAT$WL_ID_CODE)
length(unique(wl_dat0$WL_ID_CODE))

# Take a look at matches on STAT_DAT
tmp <- wl_dat0 %>% filter(WLREG_AUDIT_ID_CODE %in% STAT_DAT$WLREG_AUDIT_ID_CODE)
tmp <- STAT_DAT %>% filter(WLREG_AUDIT_ID_CODE %in% wl_dat0$WLREG_AUDIT_ID_CODE)

# Merge waitlist and status data
tmp <- left_join(wl_dat0, STAT_DAT, by = c("WL_ID_CODE", "WLREG_AUDIT_ID_CODE", "UNOS_CAND_STAT_CD")) %>%
  mutate(
    EXCEPTION = case_when(
      # Create EXCEPTION "0" for Status 6 to make it not missing
      UNOS_CAND_STAT_CD == 2160 ~ 0,
      .default = EXCEPTION
    )
  )

# Remove entries with old policy status codes
rm.ids <- tmp %>% filter(UNOS_CAND_STAT_CD %in% c(2010, 2020, 2030)) %>% pull(WL_ID_CODE)

tmp <- tmp %>% filter(!(WL_ID_CODE %in% rm.ids))
tmp <- tmp %>% filter(!is.na(EXCEPTION))


length(unique(tmp$WL_ID_CODE))

# Merge patient-level data with waitlist/status data
dat.a2 <- left_join(dat %>% filter(WL_ID_CODE %in% tmp$WL_ID_CODE), 
                    tmp, by = "WL_ID_CODE")

# Confirm first value INIT_DATE = UNOS_CAND_STAT
# Various checks
dat.tmp <- dat.a2 %>% group_by(WL_ID_CODE, CHG_DATE) %>% 
  slice(1) %>%
  ungroup() %>%
  group_by(WL_ID_CODE) %>% slice(1) %>%
  select(WL_ID_CODE, INIT_DATE, INIT_STAT, UNOS_CAND_STAT_DT, UNOS_CAND_STAT_CD, CHG_DATE)
table(dat.tmp$INIT_STAT, dat.tmp$UNOS_CAND_STAT_CD)

dat.tmp %>% mutate(DIFF = as.Date(INIT_DATE) - as.Date(CHG_DATE)) %>%
  select(WL_ID_CODE, DIFF, INIT_DATE, UNOS_CAND_STAT_DT, CHG_DATE, UNOS_CAND_STAT_CD) %>% View()

# Sanity check on exception
tmp.ids <- dat.a2 %>% filter(EXCEPTION == 1) %>% pull(WL_ID_CODE)
tmp.ids <- unique(tmp.ids)

# dat.a2 %>% filter(WL_ID_CODE %in% tmp.ids) %>% select(
#   WL_ID_CODE, CHG_DATE, UNOS_CAND_STAT_CD, EXCEPTION) %>%
#   head(20) %>% write_clip()


# Simple data frame
dat.new <- dat.a2 %>% group_by(WL_ID_CODE, CHG_DATE) %>%
  slice(1) %>% ungroup() %>% group_by(WL_ID_CODE) %>%
  mutate(UNOS_CAND_STAT_CD_WAITLIST = first(UNOS_CAND_STAT_CD))

dat.tmp <- dat.new %>% group_by(WL_ID_CODE) %>% slice(1) 
table(dat.tmp$INIT_STAT, dat.tmp$UNOS_CAND_STAT_CD_WAITLIST)

dat.a2 %>%
  group_by(WL_ID_CODE, UNOS_CAND_STAT_CD, CHG_DATE) %>%
  filter(n_distinct(EXCEPTION, na.rm = TRUE) > 1) %>%
  ungroup() %>% View()


# Want to make sure change dates are also all before 2025-03-30
dat <- dat.new %>% filter(CHG_DATE <= "2025-03-30")

saveRDS(dat, 
        file = "INIT_DATA_080726.rds")

# Students can start running here 

dat.0 <- readRDS(file = "INIT_DATA_080726.rds")

key_vars  <- c("WL_ID_CODE", "INIT_DATE", "INIT_STAT", "UNOS_CAND_STAT_DT", 
               "CHG_DATE", "UNOS_CAND_STAT_CD")

# Need to identify individuals who were downgraded from stat 1 to stat 2.
# We want to remove these individuals for the primary analysis, but keep them for future reference

stat1_before_2_code <- dat.0 %>% group_by(WL_ID_CODE) %>%
  mutate(
    ANYSTAT2 = as.integer(any(UNOS_CAND_STAT_CD == 2120))
  ) %>% filter(ANYSTAT2 == 1) %>%
  arrange(WL_ID_CODE, CHG_DATE, WLREG_AUDIT_ID_CODE) %>%
  group_by(WL_ID_CODE) %>%
  mutate(
    FIRST_STAT2 = match(TRUE, UNOS_CAND_STAT_CD == 2120)
  ) %>%
  filter(!is.na(FIRST_STAT2),
         row_number() < FIRST_STAT2,
         UNOS_CAND_STAT_CD == 2110) %>%
  ungroup() %>%
  select(-FIRST_STAT2) %>% pull(WL_ID_CODE) %>% unique()

write.csv(stat1_before_2_code, file = "stat1_before_2_wl_id_codes.csv")

# Just to double check that what I did was correct
# UNOS_CAND_STAT_CD 2110 should be before 2120 for all patients in this dataframe
dat.0 %>% filter(WL_ID_CODE %in% stat1_before_2_code,
                 UNOS_CAND_STAT_CD %in% c(2110, 2120)) %>% select(key_vars) %>%
  mutate(
    FIRST_STAT2 = match(TRUE, UNOS_CAND_STAT_CD == 2120)
  ) %>%
  filter(!is.na(FIRST_STAT2),
         row_number() <= FIRST_STAT2) %>%
  View()

# Filters data
dat2 <- dat.0 %>%
  arrange(WL_ID_CODE, CHG_DATE, WLREG_AUDIT_ID_CODE) %>%
  group_by(WL_ID_CODE) %>%
  mutate(
    FIRST_STAT2 = match(TRUE, UNOS_CAND_STAT_CD == 2120)
  ) %>%
  filter(!is.na(FIRST_STAT2),
         row_number() >= FIRST_STAT2) %>%
  mutate(STAT2_INIT = first(CHG_DATE)) %>%
  ungroup() %>%
  select(-FIRST_STAT2) 

length(unique(dat2$WL_ID_CODE))

# Identify those who never upgraded or downgraded
# Basically, those who remained status 2 until their death/tx/recovery/censored
dat3 <- dat2 %>% 
  group_by(WL_ID_CODE) %>%
  mutate(ALL_STAT2 = all(UNOS_CAND_STAT_CD == 2120))

# Now, we need to identify time-to-events who everyone.
# For those who upgraded or downgraded (ALL_STAT2 == FALSE), we want their first "non-stat2" date
# For those who remained stat 2 (ALL_STAT2 == TRUE), we want their last CHG_DATE

dat4 <- bind_rows(
  dat3 %>%
    filter(ALL_STAT2 == FALSE & UNOS_CAND_STAT_CD != 2120) %>%
    group_by(WL_ID_CODE) %>%
    slice(1),
  dat3 %>%
    filter(ALL_STAT2 == TRUE) %>%
    group_by(WL_ID_CODE) %>%
    slice(n())
) %>%
  ungroup()

length(unique(dat3$WL_ID_CODE)) == length(unique(dat4$WL_ID_CODE))

# dat4 should be a "single observation per row" dataset

# Somehow there are data inconsistencies and we have some patients who have an END_DATE 
# before they went to status 2. Not sure why...we'll remove them
dat4 %>% filter(END_DATE < STAT2_INIT) %>% pull(WL_ID_CODE)

dat5 <- dat4 %>%
  filter(END_DATE >= STAT2_INIT)

# Generate new cohort outcomes
dat5 <- dat5 %>%
  mutate(
    INIT_STAT2 = case_when(
      INIT_DATE == STAT2_INIT ~ 1,
      INIT_DATE != STAT2_INIT ~ 0,
    ),
    END_DATE_NEW = case_when(
      # For those who were downgraded/upgraded should be the date they were downgraded/upgraded based on CHG_DATE
      UNOS_CAND_STAT_CD != 2120 ~ CHG_DATE,
      # For those who remained status 2, should be END_DATE
      UNOS_CAND_STAT_CD == 2120 ~ END_DATE
    ),
    # Survival time is now based on days since being listed at status 2
    FTIME = END_DATE_NEW - STAT2_INIT,
    FTIME_NEW = ifelse(FTIME == 0, 0.1, FTIME),
    # Create NEW events
    FSTATUS_NEW = case_when(
      # Death/deterioration
      END_DATE_NEW == END_DATE & REM_CD %in% c(5, 8, 13) ~ 1,
      # HTx
      END_DATE_NEW == END_DATE & REM_CD %in% c(2, 3, 4, 14, 21, 22) ~ 2,
      # Recovery
      END_DATE_NEW == END_DATE & REM_CD %in% 12 ~ 3,
      # Upgraded to status 1
      END_DATE_NEW == CHG_DATE & UNOS_CAND_STAT_CD == 2110 ~ 4,
      # Downgraded to status 3, 4, 5, 6
      END_DATE_NEW == CHG_DATE & UNOS_CAND_STAT_CD %in% c(2130, 2140, 2150, 2160) ~ 5,
      # Downgraded to unknown status
      END_DATE_NEW == CHG_DATE & UNOS_CAND_STAT_CD == 2999 ~ 6,
      # Everyone else censored (should 2x check to make sure this is correctly coded)
      .default = 0
    )
  )

# Check to see why individuals were right censored
dat5 %>% filter(FSTATUS_NEW == 0) %>%
  select(WL_ID_CODE, INIT_DATE, STAT2_INIT, CHG_DATE, UNOS_CAND_STAT_CD, 
         REM_CD, END_DATE_NEW, FTIME_NEW, FSTATUS_NEW, INIT_STAT2) %>% View()
table(dat5$FSTATUS_NEW, dat5$REM_CD, useNA = "ifany")
# 9 = Other
# 16 = Candidate removed in ERROR
# NA = No event yet (administratively censored)

saveRDS(dat5, file = "analysis_data_080726.rds")

dat5 %>% filter(FSTATUS_NEW %in% c(1, 3)) %>% 
  select(WL_ID_CODE, DIAL_AFTER_LIST, DIAL_AFTER_LIST_Y)



aa <- readRDS("analysis_data.rds")
aa %>% filter(!(WL_ID_CODE %in% dat5$WL_ID_CODE)) %>%
  select(key_vars, EXCEPTION) %>% View()
