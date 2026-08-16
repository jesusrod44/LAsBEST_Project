test <- readRDS("data/analysis_data_080726.rds")
heart3 <- read.csv("data/heart3.csv")

setdiff(heart3$WL_ID_CODE, test$WL_ID_CODE)  # patients in heart3 but not test
setdiff(test$WL_ID_CODE, heart3$WL_ID_CODE)  # patients in test but not heart3

table(heart3$FSTATUS_NEW)
table(test$FSTATUS_NEW)

table(heart3$group)
table(test$INIT_STAT2)
