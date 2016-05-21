sapply(c("data.table", "ggplot2"), require, character.only = TRUE)

users_2014 <- read.csv("users_2014.csv")
aggr_2014 <- read.csv("aggr_2014.csv")


users_2014 <- data.table(users_2014)
users_2014$TARGET_TASK_2 <- as.Date(users_2014$TARGET_TASK_2, format = "%Y.%m.%d")
users_2014$classLabel <- factor(ifelse((is.na(users_2014$TARGET_TASK_2) == TRUE), 0, ifelse(users_2014$TARGET_TASK_2 > as.Date("2014-06-30"), 1, 0)), levels = c(0, 1))

users_2014 <- users_2014[TARGET_TASK_2 < as.Date("2015-01-01") | is.na(TARGET_TASK_2) == TRUE,]



