sapply(c("data.table", "ggplot2", "stringi"), require, character.only = TRUE)

# feature engineering of users_2014
users_2014 <- data.table(read.csv("users_2014.csv"))

setkey(users_2014, USER_ID)
users_2014$GEN <- factor(users_2014$GEN, levels = c(0,1))
users_2014$TARGET_TASK_2 <- as.Date(users_2014$TARGET_TASK_2, format = "%Y.%m.%d")
users_2014$classLabel <- as.numeric(ifelse((is.na(users_2014$TARGET_TASK_2) == TRUE), 0, ifelse(users_2014$TARGET_TASK_2 > as.Date("2014-06-30"), 1, 0)))

users_2014 <- users_2014[TARGET_TASK_2 < as.Date("2015-01-01") | is.na(TARGET_TASK_2) == TRUE,]
users_2014 <- users_2014[C201406 != 1,]
users_2014$W201406_F <- factor(ifelse(users_2014$W201406 == 1, 1, 0), levels = c(0,1))

users_2014 <- data.frame(users_2014)
users_2014 <- data.table(users_2014[-c(6:8, 9:32)])

users_2014[, .(n = .N, prop_Y = mean(classLabel)), by = .(W201406_F)]


#reading aggr_2014

aggr_2014 <- data.table(read.csv("aggr_cats.csv"))
aggr_2014$X <- NULL

# merging
setkey(aggr_2014, USER_ID)
aggr_2014 <- data.frame(aggr_2014)
users_2014 <- data.frame(users_2014)
train <- merge(users_2014, aggr_2014, all.x = TRUE)


var <- names(train[8:14])
for (i in var) {
  levels(train[[i]]) <- c(levels(train[[i]]), "xx")
  train[is.na(train[[i]]) == TRUE, ][[i]]  <- "xx"
}

summary(users_2014)
summary(aggr_2014)
summary(train)

write.csv(train_base, "train_base.csv")


