install.packages("data.table")
library(data.table)

  train_2014 <- read.csv(file, header = TRUE)
  train_2014$DATE <- as.Date(train_2014$DATE, format = "%Y-%m-%d")
  train_2014 <- train_2014[train_2014$DATE < as.Date("2014-07-01"),]

  train_2014$wd <- weekdays(train_2014$DATE)
  train_2014$month <- months(train_2014$DATE)
  
  train_2014 <- data.table(train_2014)
  
  dummy <- with(train_2014, data.table(model.matrix(~CHANNEL + 0), 
                                       model.matrix(~TIME_CAT + 0), 
                                       model.matrix(~LOC_CAT + 0),
                                       model.matrix(~MC_CAT + 0),
                                       model.matrix(~CARD_CAT + 0),
                                       model.matrix(~AMT_CAT + 0),
                                       model.matrix(~wd + 0),
                                       model.matrix(~month + 0),
                                       USER_ID))
  
  aggr_2014 <- dummy[, lapply(.SD, sum), by = USER_ID]
  
  write.csv(aggr_2014, "aggr_2014.csv")
  
