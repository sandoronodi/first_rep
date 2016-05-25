sapply(c("ROCR", "randomForest"), require, character.only = TRUE)


train <- read.csv("train.csv")
test <- read.csv("test.csv")

levels(test$AGE_CAT) <- c(levels(test$AGE_CAT), "-")
levels(train$AGE_CAT) <- c(levels(train$AGE_CAT), "-")
levels(test$MAX_CHANNEL) <- c(levels(test$MAX_CHANNEL), "b")
levels(test$MAX_MC_CAT) <- c(levels(test$MAX_MC_CAT), "b")
levels(train$MAX_MC_CAT) <- c(levels(train$MAX_MC_CAT), "b")

train$GEN <- factor(train$GEN)
test$GEN <- factor(test$GEN)
train$W201406 <- factor(train$W201406)
test$W201506 <- factor(test$W201506)
train$SUM_TRANS_NULL_F <- factor(train$SUM_TRANS_NULL_F)
test$SUM_TRANS_NULL_F <- factor(test$SUM_TRANS_NULL_F)

train <- train[-c(1)]
train$classLabel <- factor(train$classLabel)

#technical column name switch
test$W201406 <- test$W201506
test$W201506 <- NULL

summary(train)
summary(test)

####################x

# t1.prior <- rpart(formula = classLabel ~ .,
#                   data = train,
#                   method = "class",
#                   control = rpart.control(cp = 0.000001),
#                   parms = list(split = "gini"))
# 
# ##PRUNING with minimizing X-err
# t1.prior.cp <- t1.prior$cptable[which.min(t1.prior$cptable[, "xerror"]), "CP"]
# t1.prior.prune <- prune(t1.prior, t1.prior.cp)
# t1.ppp <- predict(t1.prior.prune, test, type = "prob")
# 
# t1.pred <- prediction(t1.ppp[,2], test$classLabel)

###########################xxxx

set.seed(123)
rf <- randomForest(formula = classLabel ~ .,
                   data = train,
                   importance = TRUE, 
                   ntree = 800,
                   mtry = 6)

varImpPlot(rf)

pred <- predict(rf, test, type = "prob")

submission <- data.frame(test$USER_ID)
submission$USER_ID <- submission$test.USER_ID
submission$test.USER_ID <- NULL
submission$SCORE <- pred[,2]

write.csv(submission, "submission_002.csv", row.names = FALSE)


#############################################################################################################
######################################################################################
######################################################################
smp_size <- floor(0.75 * nrow(train))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

#train <- train[train_ind, ]
test <- train[-train_ind, ]



pred <- predict(rf, test, type = "prob")
rf.pred <- prediction(pred[,2], test$classLabel)

#AUC
as.numeric(performance(rf.pred, "auc")@y.values)

#ACCURACY
conf <- table(test$classLabel, predict(rf, test, type = "class"))
sum(diag(conf))/sum(conf)
