sapply(c("rpart", "rattle", "rpart.plot", "RColorBrewer", "ROCR", "caret", "randomForest"), require, character.only = TRUE)

train_base<- read.csv("train_base.csv")
train <- train_base

train$X <- NULL
train$USER_ID <- NULL

train$GEN <- factor(train$GEN)
train$classLabel <- factor(train$classLabel)
train$W201406_F <- factor(train$W201406_F)

smp_size <- floor(0.75 * nrow(train))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

train <- train[train_ind, ]
test <- train[-train_ind, ]

train_base <- data.table(train_base)
train_base[, .(n = .N, prop_Y = mean(classLabel)), by = .(MAX_MC_CAT)]


####################x

t1.prior <- rpart(formula = classLabel ~ .,
                  data = train,
                  method = "class",
                  control = rpart.control(minbucket = 100, cp = 0.000001),
                  parms = list(split = "gini"))

##PRUNING with minimizing X-err
t1.prior.cp <- t1.prior$cptable[which.min(t1.prior$cptable[, "xerror"]), "CP"]
t1.prior.prune <- prune(t1.prior, t1.prior.cp)
t1.ppp <- predict(t1.prior.prune, test, type = "prob")

t1.pred <- prediction(t1.ppp[,2], test$classLabel)

###########################xxxx


rf <- randomForest(formula = classLabel ~ . -W201406_F,
                   data = train,
                   #importance = TRUE, 
                   ntree = 150)

#varImpPlot(rf)

pred <- predict(rf, test, type = "prob")
rf.pred <- prediction(pred[,2], test$classLabel)

#AUC
as.numeric(performance(rf.pred, "auc")@y.values)

#ACCURACY
conf <- table(test$classLabel, predict(rf, test, type = "class"))
sum(diag(conf))/sum(conf)
