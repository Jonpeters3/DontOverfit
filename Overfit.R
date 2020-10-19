library("tidyverse")
library("caret")
library("fakeR")

train <- read_csv("train.csv")
test <- read_csv("test.csv")

train1 <- train %>% filter(target == 1)
train0 <- train %>% filter(target == 0)

A <- function(x) sample(min(x):max(x), size = 2000, replace = TRUE)

train1 <- as.data.frame(apply(train1[,3:302 ], 2, A))
train1$target <- 1
train1$id <- 250

train0 <- as.data.frame(apply(train0[,3:302 ], 2, A))
train0$target <- 0
train0$id <- 251

#train <- rbind(train, train1, train0)

#train <- train[ sample(nrow(train)) ,]

train$target <- ifelse(train$target == 1, 'yes', 'no')
train$target <- as.factor(train$target)

tune.grid = expand.grid(n.trees = 250,
                        interaction.depth = 3,
                        shrinkage = .1,
                        n.minobsinnode = 20)

tr.grid <-trainControl(method="repeatedcv",
                       classProbs = TRUE,
                       number=10,
                       repeats = 3,
                       summaryFunction = prSummary)#,
#sampling = "smote")

boost <- train(form=target~., 
               data=(train %>% select(-id)),
               method = "gbm",
               trControl=tr.grid,
               #preProc = c("center", "scale", "pca"),
               metric = "F",
               tuneGrid = tune.grid,
               verbose = FALSE
)

tune.grid = expand.grid(cost = seq(.01, .1, .01),
                        Loss = "L2",
                        weight = 2)

tr.grid <-trainControl(method="repeatedcv",
                       classProbs = TRUE,
                       number=10,
                       repeats = 3,
                       summaryFunction = prSummary)#,
#sampling = "smote")

boost1 <- train(form=target~., 
                data=(train %>% select(-id)),
                method = "svmLinearWeights2",
                trControl=tr.grid,
                #preProc = c("center", "scale", "pca"),
                metric = "F",
                tuneGrid = tune.grid,
                verbose = FALSE
)

tune.grid = expand.grid(kmax = 11,
                        distance = 2,
                        kernel = "optimal")

tr.grid <-trainControl(method="repeatedcv",
                       classProbs = TRUE,
                       number=10,
                       repeats = 3,
                       summaryFunction = prSummary)#,
#sampling = "smote")

boost2 <- train(form=target~., 
                data=(train %>% select(-id)),
                method = "kknn",
                trControl=tr.grid,
                #preProc = c("center", "scale", "pca"),
                metric = "F",
                tuneGrid = tune.grid,
                verbose = FALSE
)

tune.grid = expand.grid(nIter = seq(30, 50, 1))

tr.grid <-trainControl(method="repeatedcv",
                       classProbs = TRUE,
                       number=10,
                       repeats = 3,
                       summaryFunction = prSummary)#,
#sampling = "smote")

boost3 <- train(form=target~., 
                data=(train %>% select(-id)),
                method = "LogitBoost",
                trControl=tr.grid,
                #preProc = c("center", "scale", "pca"),
                metric = "F",
                tuneGrid = tune.grid,
                verbose = FALSE
)

tune.grid = expand.grid(laplace = 0,
                        usekernel = TRUE,
                        adjust = 1)

tr.grid <-trainControl(method="repeatedcv",
                       classProbs = TRUE,
                       number=10,
                       repeats = 3,
                       summaryFunction = prSummary)#,
#sampling = "smote")

boost4 <- train(form=target~., 
                data=(train %>% select(-id)),
                method = "naive_bayes",
                trControl=tr.grid,
                #preProc = c("center", "scale", "pca"),
                metric = "F",
                #tuneGrid = tune.grid,
                verbose = FALSE
)

final <- data.frame(id=test$id)

preds <- (predict(boost, newdata = test, type="prob")[2])

final$gmb <- preds$yes


preds <- (predict(boost2, newdata = test, type="prob")[2])

final$kknn <- preds$yes

preds <- (predict(boost3, newdata = test, type="prob")[2])

final$lr <- preds$yes


preds <- (predict(boost4, newdata = test, type="prob")[2])

final$nb <- preds$yes



final <- final %>% mutate(target = (gmb + lr)/2 ) %>%
  select(-gmb, -lr, -nb, -kknn)



write_csv(x=final, path="./Peters_sub.csv")

beepr::beep()