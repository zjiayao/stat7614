###########
# XGBoost #
###########

if(!require(xgboost)) install.packages("xgboost")
if(!require(ggplot2)) install.packages("ggplot2")

library(xgboost)
library(Matrix)
library(ggplot2)
library(caret)


# parameter tuning
# package Caret also provides a systematic framework for tuning
paramGrid <- expand.grid(
  eta=c(0.005),
  max_depth=c(9), 
  subsample=0.6,
  colsample_bytree= 0.9 # randomForest
)
#best_round = 202
best_param <- list()
best_auc <- 0
best_round <- 0
for (i in 1:nrow(paramGrid)){
  current_param <- as.list(paramGrid[i,])
  history <- xgb.cv(
    data=as.matrix(x.train.bin),
    label=as.numeric(y_train$decision)-1,
    params=current_param,
    nround=2000,
    verbose=0,
    nfold=5,
    # ---XGBoost documentation---
    # validation error needs to decrease at least every
    # early_stopping_rounds to continue training
    early_stopping_rounds=50,
    eval_metric="auc",
    objective="binary:logistic",
    booster="gbtree"
  )
  current_round <- history$best_iteration
  current_auc <- history$evaluation_log$test_auc_mean[current_round]
  if(current_auc > best_auc){
    best_param <- current_param
    best_auc <- current_auc
    best_round <- current_round
  }
  # make verbose
  print(paste("Round ", i, " completed", sep=""))
}
xgb <- xgboost(
  data=as.matrix(x.train.bin),
  label=as.matrix(y_train$decision), 
  params=best_param,
  nround=best_round,
  verbose=0,
  nfold=5,
  eval_metric="auc",
  objective="binary:logistic",
  booster="gbtree"
)


imp.matrix <- xgb.importance(feature_names=colnames(x.train.bin), model=xgb)
#xgb.ggplot.importance(imp.matrix)
xgb.plot.importance(imp.matrix, main ="xgBoost Importance")

# predict
pred.y.xgb <- predict(xgb, newdata=as.matrix(x.test.bin))
test.y.xgb <- data.frame( Desision=pred.y.xgb)
pred.xgb=prediction(pred.y.xgb,y_test)
perf_AUC.xgb=performance(pred.xgb,"auc")
AUC.xgb=perf_AUC.xgb@y.values[[1]] 

#[1] 0.8542404

# for usage of caret
xgbGrid <- expand.grid(
  eta=c(0.005),
  max_depth=c(9), 
  subsample=0.6,
  colsample_bytree= 0.9, # randomForest
  gamma=0,
  min_child_weight=1,
  nrounds=1000
)
fitControl <- trainControl(
  method="cv",
  number=5,
  classProbs=TRUE,
  summaryFunction=twoClassSummary,
  verboseIter=TRUE
)
xgb <- train(
  x=x.train.bin,
  y=y_train$decision,  
  method="xgbTree",
  trControl=fitControl,
  tuneGrid=xgbGrid,
  metric="ROC"
)
plot(varImp(xgb), main="XGBoost")
ggplot(varImp(xgb, scale = TRUE), main = "XGBoost") +
  ggtitle("XGBoost")