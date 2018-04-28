if(!require(caret)) install.packages("caret")
if(!require(ranger)) install.packages("ranger")
if(!require(e1071)) install.packages("e1071")
if(!require(ggplot2)) install.packages("ggplot2")

library(caret)
library(ranger)
library(e1071)
library(ggplot2)

# set tuning parameters
rfGrid <- expand.grid(
  mtry=c(9),
  splitrule=c("extratrees"),
  min.node.size= c(6)
)

# set validation mode
fitControl <- trainControl(
  method="cv",
  number=5,
  classProbs=TRUE,
  summaryFunction=twoClassSummary,
  verboseIter=TRUE
)

levels(y_train$decision) = c("rejected", "accepted")

rf <- train(
  x= x.train.bin,
  y=y_train$decision,
  method="ranger",
  tuneGrid=rfGrid,
  trControl=fitControl,
  importance="impurity",
  metric="ROC"
)
plot(rf)

#MAX_ROC = 0.8380577
# plot variable importance
ggplot(varImp(rf, scale = FALSE), main = "randomForest") +
  ggtitle("Ranger")

#prediction and AUC of test data
library(ROCR)
pred.y.rf <- predict(rf, newdata=as.matrix(x.test.bin)) %>% as.numeric()
pred.y.rf <- pred.y.rf -1
test.y.rf <- data.frame(Desision=pred.y.rf )
pred.rf=prediction(pred.y.rf,y_test)
perf_AUC.rf=performance(pred.rf,"auc")
AUC.rf=perf_AUC.rf@y.values[[1]] 