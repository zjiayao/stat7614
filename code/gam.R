train.x <- train_x
test.x <- text_x
train.y <- train_y
test.y <- text_y

# getting rid of rows contaning na
train <- cbind(train.y, train.x) %>% na.omit()
test <- cbind(train.y, train.x) %>% na.omit()

train.x <- train[,-1]
train.y <- train[,1]
test.x <- test[,-1]
test.y <- test[,1]

# logistic regression
library(MASS)
model1.1 <- glm(decision~1, data=train, family="binomial")
model1.2 <- glm(decision~., data=train, family="binomial")
model1.3 <- stepAIC(model1.1, direction="forward", scope=list(upper=model1.2, lower=model1.1))
summary(model1.3)
model1.4 <- glm(decision~.+degree:status+degree:gpa+degree:gre, data=train, family="binomial")
summary(model1.4)
model1.5 <- glm(decision~.+degree:status, data=train, family="binomial")
summary(model1.5)
tikz('~/logistic.tex', width = 8, height = 6)
par(mfrow=c(2,2))
plot(model1.5, pch=19, cex=0.5)
dev.off()
library(pROC)
pred1 <- predict.glm(model1.5, newdata=test.x, type="response")
auc1 <- roc(test.y, pred1)$auc

# lasso
library(glmnet)
dummies <- model.matrix(decision~.+degree:status, data=train)
model2 <- cv.glmnet(x=dummies[,-1], y=train.y, family="binomial", alpha=1, nfolds=5)
tikz('~/lasso.tex', width = 4, height = 8)
par(mfrow=c(1,2))
plot(model2)
plot(model2$glmnet.fit)
dev.off()
dummies <- model.matrix(decision~.+degree:status, data=test)
pred2 <- predict.cv.glmnet(model2, newx=dummies[,-1], s="lambda.1se", type="response")
auc2 <- roc(test.y, as.numeric(pred2))$auc

# gam
library(splines)
library(mgcv)
model3.1 <- gam(decision~ns(gpa, df=3)+ns(gre,df=3)+
                  ns(uni_faculty, df=3)+ns(uni_pub, df=3)+
                  degree+status+degree:status+
                  decision_method+decision_month,
                  data=train, family="binomial")
summary(model3.1)
model3.2 <- gam(decision~lo(gpa, span=0.2)+lo(gre, span=0.2)+
                  lo(uni_faculty, span=0.2)+lo(uni_pub, span=0.2)+
                  degree+status+degree:status+
                  decision_method+decision_month, 
                data=train, family="binomial")
summary(model3.2)
AIC(model3.1, model3.2)
pred3 <- predict.gam(model3.1, newdata=test.x, type="response")
auc3 <- roc(test.y, pred3)$auc
