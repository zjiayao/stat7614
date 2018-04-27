library(dplyr)
library(recipes)
raw = read.csv(file = "/Users/l-c/Desktop/STAT3612-master/stat7614/dataset/gradcafe/cs_arw_reduced.csv", header = TRUE) %>% data.frame()
raw$id <- NULL
raw$university <- NULL
raw$season <- NULL
raw$gre = raw$gre_verbal+raw$gre_quant+raw$gre_writing
raw$gre_verbal = NULL
raw$gre_quant = NULL
raw$gre_writing <- NULL
raw$post_data <- NULL
raw$post_timestamp <- NULL
raw$decision_month <- as.Date.POSIXct(raw$decision_timestamp) %>% format(format="%m") %>%
  as.factor()
raw$decision_timestamp <- NULL
raw$decision_date <- NULL
raw$decision_method[which(raw$decision_method=="Postal Service")] <- "Other"
raw$decision_month = as.numeric(raw$decision_month )
raw$degree[which(raw$degree!= "PhD")] <- "MS"
raw$major = NULL
raw$decision = ifelse(raw$decision== "Accepted", 1, 0)
raw$decision = as.factor(raw$decision)
train_index <- sample(1:nrow(raw), 0.8 * nrow(raw))
test_index <- setdiff(1:nrow(raw), train_index)

x_train <- raw[train_index,]
x_train$decision = NULL
y_train <- data.frame(raw[train_index,]$decision) 
colnames(y_train) =c('decision')
x_test <- raw[test_index,]
x_test$decision <- NULL
y_test <- data.frame(raw[test_index,]$decision) 
colnames(y_test) =c('decision')
# accepted == 1


write.csv(x_train,
          row.names = FALSE,
          file = "/Users/l-c/Desktop/STAT3612-master/stat7614/dataset/gradcafe/train_x.csv")
write.csv(x_test, 
          row.names = FALSE,
          file = "/Users/l-c/Desktop/STAT3612-master/stat7614/dataset/gradcafe/test_x.csv")
write.csv(y_train, 
          row.names = FALSE,
          file = "/Users/l-c/Desktop/STAT3612-master/stat7614/dataset/gradcafe/train_y.csv")
write.csv(y_test, 
          row.names = FALSE,
          file = "/Users/l-c/Desktop/STAT3612-master/stat7614/dataset/gradcafe/test_y.csv")



rcp <- recipe(~., data=x_train) %>%
  #step_bs(all_numeric())%>%
  step_meanimpute(all_numeric()) %>%
  #step_ns(all_numeric(), df=3) %>%
  #step_center(all_numeric()) %>%
  #step_scale(all_numeric()) %>%
  #step_pca(all_numeric(), threshold=0.9) %>%
  step_dummy(all_nominal()) %>%
  step_interact(terms=~contains("status"):contains("uni_pub") + 
                  contains("gpa"):contains("gre") + 
                  contains("gre"):contains("degree")
                  ) %>%
  prep(training=x_train)

x.train.bin <- bake(rcp, newdata=x_train) %>% as.data.frame()
train.bin = cbind(y_train, x.train.bin)
x.test.bin <- bake(rcp, newdata = x_test) %>% as.data.frame()

library(corrplot)

x_train_nu = select(x_train, which(sapply(x_train, is.numeric))) %>% na.omit()
xcor = cor(x_train_nu)
corrplot(xcor)
