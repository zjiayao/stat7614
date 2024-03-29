library(dplyr)
raw = read.csv(url("https://i.cs.hku.hk/~jyzhang/misc/cs_reduced.csv"),
	       header = TRUE) %>% data.frame()
raw$id <- NULL
raw$university <- NULL
raw$season <- NULL
raw$gre = raw$gre_verbal+raw$gre_quant+raw$gre_writing
raw$gre_verbal = NULL
raw$gre_quant = NULL
raw$gre_writing <- NULL
raw$post_data <- NULL
raw$post_timestamp <- NULL
raw$decision_month <- as.Date.POSIXct(raw$decision_timestamp)
    %>% format(format="%m") %>% as.factor()
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
