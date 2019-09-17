#CDA assignment 2 
#Decision Tree
library(rpart)
library(rpart.plot)

train = read.csv('training.csv', header = FALSE)
test = read.csv('testing.csv', header = FALSE)

fit <- rpart(V1~., data = train, method = 'class')
rpart.plot(fit, extra = 106)

predict_unseen <-predict(fit, test, type = 'class')

table_mat <- table(test$V1, predict_unseen)

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
