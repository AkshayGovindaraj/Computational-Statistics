#CDA assignment 2 
#KNN Classification
library(class)
train = read.csv('training.csv', header = FALSE)
test = read.csv('testing.csv', header = FALSE)

K = 1
class_test = matrix(0,325,1)

for(i in c(1:325)){
  distance = matrix(0,198,1)
  for(j in c(1:198)){
    for(k in c(2:28)){
      distance[j] = distance[j] + (test[i,k]-train[j,k])^2
    }
  }
  class_test[i] = train[which.min(distance),1]
}

error <- 0
for(i in c(1:324)){
  if(test[i,1]!=class_test[i]) error = error + 1
}
pred_accuracy = 1 - error/324

#head(train)

train2 = subset(train, select = -V1)
test2 = subset(test, select = -V1)

prc_test_pred <- knn(train = train2, test = test2,cl = train$V1, k=7)

error <- 0
for(i in c(1:324)){
  if(test[i,1]!=prc_test_pred[i]) error = error + 1
}
pred_accuracy = 1 - error/324
