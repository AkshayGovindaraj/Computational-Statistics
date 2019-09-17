#CDA assignment Stochastic Gradient Descent
#HW 2. Implementing multinomial logistic regression
train = read.csv('training.csv', header = FALSE)
test = read.csv('testing.csv', header = FALSE)

#Gradient Descent

#Initialize beta
beta <- matrix(0.0001, 84, 1)
lambda = 100
step_size <- 0.0001

gradbet2 <- matrix(0, 28, 1)
gradbet3 <- matrix(0, 28, 1)
gradbet4 <- matrix(0, 28, 1)

xibet2 <- 0
xibet3 <- 0
xibet4 <- 0

#Calculate xibet2 , xibet3 , xibet4
for(k in c(1:700)){
  
  i <- sample(1:198, 1)
  

  xibet2 <- beta[1]
  xibet3 <- beta[29]
  xibet4 <- beta[57]
  
  for(j in c(1:27)){
    xibet2 = xibet2 + train[i,j+1]*beta[j+1]
    xibet3 = xibet3 + train[i,j+1]*beta[j+29]
    xibet4 = xibet4 + train[i,j+1]*beta[j+57]
  }
  gamma = 1 + exp(-xibet2) + exp(-xibet3) + exp(-xibet4)
  
  gradbet2[1] = gradbet2[1] + exp(-xibet2)/gamma -lambda*abs(beta[1]) # Add regularization term
  gradbet3[1] = gradbet3[1] + exp(-xibet3)/gamma -lambda*abs(beta[29])
  gradbet4[1] = gradbet4[1] + exp(-xibet4)/gamma -lambda*abs(beta[57])
  
  if(train[i,1]==2){
    gradbet2[1] = gradbet2[1] - 1
  }
  
  if(train[i,1]==3){
    gradbet3[1] = gradbet3[1] - 1
  }
  
  if(train[i,1]==4){
    gradbet4[1] = gradbet4[1] - 1
  }
  
  for(j in c(1:27)){   #Gradient calculation. Need to add Regularization term
    gradbet2[j+1] = gradbet2[j+1] + train[i,j+1]*exp(-xibet2)/gamma -lambda*abs(beta[1+j])
    gradbet3[j+1] = gradbet3[j+1] + train[i,j+1]*exp(-xibet3)/gamma -lambda*abs(beta[29+j])
    gradbet4[j+1] = gradbet4[j+1] + train[i,j+1]*exp(-xibet4)/gamma -lambda*abs(beta[57+j])
    
    if(train[i,1]==2){
      gradbet2[j+1] = gradbet2[j+1] - train[i,j+1]
    }
    
    if(train[i,1]==3){
      gradbet3[j+1] = gradbet3[j+1] - train[i,j+1]
    }
    
    if(train[i,1]==4){
      gradbet4[j+1] = gradbet4[j+1] - train[i,j+1]
    }
    
  }
  
  #  }
  
  #Updated values of beta
  for(j in c(1:28)){
    beta[j] = beta[j] + step_size*gradbet2[j]
    beta[j+28] = beta[j+28] + step_size*gradbet3[j]
    beta[j+56] = beta[j+56] + step_size*gradbet4[j]
  }
  
  gradbet2 <- matrix(0, 28, 1)
  gradbet3 <- matrix(0, 28, 1)
  gradbet4 <- matrix(0, 28, 1)
  
}

#Predictions for test data
pred_list <- matrix(0, 324, 1)
for(i in c(1:324)){
  xibet2 <- beta[1]
  xibet3 <- beta[29]
  xibet4 <- beta[57]
  
  for(j in c(1:27)){
    xibet2 = xibet2 + test[i,j+1]*beta[j+1]
    xibet3 = xibet3 + test[i,j+1]*beta[j+29]
    xibet4 = xibet4 + test[i,j+1]*beta[j+57]
  }
  
  gamma = 1 + exp(-xibet2) + exp(-xibet3) + exp(-xibet4)
  
  py1 = 1/gamma
  py2 = exp(-xibet2)/gamma
  py3 = exp(-xibet3)/gamma
  py4 = exp(-xibet4)/gamma
  
  max_pr = max(py1,py2,py3,py4)
  
  if(max_pr==py1) pred_list[i] = 1
  if(max_pr==py2) pred_list[i] = 2
  if(max_pr==py3) pred_list[i] = 3
  if(max_pr==py4) pred_list[i] = 4
}

#Calculating the error
error <- 0
for(i in c(1:324)){
  if(test[i,1]!=pred_list[i]) error = error + 1
}
pred_accuracy = 1 - error/324