#House prices - Variable Selection
library(glmnet)
houseprices = read.csv("RealEstate.csv")

#### Ridge Regression

#Short Sale case
ssale <- houseprices[ which(houseprices$Status == 'Short Sale'), ]

#Using GLM - for short sale
x <- model.matrix(Price ~., ssale)[,-1]
y <- ssale$Price
lambda <- 10^seq(10, -2, length = 100)

#Test train split
set.seed(100)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
cv.out_ssale_ridge <- cv.glmnet(x[train,], y[train], alpha = 0)

predict(ridge.mod, s = cv.out_ssale_ridge$lambda.min,  type = 'coefficients')

#Foreclosure case
fsale <- houseprices[ which(houseprices$Status == 'Foreclosure'), ]

#Using GLM - for short sale
x <- model.matrix(Price ~., fsale)[,-1]
y <- fsale$Price
lambda <- 10^seq(10, -2, length = 100)

#Test train split
set.seed(100)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

ridge.mod2 <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
cv.out_fsale_ridge <- cv.glmnet(x[train,], y[train], alpha = 0)

#Regular case
rsale <- houseprices[ which(houseprices$Status == 'Regular'), ]

#Using GLM - for short sale
x <- model.matrix(Price ~., rsale)[,-1]
y <- rsale$Price
lambda <- 10^seq(10, -2, length = 100)

#Test train split
set.seed(100)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

ridge.mod3 <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
cv.out_rsale_ridge <- cv.glmnet(x[train,], y[train], alpha = 0)




#### Lasso Regression

#Short Sale case
ssale <- houseprices[ which(houseprices$Status == 'Short Sale'), ]

#Using GLM - for short sale
x <- model.matrix(Price ~., ssale)[,-1]
y <- ssale$Price
lambda <- 10^seq(10, -2, length = 100)

#Test train split
set.seed(100)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
cv.out_ssale <- cv.glmnet(x[train,], y[train], alpha = 1)

predict(lasso.mod, s = cv.out_ssale$lambda.min,  type = 'coefficients')

#Foreclosure case
fsale <- houseprices[ which(houseprices$Status == 'Foreclosure'), ]

#Using GLM - for short sale
x <- model.matrix(Price ~., fsale)[,-1]
y <- fsale$Price
lambda <- 10^seq(10, -2, length = 100)

#Test train split
set.seed(100)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

lasso.mod2 <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
cv.out_fsale <- cv.glmnet(x[train,], y[train], alpha = 1)

#Regular case
rsale <- houseprices[ which(houseprices$Status == 'Regular'), ]

#Using GLM - for short sale
x <- model.matrix(Price ~., rsale)[,-1]
y <- rsale$Price
lambda <- 10^seq(10, -2, length = 100)

#Test train split
set.seed(100)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

lasso.mod3 <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
cv.out_rsale <- cv.glmnet(x[train,], y[train], alpha = 1)

predict(lasso.mod, s = cv.out_rsale$lambda.min,  type = 'coefficients')


#Predictions


ridge.pred <- predict(ridge.mod, s = cv.out_rsale_ridge$lambda.min, newx = x[test,])
lasso.pred <- predict(lasso.mod, s = 1000000000, newx = x[test,])

predict(ridge.mod, s = 100000,  type = 'coefficients')
predict(ridge.mod2, s = 100000,  type = 'coefficients')
predict(ridge.mod3, s = 300000,  type = 'coefficients')

predict(lasso.mod, s = 100000,  type = 'coefficients')
predict(lasso.mod2, s = 100000,  type = 'coefficients')
predict(lasso.mod3, s = 300000,  type = 'coefficients')

mse_ridge <- mean((ridge.pred-ytest)^2)
mse_lasso <- mean((lasso.pred-ytest)^2)
