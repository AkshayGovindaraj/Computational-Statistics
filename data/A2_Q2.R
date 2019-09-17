# Computational Stats Assignment 2 Question 2 
# Logistic Regression
x_data <- read.delim("logit-x.dat", header = FALSE, sep="")
y_data <- read.delim("logit-y.dat", header = FALSE, sep="\t")
total <- merge(y_data,x_data, by=c("V1","V2"))

#Logistic Regression
model <- glm(y_data[1]$V1~x_data$V1 + x_data$V2,family=binomial(link='logit'))
summary(model)

#Manually implementing gradient descent
library(LaplacesDemon)

#Cost Function
cost_logit <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y) ) #*(exp(X*X/20)))
}

# learning rate and iteration limit
alpha <- 0.01
num_iters <- 1000

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(0,0,0), nrow=3)

# add a column of 1's for the intercept coefficient
X <- cbind(1, x_data)
X <- data.matrix(X, rownames.force = NA)
y <- data.matrix(y_data, rownames.force = NA)

# gradient descent
for (i in 1:num_iters) {
  error <- (invlogit(X %*% theta) - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost_logit(X, y, theta)
  theta_history[[i]] <- theta
}

print(theta)

plot(cost_history, type='line', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')
