# Computational Stats Assignment 2 Question 3
# Locally weighted linear regression
x_data <- read.delim("rx.dat", header = FALSE, sep="")
y_data <- read.delim("ry.dat", header = FALSE, sep="")
df = data.frame(y_data, x_data)
colnames(df) <- c("y", "x")

#Implementing a simple linear model
model_simple <- lm(y~x, data=df)
summary(model_simple)

#Plotting the data and line
plot(df$x, df$y)
abline(model_simple)

#Implementing a weighted linear model
model_weight <- lm(y~x, data=df, weights=exp((-df$x^2)/20))
summary(model_weight)

#Plotting the data and line
plot(df$x, df$y)
abline(model_weight)

### Solving simple least squares using gradient descent

#Cost Function
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y) ) #*(exp(X*X/20)))
}

cost_weight <- function(X, y, theta) {
  sum( t(X %*% theta - y) %*% w %*% (X %*% theta - y) ) / (2*length(y) ) #*(exp(X*X/20)))
}

# learning rate and iteration limit
alpha <- 0.00005
num_iters <- 1000

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(0,0), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, x_data)
X <- data.matrix(X, rownames.force = NA)
y <- data.matrix(y_data, rownames.force = NA)

#Weight matrix
w = X %*% t(X)
w = diag(w)
w = diag(w)

# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost_weight(X, y, theta)
  theta_history[[i]] <- theta
}

print(theta)

plot(cost_history, type='line', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')
