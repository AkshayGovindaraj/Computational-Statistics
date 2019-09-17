#Bayesian statistics HW3
sse_mle <- matrix(0,1000)
sse_eb <- matrix(0,1000)

for(j in c(1:1000)){

theta <- rnorm(10,5.67,0.0001)
y <- matrix(0,10)

for(i in c(1:10)){
  y[i] = rnorm(1,theta[i],0.0004)
}
y_bar = sum(y)/10

#CAlculating sum of squared errors
ss_mle <- 0
ss_eb  <- 0
for(i in c(1:10)){
  ss_mle = ss_mle + (theta[i]-y[i])^2
  ss_eb = ss_eb + (theta[i]-(y_bar/4 + 3*y[i]/4))^2
}

sse_mle[j] = ss_mle
sse_eb[j] = ss_eb

}

#Fetching density plots
d_mle <- density(sse_mle)
d_eb <- density(sse_eb)

plot(d_mle)
plot(d_eb)
