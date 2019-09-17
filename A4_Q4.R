#Nonlinear regression with spline
copper = read.table('copper-new.txt')

#Linear model
linear_model <- lm(V1~V2, data=copper)
summary(linear_model)

#Nonlinear regression with spline function
fit<-lm(V1 ~ bs(V2,knots = c(100,200,450)),data = copper )
summary(fit)

plot(copper$V2, copper$V1)

#Plotting the Regression Line to the scatterplot   
copperlims<-range(copper$V2)
copper.grid<-seq(from=copperlims[1], to = copperlims[2])

plot(copper$V2,copper$V1,col="grey",xlab="Temperature",ylab="Y")
points(copper.grid,predict(fit,newdata = list(V2=copper.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(100,200,450),lty=2,col="darkgreen")

predict(linear_model, newdata = list(V2=400))
predict(fit, newdata = list(V2=400))