#### Computational Stats HW3 Q2
# 2 or 6 digit recognizer
library(MASS)

labels_df = read.table("label.dat", header = FALSE)
digits_df = read.table("data.dat", header = FALSE)

d <- matrix(digits_df[,29], nrow = 28, byrow = FALSE)
#image(matrix((data=d), ncol=28, nrow=28))
matrix(digits_df[,29], nrow = 28, byrow = FALSE)
image(matrix(digits_df[,29], nrow = 28, byrow = FALSE))

im.1 <-apply(matrix(digits_df[,1129], nrow = 28, byrow = FALSE),2,rev)
im.1 <- t(im.1)
image(im.1)

#Initial Guess for parameters
mew1 <- rnorm(784)
mew2 <- rnorm(784)

sigma1 <- diag(784)
sigma2 <- diag(784)

pi_gmm = 0.5
pi_gmm2 = 0.5

#Iterations
for(j in c(1:5)){

  #Initializing other variables
  pic = matrix(0.,1990,1) 
#  pic2 = matrix(0.,1990,1)               # Represents probabilities of it being a '6'
#  inv1 = solve(sigma1)
#  inv2 = solve(sigma2)
  
  #Calculate likelihood ratio
  for(i in c(1,1990)){
#    pic2[i] = pi_gmm2*exp(-  t(digits_df[,i]-mew2) %*% inv2 %*% (digits_df[,i]-mew2))
    pic[i] = pi_gmm*exp(-t(digits_df[,i]-mew1) %*% solve(sigma1) %*% (digits_df[,i]-mew1) +  t(digits_df[,i]-mew2) %*% solve(sigma2) %*% (digits_df[,i]-mew2))   
    pic[i] = (pi_gmm*pic[i])/(1-pi_gmm + pi_gmm*pic[i])
  }
    
  #Re-estimating parameters
  sigma1 = matrix(0.,784,784)
  sigma2 = matrix(0.,784,784)
  pi_gmm = 0
#  sig_pic1 = sum(pic1)
#  sig_pic2 = sum(pic2)
  
  for(i in c(1:1990)){
    sigma1 = sigma1 + (1-pic[i])*(digits_df[,i]-mew1) %*% t(digits_df[,i]-mew1)
    sigma2 = sigma2 + (pic[i])*(digits_df[,i]-mew2) %*% t(digits_df[,i]-mew2)
  }
#  sigma1 = sigma1/sig_pic1
#  sigma2 = sigma2/sig_pic2
  
  mew1 = matrix(0.,784,1)
  mew2 = matrix(0.,784,1)
  for(i in c(1:1990)){
    mew1 = mew1 + (1-pic[i])*digits_df[,i]
    mew2 = mew2 + (pic[i])*digits_df[,i]
  }
#  mew1 = mew1/sig_pic1
#  mew2 = mew2/sig_pic2
  
  for(i in c(1:1990)){
#    pi_gmm1 = pi_gmm1 + pic1[i]/1990
    pi_gmm = pi_gmm + pic[i]/1990
  }
  #Evaluate Log Likelihood
}

#Plot Image
im_tr <- apply(matrix(mew2, nrow = 28, byrow = FALSE),2,rev)
image(t(im_tr))

#Calculating results
results = matrix(0,1990,1)
for(i in c(1:1990)){
  if((labels_df[i]==2)&&(pic[i]<0.5)){
    results[i] = 1
  }
  if((labels_df[i]==6)&&(pic[i]>0.5)){
    results[i] = 1
  } 
  
}