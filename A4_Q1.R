#Computational Statistics HW3 Question 1 Digit recognizer with PCA
library(MASS)
labels_df = read.table("label.dat", header = FALSE)
digits_df = read.table("data.dat", header = FALSE)

sv <- svd(digits_df)
d <- sv$d
u <- sv$u
v <- sv$v

for(j in c(1:5)){
im.2 <-apply(matrix(u[,j], nrow = 28, byrow = FALSE),2,rev)
im.2 <- t(im.2)
image(im.2)
}

#part b: Getting the compressed data
u_prj <- u[,1:50]
d_prj <- d[1:50]
v_prj <- v[,1:50]
compressed.image<- (u[,1:50] %*% diag(d[1:50]) %*% t(v[,1:50]))
compressed.image<- (u_prj %*% diag(d_prj) %*% t(v_prj))

components = t(matrix(u[,1])) %*% matrix(digits_df[1]$V1)

#part c

#Calculating no. of 2s
count_2 = 1032

pi2 = count_2/1990
pi6 = (1990-count_2)/1990

mean1 = t(u[,1:50]) %*% matrix(digits_df[,1])       #Only the first component
mean2 = t(u[,1:50]) %*% matrix(digits_df[,1033])

for(i in c(2:800)){
  mean1 = mean1 + t(u[,1:50]) %*% matrix(digits_df[,i])
}
mean1 = mean1/800

for(i in c(1034:1732)){
  mean2 = mean2 + t(u[,1:50]) %*% matrix(digits_df[,i])
}
mean2 = mean2/700

covariance = (mean1 - t(u[,1:50]) %*% matrix(digits_df[,1])) %*% t((mean1 - t(u[,1:50]) %*% matrix(digits_df[,1])))

for(i in c(2:800)){
  covariance = covariance + (mean1 - t(u[,1:50]) %*% matrix(digits_df[,i])) %*% t((mean1 - t(u[,1:50]) %*% matrix(digits_df[,i])))
}

for(i in c(1033:1732)){
  covariance = covariance + (mean2 - t(u[,1:50]) %*% matrix(digits_df[,i])) %*% t((mean2 - t(u[,1:50]) %*% matrix(digits_df[,i])))
}

covariance = covariance/1498

sv_sigma <- svd(covariance)
d_sigma <- sv_sigma$d
u_sigma <- sv_sigma$u
v_sigma <- sv_sigma$v

d_root = solve(diag(d_sigma) %*% diag(d_sigma))

#Performing LDA

classification = matrix(0.,232 + 258,1) 

for(i in c(801:1032)){
  del1 = t(d_root %*% t(u_sigma) %*% ( mean1 - t(u[,1:50]) %*% matrix(digits_df[,i]) )) %*% (d_root %*% t(u_sigma) %*% ( mean1 - t(u[,1:50]) %*% matrix(digits_df[,i]) ))
  del2 = t(d_root %*% t(u_sigma) %*% ( mean2 - t(u[,1:50]) %*% matrix(digits_df[,i]) )) %*% (d_root %*% t(u_sigma) %*% ( mean2 - t(u[,1:50]) %*% matrix(digits_df[,i]) ))
  del1 = del1/2 - log(8/15)
  del2 = del2/2 - log(7/15)
  
  if(del1>del2){
    classification[i-800] = 6
  }
  
  if(del1<del2){
    classification[i-800] = 2
  }
}

for(i in c(1733:1990)){
  del1 = t(d_root %*% t(u_sigma) %*% ( mean1 - t(u[,1:50]) %*% matrix(digits_df[,i]) )) %*% (d_root %*% t(u_sigma) %*% ( mean1 - t(u[,1:50]) %*% matrix(digits_df[,i]) ))
  del2 = t(d_root %*% t(u_sigma) %*% ( mean2 - t(u[,1:50]) %*% matrix(digits_df[,i]) )) %*% (d_root %*% t(u_sigma) %*% ( mean2 - t(u[,1:50]) %*% matrix(digits_df[,i]) ))
  del1 = del1/2 - log(8/15)
  del2 = del2/2 - log(7/15)
  
  if(del1>del2){
    classification[i-1500] = 6
  }
  
  if(del1<del2){
    classification[i-1500] = 2
  }
}

miss_rate = 1 - (sum(classification[1:232]==2) + sum(classification[233:490]==6))/490

#Performing QDA

covariance1 = (mean1 - t(u[,1:50]) %*% matrix(digits_df[,1])) %*% t((mean1 - t(u[,1:50]) %*% matrix(digits_df[,1])))
covariance2 = (mean2 - t(u[,1:50]) %*% matrix(digits_df[,1033])) %*% t((mean2 - t(u[,1:50]) %*% matrix(digits_df[,1033])))

for(i in c(2:800)){
  covariance1 = covariance1 + (mean1 - t(u[,1:50]) %*% matrix(digits_df[,i])) %*% t((mean1 - t(u[,1:50]) %*% matrix(digits_df[,i])))
}

for(i in c(1034:1732)){
  covariance2 = covariance2 + (mean2 - t(u[,1:50]) %*% matrix(digits_df[,i])) %*% t((mean2 - t(u[,1:50]) %*% matrix(digits_df[,i])))
}

covariance1 = covariance1/799
covariance2 = covariance2/699

sv_sigma1 <- svd(covariance1)
d_sigma1 <- sv_sigma1$d
u_sigma1 <- sv_sigma1$u
v_sigma1 <- sv_sigma1$v

d_root1 = solve(diag(d_sigma1) %*% diag(d_sigma1))

sv_sigma2 <- svd(covariance2)
d_sigma2 <- sv_sigma2$d
u_sigma2 <- sv_sigma2$u
v_sigma2 <- sv_sigma2$v

d_root2 = solve(diag(d_sigma2) %*% diag(d_sigma2))

classificationqda = matrix(0.,232 + 258,1) 

for(i in c(801:1032)){
  del1 = t(d_root1 %*% t(u_sigma) %*% ( mean1 - t(u[,1:50]) %*% matrix(digits_df[,i]) )) %*% (d_root1 %*% t(u_sigma) %*% ( mean1 - t(u[,1:50]) %*% matrix(digits_df[,i]) ))
  del2 = t(d_root2 %*% t(u_sigma) %*% ( mean2 - t(u[,1:50]) %*% matrix(digits_df[,i]) )) %*% (d_root2 %*% t(u_sigma) %*% ( mean2 - t(u[,1:50]) %*% matrix(digits_df[,i]) ))
  del1 = del1/2 - log(8/15)
  del2 = del2/2 - log(7/15)
  
  if(del1>del2){
    classificationqda[i-800] = 6
  }
  
  if(del1<del2){
    classificationqda[i-800] = 2
  }
}

for(i in c(1733:1990)){
  del1 = t(d_root1 %*% t(u_sigma) %*% ( mean1 - t(u[,1:50]) %*% matrix(digits_df[,i]) )) %*% (d_root1 %*% t(u_sigma) %*% ( mean1 - t(u[,1:50]) %*% matrix(digits_df[,i]) ))
  del2 = t(d_root2 %*% t(u_sigma) %*% ( mean2 - t(u[,1:50]) %*% matrix(digits_df[,i]) )) %*% (d_root2 %*% t(u_sigma) %*% ( mean2 - t(u[,1:50]) %*% matrix(digits_df[,i]) ))
  del1 = del1/2 - log(8/15)
  del2 = del2/2 - log(7/15)
  
  if(del1>del2){
    classificationqda[i-1500] = 6
  }
  
  if(del1<del2){
    classificationqda[i-1500] = 2
  }
}

miss_rateqda = 1 - (sum(classificationqda[1:232]==2) + sum(classificationqda[233:490]==6))/490