#Computational Statistics Assignment 5 Queston 2

#Transition matrix
A = matrix(1,2,2)
A[1,1] = 0.95
A[1,2] = 0.05
A[2,1] = 0.1
A[2,2] = 0.9

#Emission matrix
E = matrix(1/6,2,6)
E[2,1] = 0.1
E[2,2] = 0.1
E[2,3] = 0.1
E[2,4] = 0.1
E[2,5] = 0.1
E[2,6] = 0.5

#Initial Distribution
pif = matrix(0.5,2,1)
pib = pif
pif_list = matrix(0,2,18)
pib_list = matrix(0,2,18)

#Observed list
obs = c(4,4,5,4,3,6,3,1,6,5,6,6,2,6,5,6,6,6)

#Forward computations
#pi1 = A %*% diag(E[,obs[1]]) %*% pif

for(j in c(1:18)){
  pif = A %*% diag(E[,obs[j]]) %*% pif
  pif = pif/sum(pif)
  pif_list[1,j] = pif[1]
  pif_list[2,j] = pif[2]
}

#Backward Computations
i = 18
while(i>=1){
  pib = A %*% diag(E[,obs[i]]) %*% pib
  pib = pib/sum(pib)
  pib_list[1,i] = pib[1]
  pib_list[2,i] = pib[2]
  i = i -1
}

final_p = matrix(0,2,18)

for(j in c(1:18)){
  final_p[1,j] = pib_list[1,j]*pif_list[1,j]
  final_p[2,j] = pib_list[2,j]*pif_list[2,j]
  final_p[,j] = final_p[,j]/(sum(final_p[,j]))
}

##### Part b : Viterbi algorithm

#Weight matrix
W = matrix(0,38,38)
W[1,2] = -log(0.5)
W[1,3] = -log(0.5)

#Adding in paths
for(i in c(1:17)){
  W[2*i,2*i+2] = -log(A[1,1]) - log(E[1,obs[i]])
  W[2*i,2*i+3] = -log(A[1,2]) - log(E[2,obs[i]])
  W[2*i+1,2*i+2] = -log(A[2,1]) - log(E[1,obs[i]])
  W[2*i+1,2*i+3] = -log(A[2,2]) - log(E[2,obs[i]])
  (i)
}

#Terms for final node

library(igraph)

g <- graph.adjacency(W, weighted=TRUE)

(s.paths <- shortest.paths(g, algorithm = "dijkstra"))
get.shortest.paths(g, 1, mode = "all")
