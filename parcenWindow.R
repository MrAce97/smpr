source("lib.r")

kernel <- function(par){
  ifelse(abs(par)>1, 0, 3/4*(1-par^2))
}
parcenWindowFixed <- function(xl,z,h){
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1          
  list <- unique(xl[,n+1])
  counts = 0
  for(i in 1:length(list)){
    counts[i] = 0
  }
  for (i in 1:l)
  {
    counts[which(xl[i,n+1]==list)] =
      counts[which(xl[i,n+1]==list)] + kernel(euclideanDistance(z,xl[i,1:n])/h)
  }
  print(counts)
  return (list[which.max(counts)]) 
  
}
parcenWindowFloat <- function(xl,z,k){
  orderedXl <- sortObjectsByDist(xl, z)
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1          
  list <- unique(xl[,n+1])
  h <- euclideanDistance(z,orderedXl[k+1,1:n])
  counts = 0
  for(i in 1:length(list)){
    counts[i] = 0
  }
  for (i in 1:l)
  {
    counts[which(xl[i,n+1]==list)] =
      counts[which(xl[i,n+1]==list)] + kernel(euclideanDistance(z,xl[i,1:n])/h)
  }
  print(counts)
  return (list[which.max(counts)]) 
  
}