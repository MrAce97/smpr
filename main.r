euclideanDistance <- function(u, v){     
  sqrt(sum((u - v)^2)) 
}
sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance) {
  l <- dim(xl)[1]     
  n <- dim(xl)[2] - 1               
  distances <- matrix(NA, l, 2)          
  for (i in 1:l){         
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))     
  }               
  orderedXl <- xl[order(distances[, 2]), ]          
  return (orderedXl); 
}
w <- function(i,k,q){
  if(k>0) {
    (k+1-i)/k
  }
  else if(q>0&q<1){
    q^i
  } 
}

kwNN <- function(xl, z, k,q){     
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1          
  classes <- orderedXl[1:k, n + 1]
  list <- unique(classes)
  counts = 0
  for(i in 1:length(list)){
    counts[i] = 0
  }
  for (i in 1:k)
  {
    counts[which(classes[i]==list)] =
      counts[which(classes[i]==list)] + w(i,k)
  }
  return (list[which.max(counts)]) 
}
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
loo <- function(xl,k,a){
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  res <- 0
  for (i in 1:l)
  {
    if(a(xl[-i, ], xl[i,-(n+1)], k) != xl$Species[i]) 
    {
      res <- res+1
    }
  }
  return (res/l)
}


colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)  
z <- c(2.7, 1) 
xl <- iris[, 3:5] 
class <- parcenWindowFloat(xl, z, 8)
res <- loo(xl,5,parcenWindowFloat)
print(res)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1) 
