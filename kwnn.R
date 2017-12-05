euclideanDistance <- function(u, v){     
  sqrt(sum((u - v)^2)) 
}
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
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
  if(counts[1] == 0 && counts[2] == 0 && counts[3] == 0) return (" ")
  else return (list[which.max(counts)]) 
  
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

xl <- iris[, 3:5] 
margin <- 0.3
xright <- max(xl[, 1]) + margin
xleft <-max(min(xl[, 1]) - margin,0)
ytop <- max(xl[, 2]) + margin
ybot <- max(min(xl[, 2]) - margin,0)
par(xpd=FALSE,oma=c(0,0,0,10))

plot(main="parsenWindowFixed, h=1", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
     xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")

for(i in seq(from=ybot+0.1, to=ytop-0.1, by=0.1)) {
  for(j in seq(from=xleft+0.1, to=xright-0.1, by=0.1)) {
    class <- parcenWindowFixed(xl, c(j,i), 1)
    print(class)
    if(class!=0) points(j, i, pch =1, col =colors[class]) # cex=6, lwd=4, asp=1
  }
}

points(iris[, c(a,b)], pch =21, bg = colors[iris$Species], col ="black", asp=1)



