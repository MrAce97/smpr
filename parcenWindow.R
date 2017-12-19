
kernel <- function(par){
  2/pi/(exp(par)+exp(-par))
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
xl <- iris[, 3:5] 
margin <- 0.3
xright <- max(xl[, 1]) + margin
xleft <-max(min(xl[, 1]) - margin,0)
ytop <- max(xl[, 2]) + margin
ybot <- max(min(xl[, 2]) - margin,0)
par(xpd=FALSE,oma=c(0,0,0,10))

plot(main="parsenWindowFloat, k=6", x=1, type="n", xlab=colnames(xl)[1], ylab=colnames(xl)[2],
     xlim=c(xleft, xright), ylim=c(ybot, ytop), xaxs="i", yaxs="i")

for(i in seq(from=ybot+0.1, to=ytop-0.1, by=0.1)) {
  for(j in seq(from=xleft+0.1, to=xright-0.1, by=0.1)) {
    class <- parcenWindowFloat(xl, c(j,i), 6)
    print(class)
    if(class!=0) points(j, i, pch =1, col =colors[class]) # cex=6, lwd=4, asp=1
  }
}

points(iris[, 3:5], pch =21, bg = colors[iris$Species], col ="black", asp=1)