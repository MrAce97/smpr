euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}


sortObjectsByDist <- function(xl, z, metricFunction =
                                euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  weights <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    weights[i, ] <- c(i, 1/metricFunction(xl[i, 1:n], z))
  }
  
  return (weights);
}

kwNN <- function(xl, z, k)
{
  weights <- sortObjectsByDist(xl, z)
  orderedXl <- xl[order(weights[, 2],decreasing = TRUE), ]
  kx <- c("setosa", "versicolor","virginica")
  w <- c(0,0,0)
  for(i in 1:3){
    for(j in 1:k){
      if(kx[i] == orderedXl[j,3]){
        w[i] <- w[i] + weights[j,2] 
      }
      
    }
  }
  f <- which.max(w)
  return (kx[f])
}

colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col
     = colors[iris$Species], asp = 1)
z <- c(4, 2)
xl <- iris[, 3:5]
class <- kwNN(xl, z, k=5)
points(z[1], z[2], pch = 23, col = 1, bg = colors[class], asp = 1)



