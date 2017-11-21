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
