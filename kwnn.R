source("lib.r")
kwnn <- function(xl,z,k){
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1  
  orderedXl <- sortObjectsByDist(xl, z)
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

 
