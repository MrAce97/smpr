
source('lib.r')
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