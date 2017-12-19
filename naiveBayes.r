kernel <- function(x){
  (1/sqrt(2*pi)*exp(-1/2*x^2))
}

naiveBayes <- function(xl,z){
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1   
  list <- unique(xl[,n+1])
  aprior<-c()
  counts = 0
  h <- c(1,1)
  apasterior <- c()
  for(i in 1:length(list)){
    temp_xl <- xl[xl$Species == list[i], ]
    aprior[i]<-nrow(temp_xl)/l
    apasterior_sum <-0
    for(j in 1:nrow(temp_xl)){
      apasterior_tmp<-1
      for(k in 1:n){
        
        apasterior_tmp <- 1/h[k]*kernel((z[k]-temp_xl[j,k])/h[k])*apasterior_tmp
      }
      apasterior_sum<-apasterior_sum+apasterior_tmp
    }
    apasterior[i]=1/nrow(temp_xl)+apasterior_sum
  }
  print(apasterior)
  counts <-c()
  for(i in 1:length(list)){
    counts[i]<-log(apasterior[i])+log(aprior[i])
  }
  
  return (list[which.max(counts)])
 
}
