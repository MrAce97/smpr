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
