source("kwnn.r")
source("loo.r")
source("parcenWindow.r")




colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)  
z <- c(2.7, 1) 
xl <- iris[, 3:5] 
class <- parcenWindowFloat(xl, z, 8)
res <- loo(xl,5,parcenWindowFloat)
print(res)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1) 
