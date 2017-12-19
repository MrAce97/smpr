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