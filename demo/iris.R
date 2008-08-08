##
## Grouping objects with different symbols and colors - 2d and 3d
##

library(bpca)
x11(w=6, h=6)

# 2d
plot(bpca(iris[-5]),
     var.factor=.3, var.cex=.7,
     obj.names=FALSE, obj.cex=1.5,
     obj.col=c('red', 'green3', 'blue')[unclass(iris$Species)],
     obj.pch=c('+', '*', '-')[unclass(iris$Species)])

# 3d static
plot(bpca(iris[-5], lambda.end=3),
     var.factor=.2, var.color=c('blue', 'red'), var.cex=1,
     obj.names=FALSE, obj.cex=1,
     obj.col=c('red', 'green3', 'blue')[unclass(iris$Species)],
     obj.pch=c('+', '*', '-')[unclass(iris$Species)])

# 3d dinamic
plot(bpca(iris[-5], method='hj', lambda.end=3), rgl.use=TRUE,
     var.col='brown', var.factor=.3, var.cex=1.2,
     obj.names=FALSE, obj.cex=.8,
     obj.col=c('red', 'green3', 'orange')[unclass(iris$Species)],
     simple.axes=FALSE, box=TRUE)
