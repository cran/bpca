##
## Basic presentation and comparation with 'biplot' function ('stats' package)
##

library(bpca)

# Opening e configuring a graphical device
x11(w=8, h=4)
op <- par(no.readonly=TRUE)
par(mfrow=c(1, 2))

# Biplot of package stats (left) and bpca of package biplot (right)
# variables in columns (represented as red vectors)
# biplot
biplot(prcomp(caith, scale=FALSE),
       main='biplot (stats) (scale=FALSE)')

# bpca
plot(bpca(caith, var.scale=FALSE),
     main='bpca - hj (var.scale=FALSE)',
     var.factor=2, var.cex=1,
     obj.cex=1)

# Variables in rows (represented as red vectors)
biplot(prcomp(t(caith), scale=TRUE),
       main='biplot (stats) (scale=TRUE)')

plot(bpca(caith, var.scale=TRUE, var.pos=1),
     main='bpca - hj (var.scale=TRUE)',
     var.factor=2, var.cex=1,
     obj.cex=1)
par(op)

# Summarizing bpca
summary(bpca(caith, var.scale=FALSE))
bpca(caith, var.scale=FALSE)$coord
bpca(caith, var.scale=FALSE)$eigenvec

##
## Computing and ploting a bpca object with 'graphics' package - 2d
##

bp <- bpca(gabriel1971)

x11(w=5, h=5)
plot(bp, var.factor=2)

# Exploring the object 'bp' created by the function 'bpca'
class(bp)
names(bp)
str(bp)

summary(bp)
bp$call
bp$eigenval
bp$eigenvec
bp$numb
bp$import
bp$coord
bp$coord$obj
bp$coord$var
bp$var.rb
bp$var.rd

# Additional graphical parameters (nonsense)
plot(bpca(gabriel1971, meth='sqrt'),
     main='gabriel1971 - sqrt', sub='The graphical parameters are working fine!',
     var.factor=2, var.cex=.6, var.col=rainbow(9), var.pch='v',
     obj.pch='o', obj.cex=.5, obj.col=rainbow(8), obj.pos=1, obj.offset=.5)

##
## Computing and ploting a bpca object with 'scatterplot3d' package - 3d
##

bp <- bpca(gabriel1971, lambda.end=3)

x11(w=6, h=6)
plot(bp, var.factor=3)

# Exploring the object 'bp' created by the function 'bpca'
class(bp)
names(bp)
str(bp)

summary(bp)
bp$call
bp$eigenval
bp$eigenvec
bp$numb
bp$import
bp$coord
bp$coord$obj
bp$coord$var
bp$var.rb
bp$var.rd

# Additional graphical parameters (nonsense)
plot(bpca(gabriel1971, lambda.end=3, meth='jk'),
     main='gabriel1971 - jk', sub='The graphical parameters are working fine!',
     var.factor=6, var.pch='+', var.cex=.6, var.col='green4',
     obj.pch='*', obj.cex=.8, obj.col=1:8,
     ref.lty='solid', ref.col='red', angle=70)

##
## Computing and ploting a bpca object with 'rgl' package - 3d
##

plot(bpca(gabriel1971, lambda.end=3),
     rgl.use=TRUE, var.factor=2)

# Suggestion: Interact with the graphic with the mouse
# left button: press, maintain and movement it to interactive rotation;
# right button: press, maintain and movement it to interactive zoom.
# Enjoy it!

##
## Computing and ploting a bpca object with 'obj.identify=TRUE' parameter - 2d
##

bp <- bpca(gabriel1971)

# Normal labels
if(dev.interactive()) {
  plot(bp, obj.names=FALSE, obj.identify=TRUE)
}  

# Alternative labels
if(dev.interactive()) {
  plot(bp, obj.names=FALSE,
       obj.labels=c('toi', 'kit', 'bat', 'ele', 'wat', 'rad', 'tv', 'ref'),
       obj.identify=TRUE)
}       

##
## Computing and ploting a bpca object with 'obj.identify=TRUE' parameter - 3d
##

bp <- bpca(gabriel1971, lambda.end=3)

# Normal labels
if(dev.interactive()) {
  plot(bp, obj.names=FALSE, obj.identify=TRUE)
}  

# Alternative labels
if(dev.interactive()) {
  plot(bp, obj.names=FALSE,
       obj.labels=c('toi', 'kit', 'bat', 'ele', 'wat', 'rad', 'tv', 'ref'),
       obj.identify=T)
}

##
## Computes: vector variable lengths, angles between vector variables and
## variable correlations from dataframe or matrix objects (n x p)
## n = rows (objects)
## p = columns (variables)
##

dt <- dt.tools(iris[-5], 2)

# Exploring the object 'bp' created by the function 'var.tools'
class(dt)
names(dt)
str(dt)

dt$length
dt$angle
dt$r
dt

# Checking the determinations
iris.tools <- round(dt.tools(iris[-5], var.center=TRUE)$r, 5); iris.tools
iris.obsv  <- round(cor(iris[-5]), 5); iris.obsv
all(iris.tools == iris.obsv)

##
## Grouping objects with different symbols and colors - 2d and 3d
##

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

##
## Example of 'var.rb=TRUE' parameter as a measure of the quality of the biplot - 2d
##

## Differences between methods of factorization
# SQRT
bp1 <- bpca(gabriel1971, meth='sqrt', var.rb=TRUE)
qbp1 <- qbpca(gabriel1971, bp1)
plot(qbp1, main='sqrt - 2d \n (poor)')

# JK
bp2 <- bpca(gabriel1971, meth='jk', var.rb=TRUE)
qbp2 <- qbpca(gabriel1971, bp2)
plot(qbp2, main='jk - 2d \n (very poor)')

# GH
bp3 <- bpca(gabriel1971, meth='gh', var.rb=TRUE)
qbp3 <- qbpca(gabriel1971, bp3)
plot(qbp3, main='gh - 2d \n (good)')

# HJ
bp4 <- bpca(gabriel1971, meth='hj', lambda.end=2, var.rb=TRUE)
qbp4 <- qbpca(gabriel1971,  bp4)
plot(qbp4, main='hj - 2d \n (good)')

##
## Example of 'var.rb=TRUE' parameter as a measure of the quality of the biplot - 3d
##

## Differences between methods of factorization
# SQRT
bp1 <- bpca(gabriel1971, meth='sqrt', lambda.end=3, var.rb=TRUE)
qbp1 <- qbpca(gabriel1971, bp1)
plot(qbp1, main='sqrt - 3d \n (poor)')

# JK
bp2 <- bpca(gabriel1971, meth='jk', lambda.end=3, var.rb=TRUE)
qbp2 <- qbpca(gabriel1971, bp2)
plot(qbp2, main='jk - 3d \n (very poor)')

# GH
bp3 <- bpca(gabriel1971, meth='gh', lambda.end=3, var.rb=TRUE)
qbp3 <- qbpca(gabriel1971, bp3)
plot(qbp3, main='gh - 3d \n (whow!)')

# HJ
bp4 <- bpca(gabriel1971, meth='hj', lambda.end=3, var.rb=TRUE)
qbp4 <- qbpca(gabriel1971, bp4)
plot(qbp4, main='hj - 3d \n (whow!)')

##
## Example of 'var.rd=TRUE' parameter as a measure of the quality of the biplot - 2d
## Mainly recommended for large datasets.
##

bp <- bpca(gabriel1971, meth='hj', lambda.end=2,
           var.rb=TRUE, var.rd=TRUE, limit=3)

bp$var.rd

# RUR followed by CRISTIAN contains information in dimensions that
# wasn't contemplated by the biplot reduction (PC3).
# Between all, RUR followed by CRISTIAN, variables are bad represented by a 2d
# biplot.

# Graphical visualization of the importance of the variables not contemplated
# in the reduction
plot(bpca(gabriel1971, meth='hj', lambda.ini=3, lambda.end=4), main='hj')
