##
## Basic presentation and comparision with 'biplot' function ('stats' package)
##

# Open e configure a graphical device
x11(w=8, h=4)
op <- par(no.readonly=TRUE)
par(mfrow=c(1, 2))
oask <- devAskNewPage(dev.interactive(orNone = TRUE))

# The biplot of package stats (left) and bpca of package biplot (right)
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

devAskNewPage(oask)

