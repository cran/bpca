\name{bpca-package}
\alias{bpca-package}
\docType{package}

\title{
  Biplot of Multivariate Data Based on Principal Components Analysis
}

\description{
  Implements biplot (2d and 3d) and diagnostic tools of the quality of the reduction.
}

\details{
  \tabular{ll}{
  Package: \tab bpca\cr
  Type: \tab Package\cr
  Version: \tab 1.0.1\cr
  Date: \tab 2008-08-08\cr
  License: \tab GPL (>= 2)\cr
  }
}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  and\cr
  Clarice Garcia Borges Demetrio (\email{clarice@esalq.usp.br})
}

\references{
  Gabriel, K. R.  (1971)
  The biplot graphical display of matrices with application to principal
  component analysis.
  \emph{Biometrika} \bold{58}, 453-467.

  Gower, J.C. and Hand, D. J.  (1996)
  \emph{Biplots}. Chapman & Hall.

  Galindo, M. P. (1986)
  Una alternativa de representacion simultanea: HJ-Biplot.
  \emph{Questiio}, 10(1):13-23, 1986.

  Johnson, R. A. and Wichern, D. W. (1988)
  \emph{Applied multivariate statistical analysis}.
  Prentice-Hall, Inc., Upper Saddle River, NJ, USA, 6 ed.

  Yan, B. W. and Kang, M. S. (2003)
  \emph{GGE biplot analysis : a graphical tool for breeders,
  geneticists, and agronomists}. CRC Press, New York, 288p.
}

\keyword{package}
\keyword{multivariate}

\examples{
  ##
  ## Example 1
  ## Basic presentation and comparision with 'biplot' function ('stats' package)
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
  ## Example 2
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
}
