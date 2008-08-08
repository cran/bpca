\name{bpca}
\alias{bpca}
\alias{bpca.default}
\alias{bpca.prcomp}

\title{Biplot of Multivariate Data Based on Principal Components Analysis}

\description{
  Computes biplot reduction on \code{data.frame}, \code{matrix} or
  \code{prcomp} objects and returns a \code{bpca} object.
}

\usage{
  bpca(x, \dots)
  \method{bpca}{default}(x, lambda.ini=1, lambda.end=2, var.position=2,
    var.center=TRUE, var.scale=TRUE, method=c('hj', 'sqrt', 'jk', 'gh'),
    var.rb=FALSE, var.rd=FALSE, limit=10, \dots)
  \method{bpca}{prcomp}(x, lambda.ini=1, lambda.end=2, \dots)
}

\arguments{
  \item{x}{A \code{data.frame}, \code{matrix} or \code{prcomp} object.}
  \item{lambda.ini}{A vector of length 1 giving the fisrt eigenvalue to be
    considered by the biplot reduction.}
  \item{lambda.end}{A vector of length 1 giving the last eigenvalue to be
    considered by the biplot reduction.}
  \item{var.position}{A vector of length 1 giving the position of the elements
    to be considered as variables on the given object. They will be presented
    as vectors in the graphical representation. \emph{1} indicates \emph{rows}
    and \emph{2} indicates \emph{columns}.}
  \item{var.center}{A logical value indicating whether the variables should be
    shifted to be zero centered. Alternatively, a vector of length equal to the
    number of columns of x can be supplied. The value is passed to
    \code{scale}.}
  \item{var.scale}{A logical value indicating whether the variables should be
    scaled to have unit variance before the analysis takes place.
    Alternatively, a vector of length equal to the number of columns of x can be
    supplied. The value is passed to \code{scale}.}
  \item{method}{A vector of character strings that indicates the method of
    factorization. `hj' to HJ (\emph{simetric}, Galindo (1986)), `sqrt' to
    SQRT (\emph{squared root - simetric}, Gabriel (1971)), `jk' to JK
    (\emph{row metric preserving}, Gabriel (1971)) and 'gh' to GH (\emph{column
    metric preserving}, Gabriel (1971)).}
  \item{var.rb}{A logical value. If TRUE, all correlation coefficients for all
    variables (\emph{under the biplot projection}) will be computed.}
  \item{var.rd}{A logical value. If TRUE, the diagnostic of the representation
    of variables projected by the biplot will be computed.
    If \code{var.rd} is TRUE the \code{var.rb} parameter must be also TRUE.}
  \item{limit}{A vector giving the percentual limit to define poor
    representation of variables.}
  \item{\dots}{Other graphical parameters may also be passed as arguments to
    these functions.}
}

\details{
  The biplot is a multivariate method for graphing row and column elements using
  a single plot (Gabriel, 1971).

  The biplot of a matrix $_{n}Y_{p}$ projects on the same plot: \emph{rows}
  (associated with \emph{n} objects) and \emph{columns} (associated with
  \emph{p} variables), markers from data that forms a two-way table
  (\code{data.frame} or \code{matrix} object).
  The markers are computed from the singular value decomposition,
  \code{svd(Y)}, and subsequent factorization.

  The \emph{bi} refers to the kind of information contained in a data set
  disposed in a two-way table. If the data are a \emph{tri}-dimensional array
  the method will be called \emph{triplot} (not still contemplated in the bpca
  package).

  The basic idea behind the biplot method was to add the information about the
  variables to the principal component graph (Johnson \& Wichern, 1988).

  Considering the results of \deqn{svd(_{n}Y_{p})}
  \item{\emph{d}:}{A vector containing the singular values of Y, of length
    \code{min(n, p)};}
  \item{\emph{u}:}{A matrix whose columns contain the left singular vectors of
    Y, present if \code{nu > 0}. Dimension \code{c(n, nu)};}
  \item{\emph{v}:}{A matrix whose columns contain the right singular vectors of
    Y, present if \code{nv > 0}. Dimension \code{c(p, nv)}.}

  and also,
  \deqn{s^2 = diag(d)}
  \deqn{n = n\_objects(Y)}

  it is possible an aproximation of Y:
  \deqn{_{n}Y_{p} \approx Y_{m} = g.h'}

  in various ways. The methods of factorization computed by the \code{bpca}
  function are:

  \item \emph{HJ - simetric}, Galindo (1986):
    \deqn{g = u*s^2}
    \deqn{h = s^2*v'}
  \item \emph{SQRT - squared root simetric}, Gabriel (1971):
    \deqn{g = u*\sqrt{s^2}}
    \deqn{h = \sqrt{s^2}*v'}
  \item \emph{JK - row metric preserving}, Gabriel (1971):
    \deqn{g = u*s^2}
    \deqn{h = v'}
  \item \emph{GH - column metric preserving}, Gabriel (1971):
    \deqn{g = \sqrt{n-1}*u}
    \deqn{h = \frac{1}{\sqrt{n-1}}*s^2*v'}

  Considering $_{n}Y_{p} \approx Y_{m}$, it is possible to deduce that if the
  rank (r) of the matrix $_{n}Y_{p}$ is bigger than `m', the biplot
  representation of Y will be an approximation, and accurate only in the case
  of $r=m$.

  Due to the need of different methods of factorization, if \code{x} is a
  \code{prcomp} object, the method \code{bpca.prcomp} will go back from the
  \code{prcom} function. In other words, it will regenerates, or computes, the
  inverse of the \code{svd} decomposition of the given data $_{n}Y_{p}$.
  After this, it will make a call to the method \code{bpca.default} with the
  adequated parameters.

  The biplot is used with many multivariate methods to display
  relationships between objects, variables and the interrelationship
  between objects and variables (as prevalence, importance). There are many
  variations of biplots (see the references).
}

\value{
  The function \code{bpca} returns an object of class \code{bpca.2d} or
  \code{bpca.3d}. Both are \code{list} objects with the slots:
  \item{call}{The call used.}
  \item{eigenvalues}{A vector of the eigenvalues.}
  \item{eigenvectors}{A vector of the eigenvectors.}
  \item{numer}{A vector of the number of eigenvalues considered in the
    reduction.}
  \item{importance}{A matrix with the \emph{general} and \emph{partial}
    variation explained by the reduction.}
  \item{coord}{A list with the coordenates of the two components: objects and
    variables.}
  \item{var.rb}{A matrix of all correlation coefficients for all variables
    under the biplot projection.}
  \item{var.rd}{A matrix of the diagnostic of the poor projection of
    variable correlations by the biplot reduction.}
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

\seealso{\code{\link{biplot.prcomp}}}

\examples{
  ##
  ## Example 1
  ## Basic presentation and compararison with 'biplot' function ('stats' package)
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

  ##
  ## Example 3
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

  ##
  ## Example 4
  ## Computing and ploting a bpca object with 'rgl' package - 3d
  ##

  plot(bpca(gabriel1971, lambda.end=3),
       rgl.use=TRUE, var.factor=2)

  # Suggestion: Interact with the graphic with the mouse
  # left button: press, maintain and movement it to interactive rotation;
  # right button: press, maintain and movement it to interactive zoom.
  # Enjoy it!

  ##
  ## Example 5
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
}

\keyword{multivariate}