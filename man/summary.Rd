\name{summary}
\alias{summary}
\alias{summary.bpca.2d}
\alias{summary.bpca.3d}

\title{Summary Method for bpca Objects}

\description{
  Returns (and prints) a summary list for \code{bpca.2d} and
  \code{bpca.3d} objects.
}

\usage{
  \method{summary}{bpca.2d}(object, \dots)
  \method{summary}{bpca.3d}(object, \dots)
}

\arguments{
  \item{object}{A given object of the class \code{bpca.2d} or
    \code{bpca.3d}.}
  \item{\dots}{Potential further arguments (require by generic).}
}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  and\cr
  Clarice Garcia Borges Demetrio (\email{clarice@esalq.usp.br})
}

\seealso{\code{\link{bpca}}}

\examples{
  ##
  ## Example 1
  ## bpca - 2d
  ##

  library(bpca)
  bp <- bpca(gabriel1971)
  bp

  summary(bp)

  ##
  ## Example 2
  ## bpca - 3d
  ##

  bp <- bpca(gabriel1971, lambda.end=3)
  bp

  summary(bp)
}

\keyword{multivariate}
