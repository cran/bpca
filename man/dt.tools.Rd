\name{dt.tools}
\alias{dt.tools}

\title{Data Tools for Multivariate}

\description{
  Calculates vector variable lengths, angles between vector variables and
  variable correlations from `data.frame' or `matrix' objects
}

\usage{
  dt.tools(x, var.position=2, var.center=TRUE, var.scale=FALSE)
}

\arguments{
  \item{x}{A \code{data.frame} or \code{matrix} object.}
  \item{var.position}{A vector of length 1 giving the position of the elements
    to be considered as variables on the given object. They will be represented
    as vectors in the graphical representation. \emph{1} indicates \emph{rows}
    and \emph{2} indicates \emph{columns}.}
  \item{var.center}{A logical value indicating whether the variables should be
    shifted to be zero centered. Alternatively, a vector of length equal to the
    number of columns of x can be supplied. The value is passed to
    \code{scale}.}
  \item{var.scale}{A logical value indicating whether the variables should be
    scaled to have unit variance before the analysis takes place.
    Alternatively, a vector of length equal the number of columns of x can be
    supplied. The value is passed to \code{scale}.}
}

\details{
  This function computes: vector variable lengths, angles between vector
  variables and variable correlations from \code{data.frame} or
  \code{matrix} objects.

  If the data are centered (\emph{var.center=TRUE}), the correlations are the
  same as those obtained by the \code{cor} function.
}

\value{
  An \emph{list} with the components:
  \item{length}{A vector of the lengths.}
  \item{angle}{A matrix of the angles.}
  \item{r}{A matrix of the observed correlations.}
}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  and\cr
  Clarice Garcia Borges Demetrio (\email{clarice@esalq.usp.br})
}

\references{
  Johnson, R. A. and Wichern, D. W. (1988)
  \emph{Applied multivariate statistical analysis}.
  Prentice-Hall, Inc., Upper Saddle River, NJ, USA, 6 ed.
}

\seealso{\code{\link{bpca}}}

\examples{
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
}

\keyword{multivariate}
