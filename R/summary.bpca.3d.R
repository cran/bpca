`summary.bpca.3d` <-
function(object, ...)
{
  if (!inherits(object, 'bpca.3d'))
    stop("Use this function only with 'bpca.3d' class")

  cat('Eigenvalues:',
    object$eigenvalues)

  cat('\nEigenvalues considered on reduction:',
    object$eigenvalues[object$number[1]:object$number[3]])

  cat('\nCumulative variance explained:',
    cumsum(object$eigenvalues[object$number[1]:object$number[3]]^2) /
           sum(object$eigenvalues^2))

  cat('\nProp. of total variance explained:',
    object$importance[1])

  if(object$importance[1] != object$importance[2])
    cat('\nProp. of partial variance explained:',
      object$importance[2])

  cat('\n')
}
