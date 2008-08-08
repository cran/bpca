`bpca.prcomp` <-
function(x, lambda.ini=1, lambda.end=2, ...)
{
  stopifnot(class(x) == 'prcomp')

  if (!length(x$x))
    stop(gettextf("object '%s' has no objects coordinates!",
         deparse(substitute(x))), domain=NA)

  if (is.complex(x$x))
    stop("bpca is not defined for complex PCA!")

  n.lambdas <- (lambda.end - lambda.ini + 1)
  if(n.lambdas < 2 || n.lambdas > 3)
    stop('Please, check the parameters: lambda.ini and lambda.end:\n',
         'It should be 2 (to bpca.2d) or 3 (to bpca.3d).\n\n')

  # Due to necessity of different type of factorization it will go back
  # from prcom, i.e, it will regenerate the data x already scaled!
  xreg <- x$x %*%
          (solve(t(x$rotation) %*%
                 x$rotation) %*%
                 t(x$rotation))
  #xreg <- x$x %*%
  #        ginv(x$rotation) # another option (will require MASS)
  bpca.default(xreg,
                lambda.ini,
                lambda.end,
                var.center=ifelse(x$center[1] == FALSE, FALSE, TRUE),
                var.scale=ifelse(x$scale[1] == FALSE, FALSE, TRUE), ...)
}
