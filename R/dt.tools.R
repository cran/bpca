`dt.tools` <-
function(x, var.position=2, var.center=TRUE, var.scale=FALSE)
{
  stopifnot(is.matrix(x) || is.data.frame(x))

  bCol = sapply(x,
                is.numeric)

  if(var.position == 1)
    x <- as.matrix(t(x[bCol]))


  x = x[, bCol]

  n <- ncol(x)
  if (n < 2 )
    stop('x has less than two columns (variables)!')

  x  <- scale(x,
              center=var.center,
              scale=var.scale)
  lv <- function(x) sqrt(t(x) %*% x)  # length of vector
  l  <- apply(x,
              2,
              lv)
  r  <- diag(n)

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {

      cost <- (t(x[,i]) %*%
              x[,j]) /
              (l[i]*l[j])

      r[j,i] <- cost    # fill lower.tri
      r[i,j] <- r[j,i]  # fill upper.tri
    }
  }

  a <- acos(r)*180/pi

  dimnames(r) <- list(colnames(x),
                     colnames(x))

  dimnames(a) <- dimnames(r)

  res <- list(length=l,
              angle=a,
              r=r)

  invisible(res)
}
