`bpca.default` <-
function(x, lambda.ini=1, lambda.end=2,
         var.position=2, var.center=TRUE, var.scale=TRUE,
         method=c('hj', 'sqrt', 'jk', 'gh'),
         var.rb=FALSE, var.rd=FALSE, limit=10, ...)
{
  stopifnot(is.matrix(x) || is.data.frame(x))

  n.lambdas <- (lambda.end - lambda.ini + 1)
  if(n.lambdas < 2 || n.lambdas > 3)
    stop('Please, check the parameters: lambda.ini and lambda.end:\n',
         'It should be 2 (to bpca.2d) or 3 (to bpca.3d).\n\n')

  if(!any(var.position == 1:2))
    stop('Please, check the parameters: var.position:\n',
         'It should be 1 (rows) or 2 (columns).\n\n')

  x <- as.matrix(x)
  if(var.position == 1)
    x <- as.matrix(t(x))

  obj.names <- rownames(x)
  var.names <- colnames(x)

  x.scaled <- scale(x,
                    center=var.center,
                    scale=var.scale)  # scalle variables

  svdx.scaled <- svd(x.scaled)        # svd of variables

  # variables
  rownames(svdx.scaled$u) <- obj.names
  rownames(svdx.scaled$v) <- var.names
  colnames(svdx.scaled$v) <- paste('PC',
                                   1:length(svdx.scaled$d),
                                   sep='')

  # variables scaled
  s2.var.scaled <- diag(svdx.scaled$d[lambda.ini:lambda.end])

  switch(match.arg(method),
    hj = {
      g.var.scaled  <- svdx.scaled$u[ ,lambda.ini:lambda.end] %*%
                       s2.var.scaled
      h.var.scaled  <- s2.var.scaled %*%
                       t(svdx.scaled$v[ ,lambda.ini:lambda.end])
      hl.var.scaled <- t(h.var.scaled)
    },
    sqrt = {
      g.var.scaled  <- svdx.scaled$u[ ,lambda.ini:lambda.end] %*%
                       sqrt(s2.var.scaled)
      h.var.scaled  <- sqrt(s2.var.scaled) %*%
                       t(svdx.scaled$v[ ,lambda.ini:lambda.end])
      hl.var.scaled <- t(h.var.scaled)
    },
    jk = {
      g.var.scaled  <- svdx.scaled$u[ ,lambda.ini:lambda.end] %*%
                       s2.var.scaled
      h.var.scaled  <- t(svdx.scaled$v[ ,lambda.ini:lambda.end])
      hl.var.scaled <- t(h.var.scaled)
    },
    gh = {
      g.var.scaled  <- sqrt(nrow(x)-1) *
                       svdx.scaled$u[,lambda.ini:lambda.end]
      h.var.scaled  <- 1/sqrt(nrow(x)-1) *
                       s2.var.scaled %*%
                       t(svdx.scaled$v[,lambda.ini:lambda.end])
      hl.var.scaled <- t(h.var.scaled)
    })

  if(is.null(rownames(x.scaled)))
    row.names <- 1:nrow(x.scaled)
  else
    row.names <- rownames(x.scaled)

  if(is.null(colnames(x.scaled)))
    col.names <- paste('V',
                       1:ncol(x.scaled),
                       sep='')
  else
    col.names <- colnames(x.scaled)

  pc.names <- paste('PC',
                    lambda.ini:lambda.end,
                    sep='')

  rownames(g.var.scaled)  <- row.names
  colnames(g.var.scaled)  <- pc.names
  rownames(hl.var.scaled) <- col.names
  colnames(hl.var.scaled) <- pc.names

  # variables
  if(var.rb)
    var.rb.res <- var.rbf(hl.var.scaled)
  else
    var.rb.res <- NA

  if(var.rb & var.rd)
    var.rd.res <- var.rdf(x.scaled, var.rb.res, limit)
  else
    var.rd.res <- NA

  res <- list(call=match.call(),
              eigenvalues=svdx.scaled$d,
              eigenvectors=svdx.scaled$v,
              number=seq(lambda.ini, lambda.end, 1),
              importance=rbind(general=round(sum(svdx.scaled$d[lambda.ini:lambda.end]^2) /
                                             sum(svdx.scaled$d^2), 3),
                               partial=round(sum(svdx.scaled$d[lambda.ini:lambda.end]^2) /
                                             sum(svdx.scaled$d[lambda.ini:length(svdx.scaled$d)]^2), 3)),
              coord=list(objects=g.var.scaled,
                         variables=hl.var.scaled),
              var.rb=var.rb.res,
              var.rd=var.rd.res)

  colnames(res$importance) <- 'explained'

  if(ncol(g.var.scaled) == 2)
    class(res) <- c('bpca.2d', 'list')
  else if(ncol(g.var.scaled) == 3)
    class(res) <- c('bpca.3d', 'list')

  invisible(res)
}
