## Plot objects of the class 'bpca.3d' with the packages 'scatterplot3d'
## ('graphics' based) and 'rgl'
plot.bpca.3d <-
function(x, rgl.use=FALSE, ref.lines=TRUE, ref.color='navy', ref.lty=ifelse(rgl.use, NA, 'dotted'),
         clear3d=ifelse(rgl.use, TRUE, NULL), simple.axes=ifelse(rgl.use, TRUE, NULL),
         aspect=ifelse(rgl.use, c(1, 1, 1), NULL),
         var.factor=1, var.color='red3', var.lty=ifelse(rgl.use, NA, 'solid'), 
         var.pch=ifelse(rgl.use, NULL, 20), var.pos=ifelse(rgl.use, 0, 4),
         var.cex=ifelse(rgl.use, .8, .6), var.offset=ifelse(rgl.use, NULL, .2),
         obj.color='black', obj.pch=ifelse(rgl.use, NULL, 20), obj.pos=ifelse(rgl.use, 0, 4),
         obj.cex=ifelse(rgl.use, .8, .6), obj.offset=ifelse(rgl.use, NULL, .2),
         obj.names=TRUE, obj.labels=rownames(x$coord$objects), obj.identify=FALSE,
         box=FALSE, angle=ifelse(rgl.use, NULL, 40), ...)
{
  if (!inherits(x, 'bpca.3d'))
    stop("Use this function only with 'bpca.3d' class!")

  scores <- rbind(x$coord$objects,
                  x$coord$variables*var.factor,
                  rep(0, ncol(x$coord$objects)))

  # spatial limits
  ms  <- max(abs(scores)) * 1.2
  msp <- c(-ms, ms)

  # Plot bpca.3d under package 'scatterplot3d'
  if(!rgl.use) {

    # check scatterplot3d package
    necessary <- 'scatterplot3d'
    installed <- necessary %in% installed.packages()[, 'Package']
    if (length(necessary[!installed]) >=1)
      stop('The package scatterplot3d is necessary to run this function!')
    require(scatterplot3d)

    op <- par(no.readonly=TRUE)
    # a empty plot (reference)
    graph <- scatterplot3d(scores,
                           xlim=msp,
                           ylim=msp,
                           zlim=msp,
                           type='n',
                           xlab=paste('PC',
                                      x$number[1],
                                      sep=''),
                           ylab=paste('PC',
                                      x$number[2],
                                      sep=''),
                           zlab=paste('PC',
                                      x$number[3],
                                      sep=''),
                           grid=FALSE,
                           box=box,
                           angle=angle, ...)

    # objects
    if(obj.names) {
      # points of objects
      graph$points3d(x$coord$objects,
                     pch=obj.pch,
                     type='p',
                     col=obj.color,
                     cex=obj.cex, ...)

      # labels of objects
      text(graph$xyz.convert(x$coord$objects),
           labels=rownames(x$coord$objects),
           pos=obj.pos,
           offset=obj.offset,
           col=obj.color,
           cex=obj.cex, ...)
    } else {
      graph$points3d(x$coord$objects,
                     pch=obj.pch,
                     type='p',
                     col=obj.color,
                     cex=obj.cex, ...)
    }

    # variables
    for(i in 1:nrow(x$coord$variables)) {
      # points of variables
      graph$points3d(c(0, x$coord$variables[i,1]*var.factor),
                     c(0, x$coord$variables[i,2]*var.factor),
                     c(0, x$coord$variables[i,3]*var.factor),
                     pch=var.pch,
                     col=var.color,
                     type='p',
                     lty=var.lty,
                     cex=var.cex, ...)

      # segments of variables (vectors)
      graph$points3d(c(0, x$coord$variables[i,1]*var.factor),
                     c(0, x$coord$variables[i,2]*var.factor),
                     c(0, x$coord$variables[i,3]*var.factor),
                     col=var.color,
                     type='l',
                     lty=var.lty, ...)
    }

    # labels of variables
    text(graph$xyz.convert(x$coord$variables*var.factor),
         labels=rownames(x$coord$variables),
         pos=var.pos,
         offset=var.offset,
         col=var.color,
         cex=var.cex, ...)

    # reference lines
    if(ref.lines) {
      # x
      graph$points3d(msp,
                     c(0, 0),
                     c(0, 0),
                     type='l',
                     lty=ref.lty,
                     col=ref.color, ...)

      # y
      graph$points3d(c(0, 0),
                     msp,
                     c(0, 0),
                     type='l',
                     lty=ref.lty,
                     col=ref.color, ...)

      # z
      graph$points3d(c(0, 0),
                     c(0, 0),
                     msp,
                     type='l',
                     lty=ref.lty,
                     col=ref.color, ...)
    }

    if(obj.identify) 
      identify(x=graph$xyz.convert(x$coord$objects), labels=obj.labels, cex=obj.cex)

    par(op,
        no.readonly=TRUE)
  }

  # Plot bpca.3d under package 'rgl'
  if(rgl.use) {

    # check rgl package
    necessary <- 'rgl'
    installed <- necessary %in% installed.packages()[, 'Package']
    if (length(necessary[!installed]) >=1)
      stop('The package rgl is necessary to run this function!')
    require(rgl)

    size <- max(x$coord$objects) /
            20*obj.cex

    if (clear3d)
      clear3d()

    # a empty plot (reference)
    plot3d(scores,
           xlim=msp,
           ylim=msp,
           zlim=msp,
           xlab='',
           ylab='',
           zlab='',
           type='n',
           axes=FALSE,
           box=box,
           aspect=aspect,
           top=TRUE, ...)

    # objects
    if (obj.names) {
      # points of objects
      spheres3d(x$coord$objects,
                col=obj.color,
                radius=size / 2,
                alpha=.5, ...)

      # labels of objects
      text3d(x$coord$objects,
             texts=rownames(x$coord$objects),
             col=obj.color,
             alpha=.5,
             adj=obj.pos,
             cex=obj.cex, ...)
    } else {
      spheres3d(x$coord$objects,
                col=obj.color,
                radius=size,
                alpha=.5, ...)
    }
    # variables
    for(i in 1:nrow(x$coord$variables)) {
      # points of variables
      spheres3d(x$coord$variables[i,]*var.factor,
                col=var.color,
                radius=size / 2,
                alpha=.5, ...)

      # segments of variables (vectors)
      segments3d(rbind(matrix(0,
                              nc=3),
                 x$coord$variables[i,]*var.factor),
                 col=var.color, ...)
    }

    # labels of variables
    text3d(x$coord$variables*var.factor,
           texts=rownames(x$coord$variables),
           col=var.color,
           adj=var.pos,
           cex=var.cex, ...)

    # axes
    if(simple.axes) {
      axes3d(c('x', 'y', 'z'))
      # simple axis
      title3d(xlab=paste('PC',
                         x$number[1],
                         sep=''),
              ylab=paste('PC',
                         x$number[2],
                         sep=''),
              zlab=paste('PC',
                         x$number[3],
                         sep=''), ...)
    }
    else
      # axes with box
      decorate3d(xlab=paste('PC',
                            x$number[1],
                            sep=''),
                 ylab=paste('PC',
                            x$number[2],
                            sep=''),
                 zlab=paste('PC',
                            x$number[3],
                            sep=''),
                 box=box, ...)

    # reference lines
    if(ref.lines) {
      # x
      lines3d(msp,
              c(0, 0),
              c(0, 0),
              lty=ref.lty,
              col=ref.color, ...)

      # y
      lines3d(c(0, 0),
              msp,
              c(0, 0),
              lty=ref.lty,
              col=ref.color, ...)

      # z
      lines3d(c(0, 0),
              c(0, 0),
              msp,
              lty=ref.lty,
              col=ref.color, ...)
    }
  }
}
