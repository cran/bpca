## Plot objects of the class 'bpca.2d' with the package 'graphics'
plot.bpca.2d <-
function(x, ref.lines=TRUE, ref.color='navy', ref.lty='dotted',
         var.factor=1, var.color='red3', var.lty='solid', var.pch=20,
         var.pos=4, var.cex=.6, var.offset=.2,
         obj.color='black', obj.pch=20, obj.pos=4, obj.cex=.6, obj.offset=.2,
         obj.names=TRUE, obj.labels=rownames(x$coord$objects), obj.identify=FALSE,
         xlim, ylim, xlab, ylab, ...)
{
  if (!inherits(x, 'bpca.2d'))
    stop("Use this function only with 'bpca.2d' class!")

  scores <- rbind(x$coord$objects,
                  x$coord$variables*var.factor,
                  rep(0,
                      ncol(x$coord$objects)))

  if (missing(xlim) || missing(ylim)) {
    ms  <- max(abs(scores)) * 1.2
    msp <- c(-ms, ms)
  }
  if (missing(xlim))
    xlim <- msp
  if (missing(ylim))
    ylim <- msp
  if (missing(xlab))
    xlab <- paste('PC', x$number[1], sep='')
  if (missing(ylab))
    ylab <- paste('PC', x$number[2], sep='')

  plot(scores,
       xlim=xlim,
       ylim=ylim,
       xlab=xlab,
       ylab=ylab,
       type='n', ...)

  if (ref.lines)
    abline(h=0,
           v=0,
           col=ref.color,
           lty=ref.lty)

  # objects
  if(obj.names) {
    points(x=x$coord$objects[,1],
           y=x$coord$objects[,2],
           pch=obj.pch,
           col=obj.color,
           cex=obj.cex, ...)

    text(x=x$coord$objects[,1],
         y=x$coord$objects[,2],
         labels=rownames(x$coord$objects),
         pos=obj.pos,
         offset=obj.offset,
         col=obj.color,
         cex=obj.cex, ...)
  } else {
    points(x=x$coord$objects[,1],
           y=x$coord$objects[,2],
           pch=obj.pch,
           col=obj.color,
           cex=obj.cex, ...)
  }

  # variables
  points(x=x$coord$variables[,1]*var.factor,
         y=x$coord$variables[,2]*var.factor,
         pch=var.pch,
         col=var.color,
         cex=var.cex, ...)

  segments(x0=0,
           y0=0,
           x1=x$coord$variables[,1]*var.factor,
           y1=x$coord$variables[,2]*var.factor,
           col=var.color,
           lty=var.lty, ...)

  text(x=x$coord$variables[,1]*var.factor,
       y=x$coord$variables[,2]*var.factor,
       labels=rownames(x$coord$variables),
       pos=var.pos,
       offset=var.offset,
       col=var.color,
       cex=var.cex, ...)

  if(obj.identify) 
    identify(x=x$coord$objects, labels=obj.labels, cex=obj.cex)
}

