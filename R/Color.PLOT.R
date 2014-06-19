#########################################################################/**
# @set "class=Color"
# @RdocMethod drawColorRamp
#
# @title "Draw a color ramp of the colors in the current plot"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# \arguments{
#  \item{x,y}{The x and y coordinate of the (non-rotated) upper-left corner
#    of the color ramp.}
#  \item{width,height}{The width and the height of the (non-rotated) color
#    ramp.}
#  \item{angle}{The rotation (in degree) of the color ramp.}
#  \item{borderColor}{The color of the border lines.}
#  \item{borderType}{The type of the border lines.}
#  \item{borderWidth}{The width of the border lines.}
#  \item{xpd}{If @TRUE, the color ramp can be drawn in the margins too,
#    otherwise not.}
#  \item{...}{Other arguments accepted by the @see "graphics::polygon" function.}
# }
#
# \value{
#  Returns a @list with x and y coordinates of the corners of the polygons 
#  but also their center coordinates, in the color ramp.
# }
#
# @examples "../incl/Color.drawColorRamp.Rex"
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# \keyword{color}
#*/######################################################################### 
setMethodS3("drawColorRamp", "Color", function(this, x, y, width, height, angle=0, xadj=0, yadj=1, borderColor=par("fg"), borderType=par("lty"), borderWidth=par("lwd"), xpd=par("xpd"), ...) {
  getCoord <- function(xy) {
    list(
      x = x + (xy$x*cos(angle) - xy$y*sin(angle)),
      y = y + (xy$x*sin(angle) + xy$y*cos(angle))
    )
  }

  angle <- (2*base::pi) / 360 * angle;

  colors <- getColors(this);
  ncolors <- length(colors);
 
  # First create a horizontal color ramp (then rotate it)
  xs <- seq(from=0, to=width, length=ncolors+1);
  xs <- matrix(xs, nrow=ncolors+1, ncol=2, byrow=FALSE);
  ys <- matrix(c(0,height), nrow=ncolors+1, ncol=2, byrow=TRUE);

  # "Adjust" coordinate
  xs <- xs - xadj*width;
  ys <- ys - yadj*height;

  xyOrg <- list(x=xs,y=ys);
  rm(xs,ys);

  xy <- getCoord(xyOrg);

  # Draw "colors"
  for (kk in 1:ncolors) {
    xx <- c(xy$x[kk,1],xy$x[kk+1,1],xy$x[kk+1,2],xy$x[kk,2],xy$x[kk,1]);
    yy <- c(xy$y[kk,1],xy$y[kk+1,1],xy$y[kk+1,2],xy$y[kk,2],xy$y[kk,1]);
    polygon(xx,yy, col=colors[kk], border=NA, xpd=xpd, ...);
  }

  # Draw "frame"
  
  for (kk in 1:nrow(xy$x)) {
    lines(xy$x[kk,], xy$y[kk,], col=borderColor, lty=borderType, lwd=borderWidth, xpd=xpd);
  }
  for (kk in 1:ncol(xy$x)) {
    lines(xy$x[,kk], xy$y[,kk], col=borderColor, lty=borderType, lwd=borderWidth, xpd=xpd);
  }

  center <- list();
  for (kk in 1:ncolors) {
    center$x <- c(center$x, mean(xyOrg$x[c(0,1)+kk,]));
    center$y <- c(center$y, mean(xyOrg$y[c(0,1)+kk,]));
  }
  above <- center; above$y <- above$y + height/2;
  below <- center; below$y <- below$y - height/2;
  center <- getCoord(center);
  xy$center <- getCoord(center);
  xy$above <- getCoord(above);
  xy$below <- getCoord(below);
  invisible(xy);
})




#########################################################################/**
# @RdocMethod plotParallel
#
# @title "Plots the column vectors in the colorspace matrix versus the index"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# \arguments{
#  \item{index}{@integer vector specifying the order the color vectors 
#    should be plotted.}
#  \item{xlab,ylab}{The labels on the x and the axis.}
#  \item{transpose}{If @TRUE, the colorspace matrix is transposed before
#    passed to @see "graphics::plot". This is makes it convenient to specify
#    plot attributes such as \code{col} or \code{pch} for either each
#    unique color or each color vector (column vector in the color space).}
#  \item{...}{Other arguments accepted by @see "graphics::plot".}
# }
#
# \value{
#  Returns nothing.
# }
#
# \examples{
#   x <- 0:100/100
#   ch1 <- (sin(x*2*pi) + 1)/2
#   ch2 <- (cos(x*2*pi) + 1)/2
#   rg <- TwoChannelMicroarrayColor(ch1, ch2, maxColorValue=1)  
#   layout(matrix(1:2))
#   plotParallel(rg)  
#   plotParallel(rg, col=c("red", "green"), transpose=TRUE)
#
#   @include "../incl/RgbColor.plotParallel.Rex"
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# \keyword{color}
#*/######################################################################### 
setMethodS3("plotParallel", "Color", function(this, index=seq(this), xlab="index", ylab=paste(colnames(this), collapse=", "), col=getColors(this)[index], transpose=FALSE, ...) {
  colorspace <- getColorSpace(this)[index,];
  colorspace <- as.matrix(colorspace);
  n <- nrow(colorspace);
  k <- ncol(colorspace);
  x <- matrix(seq(length=n), nrow=n, ncol=k);
  
  if (transpose) {
    colorspace <- t(colorspace);
    x <- t(x);
  }

  plot(x, colorspace, xlab=xlab, ylab=ylab, col=col, ...);
})



#########################################################################/**
# @RdocMethod display
#
# @title "Displays the current set of colors"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# \keyword{color}
#*/######################################################################### 
setMethodS3("display", "Color", function(this, gridColor=NA, ..., label=c("none", "index", "rgb", "space"), labelColor=NULL, labelScale=0.6, labelRotate=0) {
  image(this, gridColor=gridColor, ...);
  imageText(this, label=label, col=labelColor, srt=labelRotate, cex=labelScale);
  invisible();
})





#########################################################################/**
# @RdocMethod image
#
# @title "Creates an image of the current set of colors"
#
# @synopsis
#
# \arguments{
#  \item{gridColor}{Color used for the grid.}
#  \item{gridLty}{Line type (an @integer) for the grid lines.}
#  \item{gridLwd}{Line width (a @numeric) of the grid lines.}
#  \item{axes}{If @TRUE, axes are shown, otherwise not.}
#  \item{main}{Main title (a @character string).}
#  \item{...}{Other arguments for @see "graphics::image".}
# }
#
# \description{
#  @get "title".
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# \keyword{color}
#*/######################################################################### 
setMethodS3("image", "Color", function(x, gridColor="gray", gridLty=3, gridLwd=1, axes=FALSE, main=NULL, ...) {
  # To please R CMD check.
  this <- x;

  color <- getColors(this);
  colorMap <- sort(unique(color));
  color <- match(color, colorMap);

  n <- length(color);
  if (n == 1) {
    nrow <- ncol <- 1;
  } else {
    side <- sqrt(n);
    nrow <- floor(side);
    ncol <- ceiling(n/nrow);
    if (ncol - nrow > 1) {
        nrow <- nrow + 1;
        ncol <- ceiling(n/nrow);
    }
  }
  img <- rep(NA, length.out=nrow*ncol);
  img[1:n] <- color;
  img <- matrix(img, nrow=nrow, ncol=ncol, byrow=TRUE);
  image270(img, col=colorMap, axes=axes, ...);

  if (!is.na(gridColor)) {
    for (y in (0:nrow-1/2)/(nrow-1))
      abline(h=y, col=gridColor, lty=gridLty, lwd=gridLwd);
    for (x in (0:ncol-1/2)/(ncol-1))
      abline(v=x, col=gridColor, lty=gridLty, lwd=gridLwd);
  }

  if (is.null(main))
    main <- paste(data.class(this), ": ", n, " color", ifelse(n==1,"","s"), sep=""); 
  title(main=main);
}, private=TRUE)




#########################################################################/**
# @RdocMethod imageText
#
# @title "Adds text to the last plot"
#
# @synopsis
#
# \arguments{
#  \item{label}{Type of labels to add (a @character string).}
#  \item{col}{Color for text.}
#  \item{cex}{Scale factor (a @numeric) for the text.}
#  \item{srt}{String rotation (a @numeric).}
# }
#
# \description{
#  @get "title".
# }
#
# @examples "../incl/Color.imageText.Rex"
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# \keyword{color}
#*/######################################################################### 
setMethodS3("imageText", "Color", function(this, label=c("none", "index", "rgb", "space"), col=NULL, cex=0.6, srt=0, ...) {
  label <- match.arg(label);

  n <- length(this);
  if (n == 1) {
    nrow <- ncol <- 1;
  } else {
    side <- sqrt(n);
    nrow <- floor(side);
    ncol <- ceiling(n/nrow);
    if (ncol - nrow > 1) {
        nrow <- nrow + 1;
        ncol <- ceiling(n/nrow);
    }
  }

  y <- nrow - matrix(1:ncol, nrow=nrow, ncol=ncol, byrow=TRUE);
  x <- matrix(1:nrow, nrow=nrow, ncol=ncol, byrow=FALSE);
  nx <- length(x);
  if (nx > n)
    x[(n+1):nx] <- y[(n+1):nx] <- NA;
  x <- (x-1) / (ncol-1);
  y <- y / (nrow-1);
  if (label == "none") {
  } else if (label == "index") {
    label <- 1:n;
    text(x,y, label=label, col=col, cex=cex, srt=srt);
  } else if (label == "rgb") {
    color <- getColors(this);
    label <- gsub("^#", "", color);
    for (kk in 1:3) {
      first <- 2*kk-1;
      last <- first + 1;
      str <- substring(label, first, last);
      adj <- kk - (3/2);
      text(x,y, label=str, col=col, cex=cex, srt=srt, adj=c(0.5,adj));
    }
  } else {
    space <- getColorSpace(this);
    space <- as.matrix(space);
    names <- colnames(space);
    key <- substring(names, 1,1);
    dim <- dim(space);
    space <- t(space);
    label <- formatC(space, format="f", digits=2);
    if (length(key) > 0)
      label <- paste(key, ":", label, sep="");
    dim(label) <- rev(dim);
    label <- t(label);
    for (kk in 1:ncol(label)) {
      adj <- kk - (ncol(label)/2);
      text(x,y, label=label[,kk], col=col, cex=cex, srt=srt, adj=c(0.5,adj));
    }
  }
}, private=TRUE) # imageText()



