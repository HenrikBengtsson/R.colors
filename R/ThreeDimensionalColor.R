#########################################################################/**
# @RdocClass ThreeDimensionalColor
#
# @title "Abstract class representing \"three-dimensional\" colors"
#
# \description{
#  @classhierarchy
#
#  @get "title".
# }
#
# \section{Fields and Methods}{
#  @allmethods
#
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{\emph{Three} (preferably named) @vector arguments of color
#     values in the range [0,\code{maxColorValue}].}
#   \item{maxColorValue}{@numeric specifying maximum color value possible.}
# }
#
# @author
#
# \keyword{color}
#*/#########################################################################
setConstructorS3("ThreeDimensionalColor", function(..., maxColorValue=1) {
  colorspace <- Color$validateArgumentColorsK(list(...), maxColorValue=maxColorValue, K=3);
  extend(Color(), "ThreeDimensionalColor",
    colorspace = colorspace
  )
})


#########################################################################/**
# @RdocMethod getColorSpace
#
# @title "Gets the internal representation of the colors"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns the internal represenation of the colors as a @matrix with
#  three columns. Most often the values are in the range [0,1], but that is
#  not enforced.
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("getColorSpace", "ThreeDimensionalColor", function(this, ...) {
  this$colorspace;
})



#########################################################################/**
# @RdocMethod plot3d
#
# @title "Plots the colors in a three-dimensional plot"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# @author
#
# @examples "../incl/ThreeDimensionalColor.Rex"
#
# \keyword{color}
#
# \seealso{
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("plot3d", "ThreeDimensionalColor", function(this, col=getColors(this), xlab=NULL, ylab=NULL, zlab=NULL, xlim=c(0,1), ylim=xlim, zlim=xlim, ...) {
  colorspace <- getColorSpace(this);
  if (is.null(xlab))
    xlab <- colnames(colorspace)[1];
  if (is.null(ylab))
    ylab <- colnames(colorspace)[2];
  if (is.null(zlab))
    zlab <- colnames(colorspace)[3];
  require(R.basic);  # plot3d()
  plot3d(colorspace, col=col, xlab=xlab, ylab=ylab, zlab=zlab, xlim=xlim, ylim=ylim, zlim=zlim, ...);
})


############################################################################
# HISTORY:
# 2003-11-23
# o Added plot3d().
# 2003-11-21
# o Created.
############################################################################
