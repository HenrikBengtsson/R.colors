#########################################################################/**
# @RdocClass TwoDimensionalColor
#
# @title "Abstract class representing \"two-dimensional\" colors"
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
#   \item{...}{\emph{Two} (preferably named) @vector arguments of color
#     values in the range [0,\code{maxColorValue}].}
#   \item{maxColorValue}{@numeric specifying maximum color value possible.}
# }
#
# @author
#
# \keyword{color}
#*/#########################################################################
setConstructorS3("TwoDimensionalColor", function(..., maxColorValue=1) {
  colorspace <- Color$validateArgumentColorsK(list(...), maxColorValue=maxColorValue, K=2);
  extend(Color(), "TwoDimensionalColor",
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
#  two columns. Most often the values are in the range [0,1], but that is
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
setMethodS3("getColorSpace", "TwoDimensionalColor", function(this, ...) {
  this$colorspace;
})


setMethodS3("plot", "TwoDimensionalColor", function(x, col=getColors(this), xlab=NULL, ylab=NULL, xlim=c(0,1), ylim=xlim, ...) {
  # To please R CMD check.
  this <- x;

  colorspace <- getColorSpace(this);
  if (is.null(xlab))
    xlab <- colnames(colorspace)[1];
  if (is.null(ylab))
    ylab <- colnames(colorspace)[2];
  plot(colorspace, col=col, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, ...);
})



############################################################################
# HISTORY:
# 2003-11-23
# o Added plot().
# 2003-11-20
# o Added Rdoc comments.
# 2003-11-19
# o Created.
############################################################################
