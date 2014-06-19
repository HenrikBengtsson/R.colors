#########################################################################/**
# @RdocClass FourDimensionalColor
#
# @title "Abstract class representing \"four-dimensional\" colors"
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
#   \item{...}{\emph{Four} (preferably named) @vector arguments of color
#     values in the range [0,\code{maxColorValue}].}
#   \item{maxColorValue}{@numeric specifying maximum color value possible.}
# }
#
# @author
#
# \keyword{color}
#*/#########################################################################
setConstructorS3("FourDimensionalColor", function(..., maxColorValue=1) {
  colorspace <- Color$validateArgumentColorsK(list(...), maxColorValue=maxColorValue, K=4);
  extend(Color(), "FourDimensionalColor",
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
#  four columns. Most often the values are in the range [0,1], but that is
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
setMethodS3("getColorSpace", "FourDimensionalColor", function(this, ...) {
  this$colorspace;
})


############################################################################
# HISTORY:
# 2003-11-21
# o Created.
############################################################################
