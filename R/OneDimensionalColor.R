#########################################################################/**
# @RdocClass OneDimensionalColor
#
# @title "Abstract class representing \"one-dimensional\" colors"
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
#   \item{...}{\emph{One} (preferably named) @vector arguments of color
#     values in the range [0,\code{maxColorValue}].}
#   \item{maxColorValue}{@numeric specifying maximum color value possible.}
# }
#
# @author
#
# \keyword{color}
#*/#########################################################################
setConstructorS3("OneDimensionalColor", function(..., maxColorValue=1) {
  colorspace <- Color$validateArgumentColorsK(list(...), maxColorValue=maxColorValue, K=1);
  extend(Color(), "OneDimensionalColor",
    colorspace = colorspace
  )
}, abstract=TRUE)



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
#  Returns the internal represenation of the colors as a column @vector.
#  Most often these values are in the range [0,1], but that is not
#  enforced.
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
setMethodS3("getColorSpace", "OneDimensionalColor", function(this, ...) {
  as.matrix(this$colorspace);
})






############################################################################
# HISTORY:
# 2003-11-19
# o Created.
############################################################################
