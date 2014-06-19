#########################################################################/**
# @RdocClass HeatColor
#
# @title "Class representing heat colors"
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
#   \item{heat}{@vector of heat levels in the range [0,\code{maxColorValue}] 
#     where \code{maxColorValue} is given by the argument with the same
#     name in the constructor of the super class.}
#   \item{...}{Arguments passed to the constructor of the super class.}
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   @see "grDevices::palette".
# }
#*/#########################################################################
setConstructorS3("HeatColor", function(heat=NULL, ...) {
  extend(OneDimensionalColor(heat=heat, ...), "HeatColor")
})

setMethodS3("getColors", "HeatColor", function(this, filter=TRUE, ncolors=65535, ...) {
  # Get colors
  x <- getColorSpace(this);

  # Create color map
  colors <- heat.colors(ncolors);

  # Get the colors
  idx <- floor(x*(ncolors-1))+1;
  rm(x);

  col <- colors[idx];

  if (filter)
    col <- viewThroughColorFilter(this, col);
  col;
})


############################################################################
# HISTORY:
# 2002-11-19
# o Created from old Colors.R.
# o Added getRgbFromWavelength().
############################################################################
