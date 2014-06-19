#########################################################################/**
# @RdocClass TopologyColor
#
# @title "Class representing topology colors"
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
#   \item{topology}{@vector of topology levels in the range [0,\code{maxColorValue}] 
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
setConstructorS3("TopologyColor", function(topology=NULL, ...) {
  extend(OneDimensionalColor(topology=topology, ...), "TopologyColor")
})

setMethodS3("getColors", "TopologyColor", function(this, filter=TRUE, ncolors=65535, ...) {
  # Get colors
  x <- getColorSpace(this);

  # Create color map
  colors <- topo.colors(ncolors);

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
