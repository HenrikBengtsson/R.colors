#########################################################################/**
# @RdocClass CyanMagentaColor
#
# @title "Class representing cyan-magenta colors"
#
# \description{
#  @classhierarchy
#
#  @get "title". This type of colors are also refered to as
#  the \emph{Cleveland-style cyan-magenta colors}.
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
#   \item{cyanMagenta}{@vector of cyan-to-magenta levels in the range
#     [0,\code{maxColorValue}] 
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
setConstructorS3("CyanMagentaColor", function(cyanMagenta=NULL, ...) {
  extend(OneDimensionalColor(cyanMagenta=cyanMagenta, ...), "CyanMagentaColor")
})

setMethodS3("getColors", "CyanMagentaColor", function(this, filter=TRUE, ncolors=65535, ...) {
  # Get colors
  x <- getColorSpace(this);

  # Create color map
  col <- cm.colors(ncolors);

  # Get the colors
  idx <- floor(x*(ncolors-1))+1;
  rm(x);

  col <- col[idx];

  if (filter)
    col <- viewThroughColorFilter(this, col);
  col;
})


############################################################################
# HISTORY:
# 2010-11-28
# o Updated broken Rd cross links.
# 2002-11-19
# o Created from old Colors.R.
# o Added getRgbFromWavelength().
############################################################################
