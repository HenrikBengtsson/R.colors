#########################################################################/**
# @RdocClass GreenColor
#
# @title "Class representing green colors"
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
#   \item{green}{@vector of levels of green in range [0,\code{maxColorValue}] 
#     where \code{maxColorValue} is given by the argument with the same
#     name in the constructor of the super class.}
#   \item{...}{Arguments passed to the constructor of the super class.}
# }
#
# @author
#
# \keyword{color}
#*/#########################################################################
setConstructorS3("GreenColor", function(green=NULL, ...) {
  extend(OneDimensionalColor(green=green, ...), "GreenColor")
})

setMethodS3("getColors", "GreenColor", function(this, filter=TRUE, ...) {
  x <- getColorSpace(this);
  ok <- !is.na(x) & !is.infinite(x);
  zeros <- rep(0, length(x[ok]));
  col <- rep(NA, length(x));
  col[ok] <- rgb(zeros, x[ok], zeros);
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
