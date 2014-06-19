#########################################################################/**
# @RdocClass BlueColor
#
# @title "Class representing blue colors"
#
# \description{
#  @classhierarchy
#
#  @get "title".
# }
#
# \section{Fields and Methods}{
#   @allmethods
#
# } 
#
# @synopsis
#
# \arguments{
#   \item{blue}{@vector of levels of blue in range [0,\code{maxColorValue}] 
#     where \code{maxColorValue} is given by the argument with the same
#     name in the constructor of the super class.}
#   \item{...}{Arguments passed to the constructor of the super class.}
# }
#
# @author
#
# \keyword{color}
#*/#########################################################################
setConstructorS3("BlueColor", function(blue=NULL, ...) {
  extend(OneDimensionalColor(blue=blue, ...), "BlueColor")
})



setMethodS3("getColors", "BlueColor", function(this, filter=TRUE, ...) {
  x <- getColorSpace(this);
  ok <- !is.na(x) & !is.infinite(x);
  zeros <- rep(0, length(x[ok]));
  col <- rep(NA, length(x));
  col[ok] <- rgb(zeros, zeros, x[ok]);
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
