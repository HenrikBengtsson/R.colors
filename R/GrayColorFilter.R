########################################################################/**
# @RdocClass GrayColorFilter
#
# @title "Class representing a grayscale color filter"
#
# \description{
#  @classhierarchy
#
#  @get "title". 
#  All colors filtered through a GrayScaleFilter will be graylevel colors.
# }
#
# \section{Fields and Methods}{
#  @allmethods
# }
#
# @synopsis
# 
# \arguments{
#   \item{coefficients}{@vector of three @numeric values specifying the
#    relative amount of the red, green and blue channel that should be
#    used to generate the graylevels. The value are normalized such that
#    they sum to one.}
# }
#
# @author
#
# \keyword{color}
#*/#########################################################################
setConstructorS3("GrayColorFilter", function(coefficients=c(0.30,0.59,0.11)) {
  if (any(coefficients < 0))
    throw("Argument 'coefficients' contains negative values: ", paste(coefficients, collapse=", "));
  coefficients <- rep(coefficients, length.out=3);
  coefficients <- coefficients / sum(coefficients);

  extend(ColorFilter(), "GrayColorFilter",
    coefficients = coefficients
  )
})


setMethodS3("getColors", "GrayColorFilter", function(this, colors, coefficients=this$coefficients, ...) {
  colors <- as.character(colors);
  rgb <- col2rgb(colors);
  rgb <- rgb / 255;
  rm(colors);

  # Weight the the three color channels by the coefficients and sum them
  # up to get the gray color value in [0,1].
  rgb <- coefficients*rgb;
  gray <- apply(rgb, MARGIN=2, FUN=sum);

  # Some values may become out of range because of numerical problems.
  # Such values are truncate to one.
  gray[gray > 1] <- 1;

  getColors(GrayColor(gray));
})


############################################################################
# HISTORY:
# 2003-11-19
# o Created.
############################################################################
