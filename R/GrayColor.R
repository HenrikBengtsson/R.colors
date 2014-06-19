#########################################################################/**
# @RdocClass GrayColor
#
# @title "Class representing grayscales"
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
#   \item{gray}{@vector of gray levels in the range [0,\code{maxColorValue}] 
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
#   @see "grDevices::gray".
# }
#*/#########################################################################
setConstructorS3("GrayColor", function(gray=NULL, ...) {
  extend(OneDimensionalColor(gray=gray, ...), "GrayColor")
})


setMethodS3("getColors", "GrayColor", function(this, filter=TRUE, ...) {
  x <- this$colorspace;
  ok <- !is.na(x) & !is.infinite(x);
  col <- rep(NA, length(x));
  col[ok] <- gray(x[ok]);
  if (filter)
    col <- viewThroughColorFilter(this, col);
  col;
})


############################################################################
# HISTORY:
# 2002-12-27
# o Added getRgbFromWavelength().
# 2002-10-09
# o Added toGrayScale() and toMonochrome().
# 2002-08-23
# * getRGBorHSV(), which is used by getRGB() and getHSV(), now return NA's
#   where its corresponding input values are NA's.
# 2002-05-31
# * Added invert(), rgbToColors() and getMonochrome().
# 2002-05-11
# * BUG FIX: getHeatColors(), getTopoColors(), getTerrainColors(),
#   getCyanMagentaColors() and getRainbowColors() always returned the last
#   value as NA. This was due to an internal out-of-range array index.
# 2002-05-07
# * Added getRed(), getGreen() and getBlue() because they are simplier than
#   using getRGB() if one just want red, green or blue.
# 2002-04-21
# * getGray() now supports NA values by setting the corresponding color to
#   NA also. In previous versions gray() would generate an error if one
#   tried to generate colors where some values where NA.
# * BUG FIX: Forgot to exclude Inf's and NA's from x.range in rescale1D().
# 2002-04-03
# * Added colorToRGB().
# * Added getHeatColors(), getTopoColors(), getTerrainColors(),
#   getCyanMagentaColors(), getRainbowColors().
# 2002-03-30
# * Replaced demo() with example code.
# * Recoded with setMethodS3's etc.
# 2001-08-09
# * BUG FIX: getRGBorHSV() sometimes generated x == 0, which would give
#   an error. Added x[is.na(x)] <- 0 to solve it.
# 2001-07-02
# * getRGBorHSV() supports both upper and lower case dim argument.
# 2001-06-23
# * Added some Rdoc comments with examples.
# 2001-05-14
# * Added getInternalReferences() for improving gco() performance.
# 2001-05-04
# * Now supports formal attributes.
# 2001-04-11
# * Created from old ColorFactory, GreenColorFactory etc.
# 2001-04-03
# * Created from old DotStyler.
############################################################################
