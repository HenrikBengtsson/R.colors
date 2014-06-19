########################################################################/**
# @RdocClass MonochromeColorFilter
#
# @title "Class representing a monochrome color filter"
#
# \description{
#  @classhierarchy
#
#  @get "title". 
#  All colors filtered through a MonochromeColorFilter will be
#  monochrome colors.
# }
#
# \section{Fields and Methods}{
#  @allmethods
# }
#
# @synopsis
# 
# \arguments{
#   \item{threshold}{A @numeric threshold in [0,1] specifying for where
#    to split the first and the second monochrome color.}
#   \item{monocolors}{A @vector or a @see "Color" object containing two
#    colors to be used as the monochrome colors. 
#    By default white and black is used.}
#   \item{coefficients}{@vector of three @numeric values specifying the
#    relative amount of the red, green and blue channel that should be
#    used to generate the graylevels, which in turn are threshold to
#    monochrome colors. The values are normalized such that they sum to one.}
# }
#
# \details{
#   Conceptually, the input colors are filtered through a grayscale color
#   filter and then thresholded. Values \emph{below} the threshold is set
#   to the \emph{second} monochrome color and values equal or above to 
#   the first.
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   @see "GrayColorFilter".
# }
#*/#########################################################################
setConstructorS3("MonochromeColorFilter", function(threshold=0.5, monocolors=c("#FFFFFF", "#000000"), coefficients=c(0.3,0.59,0.11)) {
  monocolors <- getColors(RgbColor(monocolors));
  monocolors <- monocolors[1:2];

  if (any(coefficients < 0))
    throw("Argument 'coefficients' contains negative values: ", paste(coefficients, collapse=", "));
  coefficients <- rep(coefficients, length.out=3);
  coefficients <- coefficients / sum(coefficients);

  extend(ColorFilter(), "MonochromeColorFilter",
    threshold    = threshold,
    monocolors   = monocolors,
    coefficients = rep(coefficients, length.out=3)
  )
})


setMethodS3("getColors", "MonochromeColorFilter", function(this, colors, threshold=this$threshold, monocolors=this$monocolors, coefficients=this$coefficients, ...) {
  colors <- as.character(colors);
  rgb <- col2rgb(colors);
  rgb <- rgb / 255;
  rm(colors);

  # Weight the the three color channels by the coefficients and sum them
  # up to get the gray color value in [0,1].
  rgb <- coefficients*rgb;
  gray <- apply(rgb, MARGIN=2, FUN=sum);

  color <- rep(NA, length(colors));
  color[gray >= threshold] <- monocolors[1];
  color[gray  < threshold] <- monocolors[2];
  
  color;
})


############################################################################
# HISTORY:
# 2004-04-21
# o Updated the Rdoc comment since this class had the wrong name.
# 2003-11-21
# o BUG FIX: Typo in getColors().
# 2003-11-19
# o Created.
############################################################################
