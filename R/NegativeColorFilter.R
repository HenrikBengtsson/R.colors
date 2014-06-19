########################################################################/**
# @RdocClass NegativeColorFilter
#
# @title "Color filter that inverts (negatates) colors"
#
# \description{
#  @classhierarchy
#
#  @get "title". 
# }
#
# \section{Fields and Methods}{
#  @allmethods
# }
#
# @synopsis
# 
# @author
#
# \details{
#   This color filter takes the (red,green,blue) representation in 
#   [0,1]x[0,1]x[0,1] of the colors and invert the colors by
#   (1-red,1-green,1-blue). There might be other definitions of how
#   the invert colors, but this was just a quick hack.
# }
#
# \keyword{color}
#*/#########################################################################
setConstructorS3("NegativeColorFilter", function() {
  extend(ColorFilter(), "NegativeColorFilter"
  )
})


setMethodS3("getColors", "NegativeColorFilter", function(this, colors, ...) {
  colors <- as.character(colors);
  rgb <- col2rgb(colors);
  rgb <- rgb / 255;
  rgb <- 1 - rgb;
  getColors(RgbColor(rgb["red",],rgb["green",],rgb["blue",]));
})


############################################################################
# HISTORY:
# 2003-11-19
# o Created.
############################################################################
