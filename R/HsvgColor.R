########################################################################/**
# @RdocClass HsvgColor
#
# @title "Class representing colors in the (hue,saturation,value,gamma) space"
#
# \description{
#  @classhierarchy
#
#  @get "title". 
#
#  Internally all colors are represented as (hue,saturation,value,gamma)
#  in [0,1]x[0,1]x[0,1]x[0,1].
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
#   \item{hue}{A @vector of @numeric in the range [0,\code{maxColorValue}].}
#   \item{saturation}{A @vector of @numeric in the range [0,\code{maxColorValue}].}
#   \item{value}{A @vector of @numeric in the range [0,\code{maxColorValue}].}
#   \item{gamma}{A @vector of @numeric in the range [0,\code{maxColorValue}].}
#   \item{maxColorValue}{A @numeric value specifying the maximum 
#                     value the values in the different channels can take.}
#
#  Moreover, the number of colors will be the same as the length of the
#  longest @vector of the above. If some of the other vectors are shorter, 
#  their values will be looped over to get equal lengths.
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   @see "grDevices::hsv".
# }
#*/#########################################################################
setConstructorS3("HsvgColor", function(hue=NULL, saturation=1, value=1, gamma=1, maxColorValue=1) {
  # If the hue is not given, ignore the other arguments.
  if (length(hue) == 0)
    saturation <- value <- gamma <- NULL;
  extend(FourDimensionalColor(hue=hue, saturation=saturation, value=value, gamma=gamma, maxColorValue=maxColorValue), "HsvgColor",
    # Hue constants
    RED.HUE = 0,
    YELLOW.HUE = 1/6,
    GREEN.HUE = 2/6,
    CYAN.HUE = 3/6,
    BLUE.HUE = 4/6,
    PURPLE.HUE = 5/6
  )
})

setMethodS3("getColors", "HsvgColor", function(this, filter=TRUE, ...) {
  hsvg <- getColorSpace(this);
  h <- hsvg[,"hue"];
  s <- hsvg[,"saturation"];
  v <- hsvg[,"value"];
  g <- hsvg[,"gamma"];
  ok <- is.finite(h);
  col <- rep(NA, length=length(h));
  col[ok] <- hsv(h[ok],s[ok],v[ok], g[ok]);
  if (filter)
    col <- viewThroughColorFilter(this, col);
  col;
})

setMethodS3("plotParallel", "HsvgColor", function(this, index=seq(this), col=matrix(c(getColors(this), rep("black", 3*length(index))), ncol=4)[index,], ...) {
  NextMethod("plotParallel", this, col=col, ...);
})


############################################################################
# HISTORY:
# 2003-12-10
# o Added plotParallel().
# 2003-12-06
# o If the 'hue' is not given, all other arguments are ignored. This makes
#   it easy to create an empty color.
# 2003-11-19
# o Created.
############################################################################
