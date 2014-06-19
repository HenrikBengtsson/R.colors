########################################################################/**
# @RdocClass RgbColor
#
# @title "Class representing colors in the (red,green,blue) space"
#
# \description{
#  @classhierarchy
#
#  @get "title". 
#
#  Internally all colors are represented as (red,green,blue) 
#  in [0,1]x[0,1]x[0,1].
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
#   \item{red}{A @vector of @numeric or color @character string, or a 
#    @see "Color" object. If a @vector of @numeric it specifies
#    the amount of red in the range [0,\code{maxColorValue}].
#    If a @vector of color @characters string, they will be converted
#    into (red,green,blue) in [0,1].
#    If a @see "Color" object, the colors according to \code{getColors()} on
#    that object will be used.}
#   \item{green}{A @vector that specifies the amount of green in 
#    the range [0,\code{maxColorValue}].}
#   \item{blue}{A @vector that specifies the amount of blue in 
#    the range [0,\code{maxColorValue}].}
#   \item{maxColorValue}{A @numeric value specifying the maximum 
#    value the values in the different channels can take.}
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
#   @see "grDevices::rgb".
# }
#*/#########################################################################
setConstructorS3("RgbColor", function(red=NULL, green=NULL, blue=NULL, maxColorValue=1) {
  # If the first argument is the only one...
  if (!is.null(red) && is.null(green) && is.null(blue)) {
    if (inherits(red, "Color")) { 
      red <- getColors(red, filter=FALSE);
    }
    if (is.character(red)) {
      rgb <- col2rgb(red) / 255;
      red   <- rgb["red",];
      green <- rgb["green",];
      blue  <- rgb["blue",];
      rm(rgb);
    }
  }
  extend(ThreeDimensionalColor(red=red, green=green, blue=blue, maxColorValue=maxColorValue), "RgbColor")
})


setMethodS3("getColors", "RgbColor", function(this, filter=TRUE, ...) {
  rgb <- getColorSpace(this);
  col <- rgb(rgb[,"red"], rgb[,"green"], rgb[,"blue"], maxColorValue=1);
  if (filter)
    col <- viewThroughColorFilter(this, col);
  col;
})



#########################################################################/**
# @RdocMethod setChannel
#
# @title "Sets the color values in the specific channel"
#
# @synopsis
#
# \description{
#  @get "title". If there are less values than colors, the values will
#  be looped over.
# }
#
# \arguments{
#  \item{channel}{A @character string specifying which channel to assign to.}
#  \item{value}{A @vector of values to be assigned to the specified channel.}
#  \item{maxColorValue}{Specifying on what range [0,\code{maxColorValue}] the
#    @vector \code{value} is in. All values will be mapped to [0,1]. 
#    This argument exists just for convenience.}
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   See also the "shortcut" methods @seemethod "setRed", 
#   @seemethod "setGreen" and @seemethod "setBlue".
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("setChannel", "RgbColor", function(this, channel, value, maxColorValue=1, ...) {
  if (!is.element(channel, c("red", "green", "blue")))
    throw("Unknown rgb color channel: ", channel);

  value <- rep(value, length.out=length(this));

  if (maxColorValue != 1)
    value <- value / maxColorValue;

  if (any(value < 0 | value > 1, na.rm=TRUE))
    throw("Some color values are out of range.");

  this$colorspace[,channel] <- value;
})


#########################################################################/**
# @RdocMethod setRed
#
# @title "Sets the color values in the red channel"
#
# @synopsis
#
# \description{
#  @get "title". 
#  This is a wrapper method that calls
#  \code{setChannel(obj, channel="red", ...)}.
# }
#
# \arguments{
#  \item{...}{Arguments accepted by @seemethod "setChannel".}
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("setRed", "RgbColor", function(this, ...) {
  setChannel(this, channel="red", ...);
})

#########################################################################/**
# @RdocMethod setGreen
#
# @title "Sets the color values in the green channel"
#
# @synopsis
#
# \description{
#  @get "title". 
#  This is a wrapper method that calls
#  \code{setChannel(obj, channel="green", ...)}.
# }
#
# \arguments{
#  \item{...}{Arguments accepted by @seemethod "setChannel".}
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("setGreen", "RgbColor", function(this, ...) {
  setChannel(this, channel="green", ...);
})

#########################################################################/**
# @RdocMethod setBlue
#
# @title "Sets the color values in the blue channel"
#
# @synopsis
#
# \description{
#  @get "title". 
#  This is a wrapper method that calls
#  \code{setChannel(obj, channel="blue", ...)}.
# }
#
# \arguments{
#  \item{...}{Arguments accepted by @seemethod "setChannel".}
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("setBlue", "RgbColor", function(this, ...) {
  setChannel(this, channel="blue", ...);
})



setMethodS3("plotParallel", "RgbColor", function(this, col=c("red", "green", "blue"), transpose=TRUE, ...) {
  NextMethod("plotParallel", this, col=col, transpose=transpose, ...);
})



############################################################################
# HISTORY:
# 2003-12-09
# o Added plotParallel().
# 2003-11-20
# o Added Rdoc comments.
# 2003-11-19
# o Created.
############################################################################
