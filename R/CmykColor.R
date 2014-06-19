########################################################################/**
# @RdocClass CmykColor
#
# @title "Class representing colors in the (cyan,magenta,yellow,black) space"
#
# \description{
#  @classhierarchy
#
#  @get "title". 
#
#  Internally all colors are represented as (cyan,magenta,yellow,black)
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
#   \item{cyan}{A @vector of @numeric, or a @see "Color" object. 
#    If a @vector of @numeric it specifies
#    the amount of cyan in the range [0,\code{maxColorValue}].
#    If a @see "Color" object, the colors according to \code{getColors()} on
#    that object will be used.}
#   \item{magenta}{A @vector that specifies the amount of magenta in 
#    the range [0,\code{maxColorValue}].}
#   \item{yellow}{A @vector that specifies the amount of yellow in 
#    the range [0,\code{maxColorValue}].}
#   \item{black}{A @vector that specifies the amount of black in 
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
# \references{
#  [1] Borland Developer Support Staff, \emph{Converting a RGB color to a
#      CMYK color}, \url{http://community.borland.com/article/0,1410,17948,00.html}
# }
#
#*/#########################################################################
setConstructorS3("CmykColor", function(cyan=NULL, magenta=NULL, yellow=NULL, black=NULL, maxColorValue=1) {
  # If the first argument is the only one...
  if (!is.null(cyan) && is.null(magenta) && is.null(yellow) && is.null(black)) {
    if (inherits(cyan, "Color")) { 
      cyan <- getColors(cyan, filter=FALSE);
    }
    if (is.character(cyan)) {
      rgb <- col2rgb(cyan) / 255;
      rm(cyan);
      rgb <- t(rgb);
      cmy <- 1-rgb;
      rm(rgb);
      k <- apply(cmy, MARGIN=1, FUN=min);
      cmy <- cmy - k;
      cyan <- cbind(cmy,k);
      colnames(cyan) <- c("cyan", "magenta", "yellow", "black");
      rm(cmy,k);
    } else {
      throw("Unsupported value.");
    }
  }

  extend(FourDimensionalColor(cyan=cyan, magenta=magenta, yellow=yellow, black=black, maxColorValue=maxColorValue), "CmykColor")
})


setMethodS3("asRgbColor", "CmykColor", function(this, ...) {
  # [1] http://community.borland.com/article/0,1410,17948,00.html
  cmyk <- getColorSpace(this);

  rgb <- matrix(NA, nrow=nrow(cmyk), ncol=3);
  colnames(rgb) <- c("red", "green", "blue");

  inChannels <- c("cyan", "magenta", "yellow");
  for (kk in 1:3) {
    inChannel  <- inChannels[kk];
    tmp <- (cmyk[,inChannel] + cmyk[,"black"]);
    strong <- (tmp >= 1);
    rgb[strong,kk] <- 0;
    rgb[!strong,kk] <- 1 - tmp[!strong];
  }

  RgbColor(rgb);
})


#########################################################################/**
# @RdocMethod maximizeBlack
#
# @title "Maximizes the black channel to 'save' the other colors"
#
# @synopsis
#
# \description{
#  @get "title". 
# 
#  In printing, black tones can be achieved by printing cyan, magenta and
#  yellow in equal amounts. Since color toners often are more expensive than
#  black toners, by adding black one can reduce the other colors, but still
#  print exactly the same color. 
#  This method minimized the use of cyan, magenta and yellow, by compensating
#  with black.
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
setMethodS3("maximizeBlack", "CmykColor", function(this, ...) {
  cmyk <- getColorSpace(this);
  cmyIdx <- c("cyan","magenta","yellow");
  minColor <- apply(cmyk[,cmyIdx], MARGIN=1, FUN=min);
  idx <- (minColor + cmyk[,"black"] > 1);
  minColor[idx] <- 1 - cmyk[idx,"black"];
  cmyk[,cmyIdx] <- cmyk[,cmyIdx] - minColor;
  cmyk[,"black"] <- cmyk[,"black"] + minColor;
  this$colorspace <- cmyk;
}, static=TRUE)



setMethodS3("getColors", "CmykColor", function(this, filter=TRUE, ...) {
  col <- asRgbColor(this);
  if (filter) {
    col <- viewThroughColorFilter(this, col);
  } else {
    col <- getColors(col);
  }
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
#   \item{...}{Not used.}
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
setMethodS3("setChannel", "CmykColor", function(this, channel, value, maxColorValue=1, ...) {
  if (!is.element(channel, c("red", "green", "blue")))
    throw("Unknown cmyk color channel: ", channel);

  value <- rep(value, length.out=length(this));

  if (maxColorValue != 1)
    value <- value / maxColorValue;

  if (any(value < 0 | value > 1, na.rm=TRUE))
    throw("Some color values are out of range.");

  this$cmyk[,channel] <- value;
})


setMethodS3("plotParallel", "CmykColor", function(this, col=c("cyan", "magenta", "yellow", "black"), transpose=TRUE, ...) {
  NextMethod("plotParallel", this, col=col, transpose=transpose, ...);
})


############################################################################
# HISTORY:
# 2003-12-10
# o Added plotParallel().
# 2003-11-20
# o Added Rdoc comments.
# 2003-11-19
# o Created.
############################################################################
