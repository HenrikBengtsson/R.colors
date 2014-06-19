########################################################################/**
# @RdocClass HclColor
#
# \encoding{latin1}
#
# @title "Class representing colors in the (hue,chroma,luminance) space"
#
# \description{
#  @classhierarchy
#
#  @get "title". 
#
#  Internally all colors are represented as (hue,chroma,luminance)
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
#   \item{hue}{A @vector of @numeric in the range [0,\code{maxColorValue}].}
#   \item{chroma}{(colorfulness) A @vector of @numeric in the range [0,\code{maxColorValue}].}
#   \item{luminance}{A @vector of @numeric in the range [0,\code{maxColorValue}].}
#   \item{maxColorValue}{A @numeric value specifying the maximum 
#                     value the values in the different channels can take.}
#   \item{correctOutOfRange}{By default, if @TRUE, generated colors that fall 
#     outside of the valid (red,green,blue) space, will be truncated. 
#     Otherwise, they will be set to @NA.}
#
#  Moreover, the number of colors will be the same as the length of the
#  longest @vector of the above. If some of the other vectors are shorter, 
#  their values will be looped over to get equal lengths.
# }
#
# \details{
#   The creation of this Color class was strongly inspired by Ross Ihaka's talk
#   at DSC-2003 [1]
# }
#
# @author
#
# \keyword{color}
#
# \references{
#  [1] Ross Ihaka, \emph{Colour for Presentation Graphics}, Proceedings of the 
#      3rd International Workshop on Distributed Statistical Computing (DSC 2003),
#      March 20-22, 2003, Technische Universität Wien, Vienna, Austria.
#      \url{http://www.ci.tuwien.ac.at/Conferences/DSC-2003/}.\cr
#  [2] Ross Ihaka, \emph{Perceptually-Based Color Choices},
#     webpage, \url{http://www.stat.auckland.ac.nz/~ihaka/colour/}\cr
#  [3] Ross Ihaka, \emph{Statistics 120 - Information Visualisation}, 
#     webpage, \url{http://www.stat.auckland.ac.nz/~ihaka/120/}.\cr
# }
#
# \seealso{
#   See also @see "grDevices::hcl" in the \code{colorspace} package [1-3].
# }
#*/#########################################################################
setConstructorS3("HclColor", function(hue=NULL, chroma=1, luminance=1, maxColorValue=1, correctOutOfRange=TRUE) {
  extend(ThreeDimensionalColor(hue=hue, chroma=chroma, luminance=luminance, maxColorValue=maxColorValue), "HclColor",
    correctOutOfRange = as.logical(correctOutOfRange)
  )
})




#########################################################################/**
# @RdocMethod getColors
#
# @title "Gets the set of colors in the \#rrggbb format"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# \arguments{
#   \item{correctOutOfRange}{If @TRUE, generated colors that fall outside
#     of the valid (red,green,blue) space, will be truncated. 
#     Otherwise, they will be set to @NA.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @vector of @character strings of length equal to the number of 
#  colors in the Color object. The returned colors are character string of
#  format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
#  For more information about this format @see "grDevices::colors".
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
setMethodS3("getColors", "HclColor", function(this, filter=TRUE, correctOutOfRange=this$correctOutOfRange, ...) {
  colorspace <- getColorSpace(this);
  hue       <- 360 * colorspace[,"hue"];
  chroma    <- 100 * colorspace[,"chroma"];
  luminance <- 100 * colorspace[,"luminance"];

  # Utilizing Ross Ihaka's hcl() function in the 'colorspace' package
  col <- hcl(h=hue, c=chroma, l=luminance, fixup=correctOutOfRange);
  if (filter)
    col <- viewThroughColorFilter(this, col);
  col;
})


setMethodS3("plotParallel", "HclColor", function(this, index=seq(this), col=matrix(c(getColors(this), rep("black", 2*length(index))), ncol=3)[index,], ...) {
  NextMethod("plotParallel", this, col=col, ...);
})


############################################################################
# HISTORY:
# 2010-11-28
# o hcl() is now in grDevices.  Used to be in the colorspace package.
# 2007-05-09
# o Remove 'deviceGamma' argument.
# 2003-12-10
# o Added plotParallel().
# 2003-11-19
# o Created.
############################################################################
