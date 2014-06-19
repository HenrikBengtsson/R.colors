########################################################################/**
# @RdocClass RgbColorFilter
#
# @title "Class representing an RGB color filter"
#
# \description{
#  @classhierarchy
#
#  @get "title". 
#
#  With this filter you can specify which of the red, green and blue 
#  channels you want to let through. One or several channels can be
#  used at once.
# }
#
# \section{Fields and Methods}{
#  @allmethods
# }
#
# @synopsis
# 
# \arguments{
#   \item{channels}{A @vector of @character strings specifying which
#    channels that this filter \emph{will} let through.}
# }
#
# @author
#
# \keyword{color}
#*/#########################################################################
setConstructorS3("RgbColorFilter", function(channels=NULL) {
  extend(ColorFilter(), "RgbColorFilter",
    channels = as.character(channels)
  )
})

setMethodS3("getColors", "RgbColorFilter", function(this, colors, ...) {
  colors <- as.character(colors);
  rgb <- RgbColor(colors);
  rm(colors);
  complementChannels <- setdiff(c("red", "green", "blue"), this$channels);
  for (channel in complementChannels)
    setChannel(rgb, channel=channel, 0);
  getColors(rgb);
})


############################################################################
# HISTORY:
# 2003-11-19
# o Created.
############################################################################
