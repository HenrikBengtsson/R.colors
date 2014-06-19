#########################################################################/**
# @RdocClass VgaColor
#
# @title "Class representing (categorial) colors in VGA"
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
# @author
#
# \keyword{color}
#
# \references{
#   [1] F. David McRitchie, 
#       \emph{Color Palette and the 56 Excel ColorIndex Colors}, 2005.
#       \url{http://www.mvps.org/dmcritchie/excel/colors.htm}
# }
#
# \seealso{
#   @see "grDevices::palette".
# }
#*/#########################################################################
setConstructorS3("VgaColor", function() {
  availablePalettes <- list(
    default = list(
      black  ="#000000", 
      navy   ="#000080", 
      green  ="#008000", 
      teal   ="#008080", 
      maroon ="#800000", 
      purple ="#800080", 
      olive  ="#808000", 
      silver ="#C0C0C0", 
      gray   ="#808080", 
      blue   ="#0000FF", 
      lime   ="#00FF00", 
      cyan   ="#00FFFF", 
      red    ="#FF0000", 
      magenta="#FF00FF", 
      yellow ="#FFFF00", 
      white  ="#FFFFFF"
    )
  )

  palette <- "default";
  palette <- availablePalettes[[palette]];
  palette <- as.matrix(unlist(palette));

  extend(Color(), "VgaColor",
    palette = palette
  )
})


setMethodS3("getColorSpace", "VgaColor", function(this, ...) {
  this$palette;
})


setMethodS3("getColors", "VgaColor", function(this, filter=TRUE, ...) {
  palette <- this$palette;
  names <- rownames(palette);
  palette <- as.vector(palette);
  names(palette) <- names;
  if (filter)
    palette <- viewThroughColorFilter(this, palette);
  palette;
})


############################################################################
# HISTORY:
# 2005-09-05
# o Created.
############################################################################
