#########################################################################/**
# @RdocClass ExcelColor
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
setConstructorS3("ExcelColor", function() {
  availablePalettes <- list(
    default = list(
      "black"  ="#000000",
      "white"  ="#FFFFFF",
      "red"    ="#FF0000",
      "green"  ="#00FF00",
      "blue"   ="#0000FF",
      "yellow" ="#FFFF00",
      "magenta"="#FF00FF",
      "cyan"   ="#00FFFF",
      "color9" ="#800000",
      "color10"="#008000",
      "color11"="#000080",
      "color12"="#808000",
      "color13"="#800080",
      "color14"="#008080",
      "color15"="#C0C0C0",
      "color16"="#808080",
      "color17"="#9999FF",
      "color18"="#993366",
      "color19"="#FFFFCC",
      "color20"="#CCFFFF",
      "color21"="#660066",
      "color22"="#FF8080",
      "color23"="#0066CC",
      "color24"="#CCCCFF",
      "color25"="#000080",
      "color26"="#FF00FF",
      "color27"="#FFFF00",
      "color28"="#00FFFF",
      "color29"="#800080",
      "color30"="#800000",
      "color31"="#008080",
      "color32"="#0000FF",
      "color33"="#00CCFF",
      "color34"="#CCFFFF",
      "color35"="#CCFFCC",
      "color36"="#FFFF99",
      "color37"="#99CCFF",
      "color38"="#FF99CC",
      "color39"="#CC99FF",
      "color40"="#FFCC99",
      "color41"="#3366FF",
      "color42"="#33CCCC",
      "color43"="#99CC00",
      "color44"="#FFCC00",
      "color45"="#FF9900",
      "color46"="#FF6600",
      "color47"="#666699",
      "color48"="#969696",
      "color49"="#003366",
      "color50"="#339966",
      "color51"="#003300",
      "color52"="#333300",
      "color53"="#993300",
      "color54"="#993366",
      "color55"="#333399",
      "color56"="#333333"
    )
  )

  palette <- "default";
  palette <- availablePalettes[[palette]];
  palette <- as.matrix(unlist(palette));

  extend(Color(), "ExcelColor",
    palette = palette
  )
})


setMethodS3("getColorSpace", "ExcelColor", function(this, ...) {
  this$palette;
})


setMethodS3("getColors", "ExcelColor", function(this, filter=TRUE, ...) {
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
