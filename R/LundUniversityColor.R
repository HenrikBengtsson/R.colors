#########################################################################/**
# @RdocClass LundUniversityColor
#
# @title "Class representing colors according to the Lund University
#         Visual Identity Programme"
#
# \description{
#  @classhierarchy
#
#  @get "title" [1]. 
#  This programme or profile specifies four colors, 
#  a blue (\code{"lublue"}), a brown (\code{"lubrown"}),
#  a plain white (\code{"luwhite"}) and a plain 
#  black (\code{"lublack"}). 
#  The last two are included just for convenience.
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
#   \item{palette}{@character string specifying which palette to use.
#     By default, the \code{"strict"}, i.e. the formal, palette is used.
#     For on screen presentations and web pages, \code{"websoft"} provides
#     less intense colors and the \code{"websafe"} provides colors from
#     the so called 216 websafe colors.}
# }
#
# @author
#
# \keyword{color}
#
# \references{
#   [1] Lund University Visual Identity Programme, 2000.
#       \url{http://www.lu.se/LUinternt/grafiskprofil/english/}
# }
#
# \seealso{
#   @see "grDevices::palette".
# }
#*/#########################################################################
setConstructorS3("LundUniversityColor", function(palette=NULL) {
  availablePalettes <- list(
    strict = list(
      lublue  = "#0C2577",   # PMS=1395, CMYK=(0,40,100,30), NCS=S4050-Y20R
      lubrown = "#9E5A00",   # PMS= 280, CMYK=(100,60,0,10), NCS=S5040-R70B
      luwhite = "#FFFFFF",
      lublack = "#000000"    # PMS=black, CMYK=100% black, NCS=S9500-N
    ),
    websoft = list(
      lublue  = "#0C2577",
      lubrown = "#A57B52",
      luwhite = "#FFFFFF",
      lublack = "#000000"
    ),
    websafe = list(
      lublue  = "#003399",
      lubrown = "#996600",
      luwhite = "#FFFFFF",
      lublack = "#000000"
    )
  )
  if (is.null(palette))
    palette <- "strict";

  if (!is.element(palette, names(availablePalettes)))
    throw("Unknown 'palette': ", palette);

  palette <- availablePalettes[[palette]];
  palette <- as.matrix(unlist(palette));

  extend(Color(), "LundUniversityColor",
    palette = palette
  )
})


setMethodS3("getColorSpace", "LundUniversityColor", function(this, ...) {
  this$palette;
})


setMethodS3("getColors", "LundUniversityColor", function(this, filter=TRUE, ...) {
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
# 2002-11-21
# o Created.
############################################################################
