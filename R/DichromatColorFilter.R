########################################################################/**
# @RdocClass DichromatColorFilter
#
# @title "Color filter emulating dichromatic color blindness"
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
# \arguments{
#   \item{type}{A @character string specifying the type of color blindness
#    to emulate. See @see "dichromat::dichromat" [1] for more details.}
# }
#
# \author{Henrik Bengtsson, but all credits to Thomas Lumley who wrote the
#  \code{dichromat} package, which this class utilizes.}
#
# \details{
#   This color filter requires that the \code{dichromat} package [1] is
#   installed. The easiest way to install this is by calling
#   \code{install.packages("dichromat")}.
#
#   Color blindness: 
#    1) Protanope   (L-cone - red cone cells defective), 
#    2) Deuteranope (M-cone - green cone cells defective), and 
#    3) Tritanope   (S-cone - blue cone cells defective).
#
#   For a dichromat safe color palette, see [4].
#
#   To modify an image such that its colors are more accessable
#   to color blind people see [3].
# }
#
# \references{
#   [1] Thomas Lumley, \emph{R package \code{dichromat}}, CRAN.
#       \url{http://cran.r-project.org/}.\cr
#   [2] Vischeck, webpage, 2003. \url{http://www.vischeck.com/}\cr
#   [3] Daltonize, webpage, 2003. \url{http://www.vischeck.com/daltonize/}\cr
#   [4] Masataka Okabe and Kei Ito, \cr
#       \emph{Barrier-free presentation that is friendly to colorblind}, 
#       \url{http://jfly.iam.u-tokyo.ac.jp/html/color_blind/}\cr
# }
#
# \keyword{color}
#
# \seealso{
#   See also the @see "dichromat::dichromat" package [1].
# }
#
#*/#########################################################################
setConstructorS3("DichromatColorFilter", function(type=c("deutan", "protan")) {
  type <- match.arg(type);
  extend(ColorFilter(), "DichromatColorFilter",
    type = type
  )
})


setMethodS3("getColors", "DichromatColorFilter", function(this, colors, type=this$type, ...) {
  require(dichromat) || throw("Package 'dichromat' not found. Try install.packages(\"dichromat\").");

  colors <- as.character(colors);
  colors <- dichromat(colors, type=type);
  colors;
})


############################################################################
# HISTORY:
# 2003-11-19
# o Created.
############################################################################
