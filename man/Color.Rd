%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Color.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{Color}
\docType{class}
\alias{Color}


\title{Abstract class representing a set of colors}

\description{
 Package:  R.colors \cr
\bold{Class Color}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\emph{\code{Color}}\cr

\bold{Directly known subclasses:}\cr
\link[R.colors]{BlueColor}, \link[R.colors]{CmykColor}, \link[R.colors]{CyanMagentaColor}, \link[R.colors]{ExcelColor}, \link[R.colors]{FourDimensionalColor}, \link[R.colors]{GrayColor}, \link[R.colors]{GreenColor}, \link[R.colors]{HclColor}, \link[R.colors]{HeatColor}, \link[R.colors]{HsvgColor}, \link[R.colors]{LundUniversityColor}, \link[R.colors]{OneDimensionalColor}, \link[R.colors]{RainbowColor}, \link[R.colors]{RedColor}, \link[R.colors]{RgbColor}, \link[R.colors]{TerrainColor}, \link[R.colors]{ThreeDimensionalColor}, \link[R.colors]{TopologyColor}, \link[R.colors]{TwoChannelMicroarrayColor}, \link[R.colors]{TwoDimensionalColor}, \link[R.colors]{VgaColor}, \link[R.colors]{WavelengthColor}\cr

public abstract static class \bold{Color}\cr
extends \link[R.oo]{Object}\cr



 Abstract class representing a set of colors.

 Various Color classes can have various ways of representing a set of
 colors. Where some classes store the colors in a colorspace spanning the
 red, green, and blue dimensions, other classes may keep a different
 representation such as the wavelength of the light and so on.

 By calling \code{getColors()} of a Color object the set of colors where
 each color is a character string with format \code{"\#rrggbb"}, where
 \code{rr}, \code{gg}, and \code{bb} are hexadecimal values in
 [0x00-0xff] for the red, green and blue channel, respectively. This
 format is understood by all plot functions in \R.
 By default, \code{as.character()} calls \code{getColors()}.
 It is expected that \emph{no} subclass changes this.

 \emph{Importantly}, each class implementing its own \code{getColors()}
 should before returning the set of colors, pass them through the
 user-defined color filter. This is done easiest by calling
 \code{colors <- viewThroughColorFilter(colors)} before returning, where
 \code{colors} is the set of colors to be filtered. If the user has not
 specified a default color filter, the identical colors will be returned.
 This is very useful because any color can then be filtered without
 the user having to do it explicitly. For instance, if you want to filter
 all the colors to see how a "color blind" person preceives the colors,
 set the default color filter by calling the static method as
 \code{Color$setColorFilter(DichromatColorFilter())}.
 For more information about color filters, \code{\link{ColorFilter}}.
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{[} \tab  -\cr
 \tab \code{\link[R.colors:as.character.Color]{as.character}} \tab Gets a character representation of the colors.\cr
 \tab \code{colnames} \tab  -\cr
 \tab \code{\link[R.colors:display.Color]{display}} \tab Displays the current set of colors.\cr
 \tab \code{\link[R.colors:drawColorRamp.Color]{drawColorRamp}} \tab Draw a color ramp of the colors in the current plot.\cr
 \tab \code{\link[R.colors:getColorFilter.Color]{getColorFilter}} \tab Gets the default color filter.\cr
 \tab \code{\link[R.colors:getColors.Color]{getColors}} \tab Gets the set of colors in the \#rrggbb format.\cr
 \tab \code{\link[R.colors:getColorSpace.Color]{getColorSpace}} \tab Gets the internal representation of the colors.\cr
 \tab \code{\link[R.colors:image.Color]{image}} \tab Creates an image of the current set of colors.\cr
 \tab \code{\link[R.colors:imageText.Color]{imageText}} \tab Adds text to the last plot.\cr
 \tab \code{\link[R.colors:length.Color]{length}} \tab Gets the number of colors.\cr
 \tab \code{\link[R.colors:palette.Color]{palette}} \tab Use these colors as the color palette in all graphics.\cr
 \tab \code{\link[R.colors:plotParallel.Color]{plotParallel}} \tab Plots the column vectors in the colorspace matrix versus the index.\cr
 \tab \code{\link[R.colors:seq.Color]{seq}} \tab Gets the indices of all colors.\cr
 \tab \code{\link[R.colors:setColorFilter.Color]{setColorFilter}} \tab Sets the default color filter.\cr
 \tab \code{validateArgumentColorsK} \tab  -\cr
 \tab \code{\link[R.colors:viewThroughColorFilter.Color]{viewThroughColorFilter}} \tab Filters a set of colors using the default filter.\cr
}


 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\usage{Color()}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
# One-dimensional colors
ncolors <- 256
x <- seq(0,1, length=ncolors)
ch1 <- x
ch2 <- rev(x)

colors <- list(
  # One-dimensional colors
  red      = RedColor(x),
  green    = GreenColor(x),
  blue     = BlueColor(x),
  gray     = GrayColor(x),
  spectrum = WavelengthColor(seq(380,779,length=ncolors)),
  heat     = HeatColor(x),
  rainbow  = RainbowColor(x),
  topo     = TopologyColor(x),
  terrain  = TerrainColor(x),
  cm       = CyanMagentaColor(x),

  # Two-dimensional colors
  rg = TwoChannelMicroarrayColor(ch1,ch2, maxColorValue=1),
  yb = TwoChannelMicroarrayColor(ch1,ch2, hueRange=c(HsvgColor$YELLOW.HUE,HsvgColor$BLUE.HUE), maxColorValue=1),
 
  # Three-dimensional colors
  rgb  = RgbColor(x,rev(x),sample(x)),
  hcl  = HclColor(x),

  # Four-dimensional colors
  hsvg = HsvgColor(x),
  cmyk = CmykColor(x,sample(x),rev(x),x/2)
)

layout(matrix(1:16, ncol=4, byrow=TRUE))
opar <- par(mar=c(0,1,3,1)+0.5)

for(color in colors)
  display(color)

par(opar)
}

\keyword{color}

\references{
 [1] cyberglitz, \emph{Color Primer}, webpage, 2003.
     \url{http://www.cyberglitz.com/primer.htm}
 [2] ColorMatch 5K,
     \url{http://www.colormatch.dk/}.
     First prize in the Danish 5K Awards 2002.
}

\seealso{
  \code{\link{ColorFilter}}.
  \code{\link[grDevices]{colors}}.
}
\keyword{classes}
