%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ColorFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{ColorFilter}
\docType{class}
\alias{ColorFilter}


 \title{Class representing a color filter}

 \description{
  Package:  R.colors \cr
\bold{Class ColorFilter}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{ColorFilter}\cr

\bold{Directly known subclasses:}\cr
\link[R.colors]{DichromatColorFilter}, \link[R.colors]{GrayColorFilter}, \link[R.colors]{MonochromeColorFilter}, \link[R.colors]{NegativeColorFilter}, \link[R.colors]{RgbColorFilter}, \link[R.colors]{SerialColorFilter}\cr

public static class \bold{ColorFilter}\cr
extends \link[R.oo]{Object}\cr



  Class representing a color filter.

  This "mother" of ColorFilter classes works also as a "transparent"
  filter, i.e. as a filter that does not filter at all.
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.colors:getColors.ColorFilter]{getColors}} \tab Filters a set of colors.\cr
}


 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \usage{ColorFilter()}

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

filters <- list(
  none      = ColorFilter(),  # or NULL
  red       = RgbColorFilter(c("red")),
  green     = RgbColorFilter(c("green")),
  blue      = RgbColorFilter(c("blue")),
  redgreen  = RgbColorFilter(c("red", "green")),
  redblue   = RgbColorFilter(c("red", "blue")),
  greenblue = RgbColorFilter(c("green", "blue")),
  gray      = GrayColorFilter(),
  neg       = NegativeColorFilter(),
  mono      = MonochromeColorFilter()
)

if ("dichromat" \%in\% library()$results) {
  filter <- append(filter, list(
    deutan    = DichromatColorFilter(type="deutan"),
    protan    = DichromatColorFilter(type="protan")
  ))
}

ofilter <- Color$setColorFilter(filters$gray)

for(color in colors)
  display(color)

par(opar)
Color$setColorFilter(ofilter)
}

 \keyword{color}

 \seealso{
   \code{\link{Color}}.
 }
\keyword{classes}
