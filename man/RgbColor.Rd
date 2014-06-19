%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  RgbColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{RgbColor}
\docType{class}
\alias{RgbColor}


\title{Class representing colors in the (red,green,blue) space}

\description{
 Package:  R.colors \cr
\bold{Class RgbColor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{Color}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link[R.colors]{ThreeDimensionalColor}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{RgbColor}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{RgbColor}\cr
extends \link[R.colors]{ThreeDimensionalColor}\cr



 Class representing colors in the (red,green,blue) space.

 Internally all colors are represented as (red,green,blue)
 in [0,1]x[0,1]x[0,1].
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{getColors} \tab  -\cr
 \tab \code{plotParallel} \tab  -\cr
 \tab \code{\link[R.colors:setBlue.RgbColor]{setBlue}} \tab Sets the color values in the blue channel.\cr
 \tab \code{\link[R.colors:setChannel.RgbColor]{setChannel}} \tab Sets the color values in the specific channel.\cr
 \tab \code{\link[R.colors:setGreen.RgbColor]{setGreen}} \tab Sets the color values in the green channel.\cr
 \tab \code{\link[R.colors:setRed.RgbColor]{setRed}} \tab Sets the color values in the red channel.\cr
}


 \bold{Methods inherited from ThreeDimensionalColor}:\cr
getColorSpace, plot3d

 \bold{Methods inherited from Color}:\cr
[, as.character, display, drawColorRamp, getColorFilter, getColors, getColorSpace, length, palette, plotParallel, seq, setColorFilter, viewThroughColorFilter

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\usage{RgbColor(red=NULL, green=NULL, blue=NULL, maxColorValue=1)}

\arguments{
  \item{red}{A \code{\link[base]{vector}} of \code{\link[base]{numeric}} or color \code{\link[base]{character}} string, or a
   \code{\link{Color}} object. If a \code{\link[base]{vector}} of \code{\link[base]{numeric}} it specifies
   the amount of red in the range [0,\code{maxColorValue}].
   If a \code{\link[base]{vector}} of color \code{\link[base]{character}}s string, they will be converted
   into (red,green,blue) in [0,1].
   If a \code{\link{Color}} object, the colors according to \code{getColors()} on
   that object will be used.}
  \item{green}{A \code{\link[base]{vector}} that specifies the amount of green in
   the range [0,\code{maxColorValue}].}
  \item{blue}{A \code{\link[base]{vector}} that specifies the amount of blue in
   the range [0,\code{maxColorValue}].}
  \item{maxColorValue}{A \code{\link[base]{numeric}} value specifying the maximum
   value the values in the different channels can take.}

 Moreover, the number of colors will be the same as the length of the
 longest \code{\link[base]{vector}} of the above. If some of the other vectors are shorter,
 their values will be looped over to get equal lengths.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  \code{\link[grDevices]{rgb}}.
}
\keyword{classes}
