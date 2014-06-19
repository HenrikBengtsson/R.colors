%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  HsvgColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{HsvgColor}
\docType{class}
\alias{HsvgColor}


\title{Class representing colors in the (hue,saturation,value,gamma) space}

\description{
 Package:  R.colors \cr
\bold{Class HsvgColor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{Color}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link[R.colors]{FourDimensionalColor}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{HsvgColor}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{HsvgColor}\cr
extends \link[R.colors]{FourDimensionalColor}\cr



 Class representing colors in the (hue,saturation,value,gamma) space.

 Internally all colors are represented as (hue,saturation,value,gamma)
 in [0,1]x[0,1]x[0,1]x[0,1].
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{getColors} \tab  -\cr
 \tab \code{plotParallel} \tab  -\cr
}


 \bold{Methods inherited from FourDimensionalColor}:\cr
getColorSpace

 \bold{Methods inherited from Color}:\cr
[, as.character, display, drawColorRamp, getColorFilter, getColors, getColorSpace, length, palette, plotParallel, seq, setColorFilter, viewThroughColorFilter

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\usage{HsvgColor(hue=NULL, saturation=1, value=1, gamma=1, maxColorValue=1)}

\arguments{
  \item{hue}{A \code{\link[base]{vector}} of \code{\link[base]{numeric}} in the range [0,\code{maxColorValue}].}
  \item{saturation}{A \code{\link[base]{vector}} of \code{\link[base]{numeric}} in the range [0,\code{maxColorValue}].}
  \item{value}{A \code{\link[base]{vector}} of \code{\link[base]{numeric}} in the range [0,\code{maxColorValue}].}
  \item{gamma}{A \code{\link[base]{vector}} of \code{\link[base]{numeric}} in the range [0,\code{maxColorValue}].}
  \item{maxColorValue}{A \code{\link[base]{numeric}} value specifying the maximum
                    value the values in the different channels can take.}

 Moreover, the number of colors will be the same as the length of the
 longest \code{\link[base]{vector}} of the above. If some of the other vectors are shorter,
 their values will be looped over to get equal lengths.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  \code{\link[grDevices]{hsv}}.
}
\keyword{classes}
