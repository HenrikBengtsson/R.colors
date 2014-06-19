%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  CmykColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{CmykColor}
\docType{class}
\alias{CmykColor}


\title{Class representing colors in the (cyan,magenta,yellow,black) space}

\description{
 Package:  R.colors \cr
\bold{Class CmykColor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{Color}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link[R.colors]{FourDimensionalColor}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{CmykColor}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{CmykColor}\cr
extends \link[R.colors]{FourDimensionalColor}\cr



 Class representing colors in the (cyan,magenta,yellow,black) space.

 Internally all colors are represented as (cyan,magenta,yellow,black)
 in [0,1]x[0,1]x[0,1]x[0,1].
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{asRgbColor} \tab  -\cr
 \tab \code{getColors} \tab  -\cr
 \tab \code{\link[R.colors:maximizeBlack.CmykColor]{maximizeBlack}} \tab Maximizes the black channel to 'save' the other colors.\cr
 \tab \code{plotParallel} \tab  -\cr
 \tab \code{\link[R.colors:setChannel.CmykColor]{setChannel}} \tab Sets the color values in the specific channel.\cr
}


 \bold{Methods inherited from FourDimensionalColor}:\cr
getColorSpace

 \bold{Methods inherited from Color}:\cr
[, as.character, display, drawColorRamp, getColorFilter, getColors, getColorSpace, length, palette, plotParallel, seq, setColorFilter, viewThroughColorFilter

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\usage{CmykColor(cyan=NULL, magenta=NULL, yellow=NULL, black=NULL, maxColorValue=1)}

\arguments{
  \item{cyan}{A \code{\link[base]{vector}} of \code{\link[base]{numeric}}, or a \code{\link{Color}} object.
   If a \code{\link[base]{vector}} of \code{\link[base]{numeric}} it specifies
   the amount of cyan in the range [0,\code{maxColorValue}].
   If a \code{\link{Color}} object, the colors according to \code{getColors()} on
   that object will be used.}
  \item{magenta}{A \code{\link[base]{vector}} that specifies the amount of magenta in
   the range [0,\code{maxColorValue}].}
  \item{yellow}{A \code{\link[base]{vector}} that specifies the amount of yellow in
   the range [0,\code{maxColorValue}].}
  \item{black}{A \code{\link[base]{vector}} that specifies the amount of black in
   the range [0,\code{maxColorValue}].}
  \item{maxColorValue}{A \code{\link[base]{numeric}} value specifying the maximum
   value the values in the different channels can take.}

 Moreover, the number of colors will be the same as the length of the
 longest \code{\link[base]{vector}} of the above. If some of the other vectors are shorter,
 their values will be looped over to get equal lengths.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\references{
 [1] Borland Developer Support Staff, \emph{Converting a RGB color to a
     CMYK color}, \url{http://community.borland.com/article/0,1410,17948,00.html}
}
\keyword{classes}
