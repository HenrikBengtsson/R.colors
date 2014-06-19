%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  OneDimensionalColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{OneDimensionalColor}
\docType{class}
\alias{OneDimensionalColor}


\title{Abstract class representing \"one-dimensional\" colors}

\description{
 Package:  R.colors \cr
\bold{Class OneDimensionalColor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{Color}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{OneDimensionalColor}\cr

\bold{Directly known subclasses:}\cr
\link[R.colors]{BlueColor}, \link[R.colors]{CyanMagentaColor}, \link[R.colors]{GrayColor}, \link[R.colors]{GreenColor}, \link[R.colors]{HeatColor}, \link[R.colors]{RainbowColor}, \link[R.colors]{RedColor}, \link[R.colors]{TerrainColor}, \link[R.colors]{TopologyColor}, \link[R.colors]{WavelengthColor}\cr

public static class \bold{OneDimensionalColor}\cr
extends \emph{\link[R.colors]{Color}}\cr



 Abstract class representing \"one-dimensional\" colors.
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.colors:getColorSpace.OneDimensionalColor]{getColorSpace}} \tab Gets the internal representation of the colors.\cr
}


 \bold{Methods inherited from Color}:\cr
[, as.character, display, drawColorRamp, getColorFilter, getColors, getColorSpace, length, palette, plotParallel, seq, setColorFilter, viewThroughColorFilter

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\usage{OneDimensionalColor(..., maxColorValue=1)}

\arguments{
  \item{...}{\emph{One} (preferably named) \code{\link[base]{vector}} arguments of color
    values in the range [0,\code{maxColorValue}].}
  \item{maxColorValue}{\code{\link[base]{numeric}} specifying maximum color value possible.}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}
\keyword{classes}
