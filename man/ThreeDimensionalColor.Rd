%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ThreeDimensionalColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{ThreeDimensionalColor}
\docType{class}
\alias{ThreeDimensionalColor}


\title{Abstract class representing \"three-dimensional\" colors}

\description{
 Package:  R.colors \cr
\bold{Class ThreeDimensionalColor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{Color}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{ThreeDimensionalColor}\cr

\bold{Directly known subclasses:}\cr
\link[R.colors]{HclColor}, \link[R.colors]{RgbColor}\cr

public static class \bold{ThreeDimensionalColor}\cr
extends \emph{\link[R.colors]{Color}}\cr



 Abstract class representing \"three-dimensional\" colors.
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.colors:getColorSpace.ThreeDimensionalColor]{getColorSpace}} \tab Gets the internal representation of the colors.\cr
 \tab \code{\link[R.colors:plot3d.ThreeDimensionalColor]{plot3d}} \tab Plots the colors in a three-dimensional plot.\cr
}


 \bold{Methods inherited from Color}:\cr
[, as.character, display, drawColorRamp, getColorFilter, getColors, getColorSpace, length, palette, plotParallel, seq, setColorFilter, viewThroughColorFilter

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\usage{ThreeDimensionalColor(..., maxColorValue=1)}

\arguments{
  \item{...}{\emph{Three} (preferably named) \code{\link[base]{vector}} arguments of color
    values in the range [0,\code{maxColorValue}].}
  \item{maxColorValue}{\code{\link[base]{numeric}} specifying maximum color value possible.}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}
\keyword{classes}
