%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  TwoDimensionalColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{TwoDimensionalColor}
\docType{class}
\alias{TwoDimensionalColor}


\title{Abstract class representing \"two-dimensional\" colors}

\description{
 Package:  R.colors \cr
\bold{Class TwoDimensionalColor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{Color}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{TwoDimensionalColor}\cr

\bold{Directly known subclasses:}\cr
\link[R.colors]{TwoChannelMicroarrayColor}\cr

public static class \bold{TwoDimensionalColor}\cr
extends \emph{\link[R.colors]{Color}}\cr



 Abstract class representing \"two-dimensional\" colors.
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.colors:getColorSpace.TwoDimensionalColor]{getColorSpace}} \tab Gets the internal representation of the colors.\cr
 \tab \code{plot} \tab  -\cr
}


 \bold{Methods inherited from Color}:\cr
[, as.character, display, drawColorRamp, getColorFilter, getColors, getColorSpace, length, palette, plotParallel, seq, setColorFilter, viewThroughColorFilter

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\usage{TwoDimensionalColor(..., maxColorValue=1)}

\arguments{
  \item{...}{\emph{Two} (preferably named) \code{\link[base]{vector}} arguments of color
    values in the range [0,\code{maxColorValue}].}
  \item{maxColorValue}{\code{\link[base]{numeric}} specifying maximum color value possible.}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}
\keyword{classes}
