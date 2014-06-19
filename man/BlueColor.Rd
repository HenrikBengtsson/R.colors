%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  BlueColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{BlueColor}
\docType{class}
\alias{BlueColor}


\title{Class representing blue colors}

\description{
 Package:  R.colors \cr
\bold{Class BlueColor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{Color}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link[R.colors]{OneDimensionalColor}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{BlueColor}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{BlueColor}\cr
extends \link[R.colors]{OneDimensionalColor}\cr



 Class representing blue colors.
}

\section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{getColors} \tab  -\cr
}


 \bold{Methods inherited from OneDimensionalColor}:\cr
getColorSpace

 \bold{Methods inherited from Color}:\cr
[, as.character, display, drawColorRamp, getColorFilter, getColors, getColorSpace, length, palette, plotParallel, seq, setColorFilter, viewThroughColorFilter

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\usage{BlueColor(blue=NULL, ...)}

\arguments{
  \item{blue}{\code{\link[base]{vector}} of levels of blue in range [0,\code{maxColorValue}]
    where \code{maxColorValue} is given by the argument with the same
    name in the constructor of the super class.}
  \item{...}{Arguments passed to the constructor of the super class.}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}
\keyword{classes}
