%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  TopologyColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{TopologyColor}
\docType{class}
\alias{TopologyColor}


\title{Class representing topology colors}

\description{
 Package:  R.colors \cr
\bold{Class TopologyColor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{Color}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link[R.colors]{OneDimensionalColor}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{TopologyColor}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{TopologyColor}\cr
extends \link[R.colors]{OneDimensionalColor}\cr



 Class representing topology colors.
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

\usage{TopologyColor(topology=NULL, ...)}

\arguments{
  \item{topology}{\code{\link[base]{vector}} of topology levels in the range [0,\code{maxColorValue}]
    where \code{maxColorValue} is given by the argument with the same
    name in the constructor of the super class.}
  \item{...}{Arguments passed to the constructor of the super class.}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  \code{\link[grDevices]{palette}}.
}
\keyword{classes}
