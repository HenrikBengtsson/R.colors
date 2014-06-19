%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  HclColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{HclColor}
\docType{class}
\alias{HclColor}


\encoding{latin1}

\title{Class representing colors in the (hue,chroma,luminance) space}

\description{
 Package:  R.colors \cr
\bold{Class HclColor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{Color}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link[R.colors]{ThreeDimensionalColor}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{HclColor}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{HclColor}\cr
extends \link[R.colors]{ThreeDimensionalColor}\cr



 Class representing colors in the (hue,chroma,luminance) space.

 Internally all colors are represented as (hue,chroma,luminance)
 in [0,1]x[0,1]x[0,1].
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.colors:getColors.HclColor]{getColors}} \tab Gets the set of colors in the \#rrggbb format.\cr
 \tab \code{plotParallel} \tab  -\cr
}


 \bold{Methods inherited from ThreeDimensionalColor}:\cr
getColorSpace, plot3d

 \bold{Methods inherited from Color}:\cr
[, as.character, display, drawColorRamp, getColorFilter, getColors, getColorSpace, length, palette, plotParallel, seq, setColorFilter, viewThroughColorFilter

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\usage{HclColor(hue=NULL, chroma=1, luminance=1, maxColorValue=1, correctOutOfRange=TRUE)}

\arguments{
  \item{hue}{A \code{\link[base]{vector}} of \code{\link[base]{numeric}} in the range [0,\code{maxColorValue}].}
  \item{chroma}{(colorfulness) A \code{\link[base]{vector}} of \code{\link[base]{numeric}} in the range [0,\code{maxColorValue}].}
  \item{luminance}{A \code{\link[base]{vector}} of \code{\link[base]{numeric}} in the range [0,\code{maxColorValue}].}
  \item{maxColorValue}{A \code{\link[base]{numeric}} value specifying the maximum
                    value the values in the different channels can take.}
  \item{correctOutOfRange}{By default, if \code{\link[base:logical]{TRUE}}, generated colors that fall
    outside of the valid (red,green,blue) space, will be truncated.
    Otherwise, they will be set to \code{\link[base]{NA}}.}

 Moreover, the number of colors will be the same as the length of the
 longest \code{\link[base]{vector}} of the above. If some of the other vectors are shorter,
 their values will be looped over to get equal lengths.
}

\details{
  The creation of this Color class was strongly inspired by Ross Ihaka's talk
  at DSC-2003 [1]
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\references{
 [1] Ross Ihaka, \emph{Colour for Presentation Graphics}, Proceedings of the
     3rd International Workshop on Distributed Statistical Computing (DSC 2003),
     March 20-22, 2003, Technische Universität Wien, Vienna, Austria.
     \url{http://www.ci.tuwien.ac.at/Conferences/DSC-2003/}.\cr
 [2] Ross Ihaka, \emph{Perceptually-Based Color Choices},
    webpage, \url{http://www.stat.auckland.ac.nz/~ihaka/colour/}\cr
 [3] Ross Ihaka, \emph{Statistics 120 - Information Visualisation},
    webpage, \url{http://www.stat.auckland.ac.nz/~ihaka/120/}.\cr
}

\seealso{
  See also \code{\link[grDevices]{hcl}} in the \code{colorspace} package [1-3].
}
\keyword{classes}
