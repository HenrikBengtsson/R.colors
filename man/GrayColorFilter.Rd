%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GrayColorFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{GrayColorFilter}
\docType{class}
\alias{GrayColorFilter}


 \title{Class representing a grayscale color filter}

 \description{
  Package:  R.colors \cr
\bold{Class GrayColorFilter}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{ColorFilter}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{GrayColorFilter}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{GrayColorFilter}\cr
extends \link[R.colors]{ColorFilter}\cr



  Class representing a grayscale color filter.
  All colors filtered through a GrayScaleFilter will be graylevel colors.
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{getColors} \tab  -\cr
}


 \bold{Methods inherited from ColorFilter}:\cr
getColors

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \usage{GrayColorFilter(coefficients=c(0.3, 0.59, 0.11))}

 \arguments{
   \item{coefficients}{\code{\link[base]{vector}} of three \code{\link[base]{numeric}} values specifying the
    relative amount of the red, green and blue channel that should be
    used to generate the graylevels. The value are normalized such that
    they sum to one.}
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \keyword{color}
\keyword{classes}
