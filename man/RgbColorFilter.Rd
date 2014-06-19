%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  RgbColorFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{RgbColorFilter}
\docType{class}
\alias{RgbColorFilter}


 \title{Class representing an RGB color filter}

 \description{
  Package:  R.colors \cr
\bold{Class RgbColorFilter}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{ColorFilter}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{RgbColorFilter}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{RgbColorFilter}\cr
extends \link[R.colors]{ColorFilter}\cr



  Class representing an RGB color filter.

  With this filter you can specify which of the red, green and blue
  channels you want to let through. One or several channels can be
  used at once.
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

 \usage{RgbColorFilter(channels=NULL)}

 \arguments{
   \item{channels}{A \code{\link[base]{vector}} of \code{\link[base]{character}} strings specifying which
    channels that this filter \emph{will} let through.}
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \keyword{color}
\keyword{classes}
