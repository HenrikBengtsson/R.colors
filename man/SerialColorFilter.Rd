%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  SerialColorFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{SerialColorFilter}
\docType{class}
\alias{SerialColorFilter}


 \title{Class representing a set of color filters connected serially}

 \description{
  Package:  R.colors \cr
\bold{Class SerialColorFilter}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{ColorFilter}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{SerialColorFilter}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{SerialColorFilter}\cr
extends \link[R.colors]{ColorFilter}\cr



  Class representing a set of color filters connected serially.

  With this filter you can "link" together several \code{\link{ColorFilter}}s
  to work as it was one.
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

 \usage{SerialColorFilter(filters=NULL)}

 \arguments{
   \item{filters}{An ordered \code{\link[base]{list}} of \code{\link{ColorFilter}}s to be linked.}
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \keyword{color}
\keyword{classes}
