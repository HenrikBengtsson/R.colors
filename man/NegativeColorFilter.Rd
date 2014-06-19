%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  NegativeColorFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{NegativeColorFilter}
\docType{class}
\alias{NegativeColorFilter}


 \title{Color filter that inverts (negatates) colors}

 \description{
  Package:  R.colors \cr
\bold{Class NegativeColorFilter}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{ColorFilter}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{NegativeColorFilter}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{NegativeColorFilter}\cr
extends \link[R.colors]{ColorFilter}\cr



  Color filter that inverts (negatates) colors.
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

 \usage{NegativeColorFilter()}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \details{
   This color filter takes the (red,green,blue) representation in
   [0,1]x[0,1]x[0,1] of the colors and invert the colors by
   (1-red,1-green,1-blue). There might be other definitions of how
   the invert colors, but this was just a quick hack.
 }

 \keyword{color}
\keyword{classes}
