%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  MonochromeColorFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{MonochromeColorFilter}
\docType{class}
\alias{MonochromeColorFilter}


 \title{Class representing a monochrome color filter}

 \description{
  Package:  R.colors \cr
\bold{Class MonochromeColorFilter}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{ColorFilter}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{MonochromeColorFilter}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{MonochromeColorFilter}\cr
extends \link[R.colors]{ColorFilter}\cr



  Class representing a monochrome color filter.
  All colors filtered through a MonochromeColorFilter will be
  monochrome colors.
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

 \usage{MonochromeColorFilter(threshold=0.5, monocolors=c("#FFFFFF", "#000000"), coefficients=c(0.3, 0.59, 0.11))}

 \arguments{
   \item{threshold}{A \code{\link[base]{numeric}} threshold in [0,1] specifying for where
    to split the first and the second monochrome color.}
   \item{monocolors}{A \code{\link[base]{vector}} or a \code{\link{Color}} object containing two
    colors to be used as the monochrome colors.
    By default white and black is used.}
   \item{coefficients}{\code{\link[base]{vector}} of three \code{\link[base]{numeric}} values specifying the
    relative amount of the red, green and blue channel that should be
    used to generate the graylevels, which in turn are threshold to
    monochrome colors. The values are normalized such that they sum to one.}
 }

 \details{
   Conceptually, the input colors are filtered through a grayscale color
   filter and then thresholded. Values \emph{below} the threshold is set
   to the \emph{second} monochrome color and values equal or above to
   the first.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \keyword{color}

 \seealso{
   \code{\link{GrayColorFilter}}.
 }
\keyword{classes}
