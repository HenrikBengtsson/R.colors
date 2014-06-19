%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  DichromatColorFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{DichromatColorFilter}
\docType{class}
\alias{DichromatColorFilter}


 \title{Color filter emulating dichromatic color blindness}

 \description{
  Package:  R.colors \cr
\bold{Class DichromatColorFilter}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{ColorFilter}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{DichromatColorFilter}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{DichromatColorFilter}\cr
extends \link[R.colors]{ColorFilter}\cr



  Color filter emulating dichromatic color blindness.
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

 \usage{DichromatColorFilter(type=c("deutan", "protan"))}

 \arguments{
   \item{type}{A \code{\link[base]{character}} string specifying the type of color blindness
    to emulate. See \code{\link[dichromat]{dichromat}} [1] for more details.}
 }

 \author{Henrik Bengtsson, but all credits to Thomas Lumley who wrote the
  \code{dichromat} package, which this class utilizes.}

 \details{
   This color filter requires that the \code{dichromat} package [1] is
   installed. The easiest way to install this is by calling
   \code{install.packages("dichromat")}.

   Color blindness:
    1) Protanope   (L-cone - red cone cells defective),
    2) Deuteranope (M-cone - green cone cells defective), and
    3) Tritanope   (S-cone - blue cone cells defective).

   For a dichromat safe color palette, see [4].

   To modify an image such that its colors are more accessable
   to color blind people see [3].
 }

 \references{
   [1] Thomas Lumley, \emph{R package \code{dichromat}}, CRAN.
       \url{http://cran.r-project.org/}.\cr
   [2] Vischeck, webpage, 2003. \url{http://www.vischeck.com/}\cr
   [3] Daltonize, webpage, 2003. \url{http://www.vischeck.com/daltonize/}\cr
   [4] Masataka Okabe and Kei Ito, \cr
       \emph{Barrier-free presentation that is friendly to colorblind},
       \url{http://jfly.iam.u-tokyo.ac.jp/html/color_blind/}\cr
 }

 \keyword{color}

 \seealso{
   See also the \code{\link[dichromat]{dichromat}} package [1].
 }
\keyword{classes}
