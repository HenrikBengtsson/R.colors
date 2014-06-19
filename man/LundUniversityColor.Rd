%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  LundUniversityColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{LundUniversityColor}
\docType{class}
\alias{LundUniversityColor}


\title{Class representing colors according to the Lund University
        Visual Identity Programme}

\description{
 Package:  R.colors \cr
\bold{Class LundUniversityColor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{Color}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{LundUniversityColor}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{LundUniversityColor}\cr
extends \emph{\link[R.colors]{Color}}\cr



 Class representing colors according to the Lund University
        Visual Identity Programme [1].
 This programme or profile specifies four colors,
 a blue (\code{"lublue"}), a brown (\code{"lubrown"}),
 a plain white (\code{"luwhite"}) and a plain
 black (\code{"lublack"}).
 The last two are included just for convenience.
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{getColors} \tab  -\cr
 \tab \code{getColorSpace} \tab  -\cr
}


 \bold{Methods inherited from Color}:\cr
[, as.character, display, drawColorRamp, getColorFilter, getColors, getColorSpace, length, palette, plotParallel, seq, setColorFilter, viewThroughColorFilter

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\usage{LundUniversityColor(palette=NULL)}

\arguments{
  \item{palette}{\code{\link[base]{character}} string specifying which palette to use.
    By default, the \code{"strict"}, i.e. the formal, palette is used.
    For on screen presentations and web pages, \code{"websoft"} provides
    less intense colors and the \code{"websafe"} provides colors from
    the so called 216 websafe colors.}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\references{
  [1] Lund University Visual Identity Programme, 2000.
      \url{http://www.lu.se/LUinternt/grafiskprofil/english/}
}

\seealso{
  \code{\link[grDevices]{palette}}.
}
\keyword{classes}
