%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  WavelengthColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{WavelengthColor}
\docType{class}
\alias{WavelengthColor}


\title{Class representing colors specified by wavelength}

\description{
 Package:  R.colors \cr
\bold{Class WavelengthColor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.colors]{Color}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link[R.colors]{OneDimensionalColor}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{WavelengthColor}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{WavelengthColor}\cr
extends \link[R.colors]{OneDimensionalColor}\cr



 Class representing colors specified by wavelength.

  Because the concept of colors is all about spectroscopy, the biology and
  physics of the human eye and the brain's perception of the neurological
  signals, there is no unique one-to-one mapping between wavelength and
  (R,G,B) values.
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.colors:getColors.WavelengthColor]{getColors}} \tab Gets the set of colors in the \#rrggbb format.\cr
 \tab \code{\link[R.colors:getFrequency.WavelengthColor]{getFrequency}} \tab Gets the frequences (in THz) of the colors.\cr
 \tab \code{\link[R.colors:getWavelength.WavelengthColor]{getWavelength}} \tab Gets the wavelengths (in nm) of the colors.\cr
 \tab \code{\link[R.colors:range.WavelengthColor]{range}} \tab Gets the (wavelength) range of the colors.\cr
}


 \bold{Methods inherited from OneDimensionalColor}:\cr
getColorSpace

 \bold{Methods inherited from Color}:\cr
[, as.character, display, drawColorRamp, getColorFilter, getColors, getColorSpace, length, palette, plotParallel, seq, setColorFilter, viewThroughColorFilter

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\usage{WavelengthColor(wavelength=NULL, deviceGamma=0.8)}

\arguments{
  \item{wavelength}{\code{\link[base]{vector}} of wavelengths (in units of nm). Note that
    there is no upper limit (\code{maxColorValue}) of possible wavelength
    but above a certain limit they will not be visible (shown).}
  \item{deviceGamma}{Gamma factor used by the  \code{\link[R.colors:getColors.WavelengthColor]{*getColors}()} method.}
  \item{...}{Arguments passed to the constructor of the super class.}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\examples{
require(R.basic)  # image270()

lambda <- seq(from=380, to=779)  # in units of nm.
col <- WavelengthColor(lambda)

filters <- list(
  none   = ColorFilter(),
  gray   = GrayColorFilter()
)

if ("dichromat" \%in\% library()$results) {
  filter <- append(filter, list(
    deutan    = DichromatColorFilter(type="deutan")
  ))
}


layout(matrix(seq(filters), ncol=1))

ofilter <- Color$getColorFilter()
for (filter in filters) {
  # View the colors through the given filter
  Color$setColorFilter(filter)

  # Display the colors along the wavelength dimension
  opar <- par(mar=c(5,2,6,2)+0.1)
  image270(x=as.matrix(lambda), y=as.matrix(1), z=matrix(lambda, nrow=1), col=getColors(col), xlab=expression(paste("Top: frequency (THz), Bottom: ", lambda, "- wavelength (nm)")), ylab="", axes=FALSE)
  at <- seq(from=min(lambda), to=max(lambda), by=20)
  axis(side=1, at=at)
  freq <- round(getFrequency(WavelengthColor(at)));
  axis(side=3, at=at, labels=freq)
  main <- paste(data.class(col), ": ", paste(range(col),collapse="-"), " nm", sep="");
  if (data.class(filter) != "ColorFilter")
    main <- paste(main, "filter through a", data.class(filter));
  title(main=main);
  par(opar)
}
Color$setColorFilter(ofilter)
}

\references{
  Dan Bruton, Color Science,
  \url{http://www.physics.sfasu.edu/astro/color.html}, 2002.\cr

  efg's Computer Lab,
    \url{http://www.efg2.com/Lab/ScienceAndEngineering/Spectra.htm}, 2002.
}

\keyword{color}
\keyword{classes}
