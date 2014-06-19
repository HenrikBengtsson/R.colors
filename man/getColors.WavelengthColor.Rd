%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  WavelengthColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getColors.WavelengthColor}
\alias{getColors.WavelengthColor}
\alias{WavelengthColor.getColors}
\alias{getColors.WavelengthColor}
\alias{getColors,WavelengthColor-method}


\title{Gets the set of colors in the \#rrggbb format}

\usage{\method{getColors}{WavelengthColor}(this, filter=TRUE, deviceGamma=this$deviceGamma, ...)}

\description{
 Gets the set of colors in the \#rrggbb format.
}

\arguments{
  \item{filter}{If \code{\link[base:logical]{TRUE}}, the colors are filter through the default
    filter, otherwise not.}
  \item{deviceGamma}{Gamma factor, a \code{\link[base]{numeric}} in [0,1].}
  \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{vector}} of \code{\link[base]{character}} strings of length equal to the number of
 colors in the Color object, cf. \code{\link{Color}} class.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  For more information see \code{\link{WavelengthColor}}.
}
\keyword{internal}
\keyword{methods}
