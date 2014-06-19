%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FourDimensionalColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getColorSpace.FourDimensionalColor}
\alias{getColorSpace.FourDimensionalColor}
\alias{FourDimensionalColor.getColorSpace}
\alias{getColorSpace.FourDimensionalColor}
\alias{getColorSpace,FourDimensionalColor-method}


\title{Gets the internal representation of the colors}

\usage{\method{getColorSpace}{FourDimensionalColor}(this, ...)}

\description{
 Gets the internal representation of the colors.
}

\arguments{
  \item{...}{Not used.}
}

\value{
 Returns the internal represenation of the colors as a \code{\link[base]{matrix}} with
 four columns. Most often the values are in the range [0,1], but that is
 not enforced.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  For more information see \code{\link{FourDimensionalColor}}.
}
\keyword{internal}
\keyword{methods}
