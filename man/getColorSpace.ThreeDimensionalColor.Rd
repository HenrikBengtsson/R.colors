%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ThreeDimensionalColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getColorSpace.ThreeDimensionalColor}
\alias{getColorSpace.ThreeDimensionalColor}
\alias{ThreeDimensionalColor.getColorSpace}
\alias{getColorSpace.ThreeDimensionalColor}
\alias{getColorSpace,ThreeDimensionalColor-method}


\title{Gets the internal representation of the colors}

\usage{\method{getColorSpace}{ThreeDimensionalColor}(this, ...)}

\description{
 Gets the internal representation of the colors.
}

\arguments{
  \item{...}{Not used.}
}

\value{
 Returns the internal represenation of the colors as a \code{\link[base]{matrix}} with
 three columns. Most often the values are in the range [0,1], but that is
 not enforced.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  For more information see \code{\link{ThreeDimensionalColor}}.
}
\keyword{internal}
\keyword{methods}
