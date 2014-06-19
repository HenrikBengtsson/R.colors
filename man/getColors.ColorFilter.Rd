%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ColorFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getColors.ColorFilter}
\alias{getColors.ColorFilter}
\alias{ColorFilter.getColors}
\alias{getColors.ColorFilter}
\alias{getColors,ColorFilter-method}


\title{Filters a set of colors}

\usage{\method{getColors}{ColorFilter}(this, colors, ...)}

\description{
 Filters a set of colors and returns a \code{\link[base]{character}} representation of the filtered
 colors.

 For this class, no filtering is done, but subclasses will override this.
}

\arguments{
  By default the getColors() method does not take any arguments, but
  subclasses may provide optional arguments for convenience, e.g.
  gamma correction parameters etc.
}

\value{
 Returns a \code{\link[base]{character}} string \code{\link[base]{vector}} of the filtered colors, which is
 of the same length as the number of input colors.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  For more information see \code{\link{ColorFilter}}.
}
\keyword{internal}
\keyword{methods}
