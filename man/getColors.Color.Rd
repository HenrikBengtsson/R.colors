%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Color.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getColors.Color}
\alias{getColors.Color}
\alias{Color.getColors}
\alias{getColors.Color}
\alias{getColors,Color-method}


\title{Gets the set of colors in the \#rrggbb format}

\usage{\method{getColors}{Color}(this, filter=TRUE, ...)}

\description{
 Gets the set of colors in the \#rrggbb format.
}

\arguments{
  \item{filter}{If \code{\link[base:logical]{TRUE}}, the colors are filter through the default
    filter, see \code{\link[R.colors:viewThroughColorFilter.Color]{*viewThroughColorFilter}()}, otherwise not.}

  By default the getColors() method does not take any other arguments,
  but subclasses may provide optional arguments for convenience, e.g.
  gamma correction parameters etc.
  \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{vector}} of \code{\link[base]{character}} strings of length equal to the number of
 colors in the Color object. The returned colors are character string of
 format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
 For more information about this format \code{\link[grDevices]{colors}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  \code{\link[R.colors:000-LB-.Color]{*[}()},
  \code{\link[R.colors:as.character.Color]{*as.character}()},
  For more information see \code{\link{Color}}.
}
\keyword{internal}
\keyword{methods}
