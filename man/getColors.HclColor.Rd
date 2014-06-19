%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  HclColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getColors.HclColor}
\alias{getColors.HclColor}
\alias{HclColor.getColors}
\alias{getColors.HclColor}
\alias{getColors,HclColor-method}


\title{Gets the set of colors in the \#rrggbb format}

\usage{\method{getColors}{HclColor}(this, filter=TRUE, correctOutOfRange=this$correctOutOfRange, ...)}

\description{
 Gets the set of colors in the \#rrggbb format.
}

\arguments{
  \item{correctOutOfRange}{If \code{\link[base:logical]{TRUE}}, generated colors that fall outside
    of the valid (red,green,blue) space, will be truncated.
    Otherwise, they will be set to \code{\link[base]{NA}}.}
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
  For more information see \code{\link{HclColor}}.
}
\keyword{internal}
\keyword{methods}
