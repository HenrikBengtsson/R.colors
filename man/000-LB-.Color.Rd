%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Color.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{[.Color}
\alias{[.Color}
\alias{Color.[}
\alias{[.Color}
\alias{[,Color-method}


\title{Gets a subset of the colors by indices}

\usage{\method{[}{Color}(this, indices, ...)}

\description{
 Gets a subset of the colors by indices.
 This is just a convenient wrapper for \code{getColors(col)[indices]}.
}

\arguments{
  \item{indices}{A \code{\link[base]{vector}} of indices specifying which of the colors
     to be returned.}
  \item{...}{Not used.}
}

\value{
 Returns a subset of the colors returned by \code{\link[R.colors:getColors.Color]{*getColors}()}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  For more information see \code{\link{Color}}.
}
\keyword{internal}
\keyword{methods}
