%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Color.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getColorFilter.Color}
\alias{getColorFilter.Color}
\alias{Color.getColorFilter}
\alias{getColorFilter.Color}
\alias{getColorFilter,Color-method}


\title{Gets the default color filter}

\synopsis{getColorFilter.Color(static, ...)}
\usage{Color$getColorFilter(...)}

\description{
 Gets the default color filter, which all colors are filtered through.
 If \code{\link[base]{NULL}}, no filtering is applied.
}

\arguments{
  \item{...}{Not used.}
}

\value{
  Returns \code{\link{ColorFilter}} or \code{\link[base]{NULL}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  To set or reset the default filter see \code{\link[R.colors:setColorFilter.Color]{*setColorFilter}()}.
  To filter colors through the default filter see
  \code{\link[R.colors:viewThroughColorFilter.Color]{*viewThroughColorFilter}()}.
  For more information see \code{\link{Color}}.
}
\keyword{internal}
\keyword{methods}
