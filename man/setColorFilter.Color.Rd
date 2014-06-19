%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Color.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{setColorFilter.Color}
\alias{setColorFilter.Color}
\alias{Color.setColorFilter}
\alias{setColorFilter.Color}
\alias{setColorFilter,Color-method}


\title{Sets the default color filter}

\synopsis{setColorFilter.Color(static, filter=NULL, ...)}
\usage{Color$setColorFilter(filter=NULL, ...)}

\description{
 Sets the default color filter, which all colors will be filtered through.
 If \code{\link[base]{NULL}}, no filtering will done.
}

\arguments{
  \item{filter}{A \code{\link{ColorFilter}} or \code{\link[base]{NULL}}. If \code{\link[base]{NULL}}, no filtering
   will be done.}
  \item{...}{Not used.}
}

\value{
  Returns (invisibly) the previous \code{\link{ColorFilter}} used.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  To get the default filter see \code{\link[R.colors:getColorFilter.Color]{*getColorFilter}()}.
  To filter colors through the default filter see
  \code{\link[R.colors:viewThroughColorFilter.Color]{*viewThroughColorFilter}()}.
  For more information see \code{\link{Color}}.
}
\keyword{internal}
\keyword{methods}
