%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Color.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getColorSpace.Color}
\alias{getColorSpace.Color}
\alias{Color.getColorSpace}
\alias{getColorSpace.Color}
\alias{getColorSpace,Color-method}


 \title{Gets the internal representation of the colors}

 \usage{\method{getColorSpace}{Color}(...)}

 \description{
  Gets the internal representation of the colors.

  To the developer of a Color class:
  \emph{This method is to be implemented in a subclass.}
 }

 \arguments{
   \item{...}{Not used.}
 }

 \value{
  Returns the internal represenation of the colors. This can be a
  \code{\link[base]{vector}}, a \code{\link[base]{matrix}}, but any class is possible.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \keyword{color}

 \seealso{
   For more information see \code{\link{Color}}.
 }
\keyword{internal}
\keyword{methods}
