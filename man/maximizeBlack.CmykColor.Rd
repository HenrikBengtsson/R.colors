%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  CmykColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{maximizeBlack.CmykColor}
\alias{maximizeBlack.CmykColor}
\alias{CmykColor.maximizeBlack}
\alias{maximizeBlack.CmykColor}
\alias{maximizeBlack,CmykColor-method}


 \title{Maximizes the black channel to 'save' the other colors}

 \synopsis{maximizeBlack.CmykColor(this, ...)}
\usage{CmykColor$maximizeBlack(...)}

 \description{
  Maximizes the black channel to 'save' the other colors.

  In printing, black tones can be achieved by printing cyan, magenta and
  yellow in equal amounts. Since color toners often are more expensive than
  black toners, by adding black one can reduce the other colors, but still
  print exactly the same color.
  This method minimized the use of cyan, magenta and yellow, by compensating
  with black.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \keyword{color}

 \seealso{
   For more information see \code{\link{CmykColor}}.
 }
\keyword{internal}
\keyword{methods}
