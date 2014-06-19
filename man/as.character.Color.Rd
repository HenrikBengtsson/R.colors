%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Color.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{as.character.Color}
\alias{as.character.Color}
\alias{Color.as.character}
\alias{as.character.Color}
\alias{as.character,Color-method}


\title{Gets a character representation of the colors}

\usage{\method{as.character}{Color}(x, ...)}

\description{
 Gets a character representation of the colors.
 By default this method calls \code{\link[R.colors:getColors.Color]{*getColors}()} and it is
 \emph{expected} that this not overridden by subclasses.
}

\arguments{
  \item{...}{Not used.}
}

\value{
 Returns the colors as a \code{\link[base]{vector}} of \code{\link[base]{character}} strings. For more
 details see \code{\link[R.colors:getColors.Color]{*getColors}()}.
}

\details{
 To the develop of a new Color class: \emph{Do override this method.}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  \code{\link[R.colors:getColors.Color]{*getColors}()}.
  For more information see \code{\link{Color}}.
}
\keyword{internal}
\keyword{methods}
