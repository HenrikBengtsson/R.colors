%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Color.PLOT.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{imageText.Color}
\alias{imageText.Color}
\alias{Color.imageText}
\alias{imageText.Color}
\alias{imageText,Color-method}


\title{Adds text to the last plot}

\usage{\method{imageText}{Color}(this, label=c("none", "index", "rgb", "space"), col=NULL, cex=0.6, srt=0, ...)}

\arguments{
 \item{label}{Type of labels to add (a \code{\link[base]{character}} string).}
 \item{col}{Color for text.}
 \item{cex}{Scale factor (a \code{\link[base]{numeric}}) for the text.}
 \item{srt}{String rotation (a \code{\link[base]{numeric}}).}
}

\description{
 Adds text to the last plot.
}

\examples{
# One-dimensional colors
ncolors <- 256/4
x <- seq(0,1, length=ncolors)

layout(matrix(1:4, ncol=2, byrow=TRUE))
opar <- par(mar=c(0,1,3,1)+0.5)

color <- RainbowColor(x);
for (label in c("index", "rgb", "space")) {
  image(color, gridLty=1, gridLwd=3)
  imageText(color, label=label, cex=1)
}

par(opar)
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{Color}}.
}

\keyword{color}
\keyword{internal}
\keyword{methods}
