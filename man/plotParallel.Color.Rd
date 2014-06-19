%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Color.PLOT.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{plotParallel.Color}
\alias{plotParallel.Color}
\alias{Color.plotParallel}
\alias{plotParallel.Color}
\alias{plotParallel,Color-method}


\title{Plots the column vectors in the colorspace matrix versus the index}

\usage{\method{plotParallel}{Color}(this, index=seq(this), xlab="index", ylab=paste(colnames(this), collapse = ", "), col=getColors(this)[index], transpose=FALSE, ...)}

\description{
 Plots the column vectors in the colorspace matrix versus the index.
}

\arguments{
 \item{index}{\code{\link[base]{integer}} vector specifying the order the color vectors
   should be plotted.}
 \item{xlab,ylab}{The labels on the x and the axis.}
 \item{transpose}{If \code{\link[base:logical]{TRUE}}, the colorspace matrix is transposed before
   passed to \code{\link[graphics]{plot}}. This is makes it convenient to specify
   plot attributes such as \code{col} or \code{pch} for either each
   unique color or each color vector (column vector in the color space).}
 \item{...}{Other arguments accepted by \code{\link[graphics]{plot}}.}
}

\value{
 Returns nothing.
}

\examples{
  x <- 0:100/100
  ch1 <- (sin(x*2*pi) + 1)/2
  ch2 <- (cos(x*2*pi) + 1)/2
  rg <- TwoChannelMicroarrayColor(ch1, ch2, maxColorValue=1)
  layout(matrix(1:2))
  plotParallel(rg)
  plotParallel(rg, col=c("red", "green"), transpose=TRUE)

  # One-dimensional colors
ncolors <- 256
x <- seq(0,1, length=ncolors)
ch1 <- x
ch2 <- rev(x)

colors <- list(
  # One-dimensional colors
  red      = RedColor(x),
  green    = GreenColor(x),
  blue     = BlueColor(x),
  gray     = GrayColor(x),
  spectrum = WavelengthColor(seq(380,779,length=ncolors)),
  heat     = HeatColor(x),
  rainbow  = RainbowColor(x),
  topo     = TopologyColor(x),
  terrain  = TerrainColor(x),
  cm       = CyanMagentaColor(x),

  # Two-dimensional colors
  rg = TwoChannelMicroarrayColor(ch1,ch2, maxColorValue=1),
  yb = TwoChannelMicroarrayColor(ch1,ch2, hueRange=c(HsvgColor$YELLOW.HUE,HsvgColor$BLUE.HUE), maxColorValue=1),
 
  # Three-dimensional colors
  rgb  = RgbColor(x,rev(x),sample(x)),
  hcl  = HclColor(x),

  # Four-dimensional colors
  hsvg = HsvgColor(x),
  cmyk = CmykColor(x,sample(x),rev(x),x/2)
)

layout(matrix(1:16, ncol=4, byrow=TRUE))
opar <- par(mar=c(0,1,3,1)+0.5)

for(color in colors)
  plotParallel(RgbColor(color))

par(opar)

}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{Color}}.
}

\keyword{color}
\keyword{internal}
\keyword{methods}
