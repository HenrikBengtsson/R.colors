%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Color.PLOT.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{drawColorRamp.Color}
\alias{drawColorRamp.Color}
\alias{Color.drawColorRamp}
\alias{drawColorRamp.Color}
\alias{drawColorRamp,Color-method}


\title{Draw a color ramp of the colors in the current plot}

\usage{\method{drawColorRamp}{Color}(this, x, y, width, height, angle=0, xadj=0, yadj=1, borderColor=par("fg"), borderType=par("lty"), borderWidth=par("lwd"), xpd=par("xpd"), ...)}

\description{
 Draw a color ramp of the colors in the current plot.
}

\arguments{
 \item{x,y}{The x and y coordinate of the (non-rotated) upper-left corner
   of the color ramp.}
 \item{width,height}{The width and the height of the (non-rotated) color
   ramp.}
 \item{angle}{The rotation (in degree) of the color ramp.}
 \item{borderColor}{The color of the border lines.}
 \item{borderType}{The type of the border lines.}
 \item{borderWidth}{The width of the border lines.}
 \item{xpd}{If \code{\link[base:logical]{TRUE}}, the color ramp can be drawn in the margins too,
   otherwise not.}
 \item{...}{Other arguments accepted by the \code{\link[graphics]{polygon}} function.}
}

\value{
 Returns a \code{\link[base]{list}} with x and y coordinates of the corners of the polygons
 but also their center coordinates, in the color ramp.
}

\examples{
# Create data
x <- seq(from=0, to=3*base::pi, length=101)
y <- sin(x)

# Generate colors
y01 <- (y-min(y))/diff(range(y))
col <- RainbowColor(y01)
rampValues <- seq(from=-1,to=1, length=51);
ramp01 <- (rampValues-min(rampValues))/diff(range(rampValues))
ramp <- RainbowColor(ramp01)

# Plot data
opar <- par(mar=c(5,4,4,6)+0.1, xpd=TRUE)
plot(x,y, col=getColors(col), lwd=2)

# Plot color ramp
usr <- par("usr")
dx <- diff(usr[1:2]);
dy <- diff(usr[3:4]);
sh <- 0.05;

# Draw horisontal ramp
drawColorRamp(col, x=usr[1], y=usr[4]+0.01*dy, width=dx, height=sh*dy, angle=0, yadj=0, borderColor=NA)

# Draw vertical ramp
xy <- drawColorRamp(ramp, x=usr[2]+0.01*dx, y=usr[3], width=dy, height=sh*dx, angle=90, xadj=0, borderColor=NA)
incl <- seq(from=1, to=length(rampValues), length=11);
text(xy$below$x[incl], xy$below$y[incl], label=sapply(rampValues[incl], FUN=sprintf, fmt="\%+3.2f"), adj=c(-0.1,0.5), cex=0.8)

par(opar)
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{Color}}.
}

\keyword{color}
\keyword{internal}
\keyword{methods}
