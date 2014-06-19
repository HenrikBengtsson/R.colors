%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  TwoChannelMicroarrayColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{TwoChannelMicroarrayColor}
\docType{class}
\alias{TwoChannelMicroarrayColor}


\title{Class representing two-channel microarray colors}

\usage{TwoChannelMicroarrayColor(ch1=NULL, ch2=NULL, maxColorValue=65535, log2Cutoff=5, hueRange=c(HsvgColor$RED.HUE, HsvgColor$GREEN.HUE), hueIntensityColor="hsvg")}

\description{
  Class representing two-channel microarray colors.
}

\arguments{
  \item{ch1,ch2}{\code{\link[base]{numeric}} \code{\link[base]{vector}}s reprensenting the signals in
    each channel. The signals should be in the range
    [0,\code{maxColorValue}].}
  \item{maxColorValue}{\code{\link[base]{numeric}} specifying the maximum signal value.}
  \item{log2Cutoff}{\code{\link[base]{numeric}} specifying the cutoff level of the
    absolute log-ratios (base 2). Absolute log-ratios above this level
    will be made equal to \code{log2Cutoff}.}
  \item{hueRange}{A \code{\link[base]{vector}} of two integers or a \code{\link[base]{character}} string,
    specifying the hue (color) range that the log-ratios should span.}
  \item{hueIntensityColor}{A \code{\link[base]{function}} or a \code{\link{Color}} class that takes the
    arguments \code{hue} and \code{intensity}, or a \code{\link[base]{character}} string
    specifying which color class. This specifies the class of colors
    from which the microarray colors should be generated.}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\details{
  TO DO:
  "In the fluorescent double-staining micrographs, DNA chips, etc, do
   not use the combination of red and green. Use magenta (purple) and
   green instead." [1].
}

\references{
 [1] Masataka Okabe and Kei Ito,
     \emph{Barrier-free presentation that is friendly to colorblind},
     \url{http://jfly.iam.u-tokyo.ac.jp/html/color_blind/#stain}
}

\examples{
layout(matrix(1:4, ncol=2, byrow=TRUE))

TwoChannelMicroarrayColor$demo()

if ("dichromat" \%in\% library()$results) {
  ofilter <- Color$setColorFilter(DichromatColorFilter())
  TwoChannelMicroarrayColor$demo()
 
  Color$setColorFilter(DichromatColorFilter(type="protan"))
  TwoChannelMicroarrayColor$demo()
}

Color$setColorFilter(GrayColorFilter())
TwoChannelMicroarrayColor$demo()

if ("dichromat" \%in\% library()$results) {
  Color$setColorFilter(ofilter)
}
}

\keyword{color}
\keyword{classes}
