%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  RgbColor.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{setChannel.RgbColor}
\alias{setChannel.RgbColor}
\alias{RgbColor.setChannel}
\alias{setChannel.RgbColor}
\alias{setChannel,RgbColor-method}


\title{Sets the color values in the specific channel}

\usage{\method{setChannel}{RgbColor}(this, channel, value, maxColorValue=1, ...)}

\description{
 Sets the color values in the specific channel. If there are less values than colors, the values will
 be looped over.
}

\arguments{
 \item{channel}{A \code{\link[base]{character}} string specifying which channel to assign to.}
 \item{value}{A \code{\link[base]{vector}} of values to be assigned to the specified channel.}
 \item{maxColorValue}{Specifying on what range [0,\code{maxColorValue}] the
   \code{\link[base]{vector}} \code{value} is in. All values will be mapped to [0,1].
   This argument exists just for convenience.}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\keyword{color}

\seealso{
  See also the "shortcut" methods \code{\link[R.colors:setRed.RgbColor]{*setRed}()},
  \code{\link[R.colors:setGreen.RgbColor]{*setGreen}()} and \code{\link[R.colors:setBlue.RgbColor]{*setBlue}()}.
  For more information see \code{\link{RgbColor}}.
}
\keyword{internal}
\keyword{methods}
