#########################################################################/**
# @RdocClass TwoChannelMicroarrayColor
#
# @title "Class representing two-channel microarray colors"
#
# @synopsis
#
# \description{
#   @get "title".
# }
#
# \arguments{
#   \item{ch1,ch2}{@numeric @vectors reprensenting the signals in 
#     each channel. The signals should be in the range
#     [0,\code{maxColorValue}].}
#   \item{maxColorValue}{@numeric specifying the maximum signal value.}
#   \item{log2Cutoff}{@numeric specifying the cutoff level of the 
#     absolute log-ratios (base 2). Absolute log-ratios above this level
#     will be made equal to \code{log2Cutoff}.}
#   \item{hueRange}{A @vector of two integers or a @character string,
#     specifying the hue (color) range that the log-ratios should span.}
#   \item{hueIntensityColor}{A @function or a @see "Color" class that takes the
#     arguments \code{hue} and \code{intensity}, or a @character string
#     specifying which color class. This specifies the class of colors
#     from which the microarray colors should be generated.}
# }
#
# @author
#
# \details{
#   TO DO:
#   "In the fluorescent double-staining micrographs, DNA chips, etc, do
#    not use the combination of red and green. Use magenta (purple) and
#    green instead." [1].
# }
#
# \references{
#  [1] Masataka Okabe and Kei Ito, 
#      \emph{Barrier-free presentation that is friendly to colorblind}, 
#      \url{http://jfly.iam.u-tokyo.ac.jp/html/color_blind/#stain}
# }
#
# @examples "../incl/TwoChannelMicroarrayColor.Rex"
#
# \keyword{color}
#*/#########################################################################
setConstructorS3("TwoChannelMicroarrayColor", function(ch1=NULL, ch2=NULL, maxColorValue=65535, log2Cutoff=5, hueRange=c(HsvgColor$RED.HUE, HsvgColor$GREEN.HUE), hueIntensityColor="hsvg") {
  extend(TwoDimensionalColor(ch1=ch1, ch2=ch2, maxColorValue=maxColorValue), "TwoChannelMicroarrayColor",
    hueIntensityColor = hueIntensityColor,
    hueRange = hueRange,
    log2Cutoff  = log2Cutoff
  )
})

setMethodS3("getColors", "TwoChannelMicroarrayColor", function(this, log=this$log, log2Cutoff=this$log2Cutoff, hueRange=this$hueRange, hueIntensityColor=this$hueIntensityColor, filter=TRUE, ...) {
  if (is.character(hueIntensityColor)) {
    if (identical(hueIntensityColor, "hsvg")) {
      hueIntensityColor <- function(hue, intensity) {
        HsvgColor(hue=hue,value=intensity);
      }
    } else if (identical(hueIntensityColor, "hcl")) {
      hueIntensityColor <- function(hue, intensity) {
        HclColor(hue=hue, luminance=intensity);
      }
    } else {
      throw("Unknown value of argument 'hueIntensityColor': ", 
                     paste(hueIntensityColor, collapse=", "));
    }
  }

  if (!is.function(hueIntensityColor)) {
      throw("Argument 'hueIntensityColor' must be either a string of a function: ", paste(hueIntensityColor, collapse=", "));
  }

  # Get the values in the two channels in [0,1]x[0,1]
  xy <- getColorSpace(this);

  # Rescale the signals to "well known" [0,65535]x[0,65535]
  xy <- 65535*xy;

  # Calculate the log-ratios in range [-Inf,+Inf] and
  # the average log-intensities in [-Inf,16].
  M <- log(xy[,1]/xy[,2], base=2)
  A <- log(xy[,1]*xy[,2], base=2) / 2;

  # Bring the log-ratio to same scale as the log-intensities
  M[M < -log2Cutoff] <- -log2Cutoff;
  M[M > +log2Cutoff] <- +log2Cutoff;

  # Bring the log-ratio to the [-1,1] range
  M <- M / log2Cutoff;

  # Bring the log-ratio to the [0,1] range
  M <- (1+M)/2;

  # Bring the log-intensity to the [0,16] range by censoring
  A[A <  0] <-  0;

  # Bring the log-intensity to the [0,1] range by censoring
  A <- A / 16;
  A[A > 1] <- 1;  # Just in case

  if (identical(hueRange, "red-green")) {
    hueRange <- c(HsvgColor$RED.HUE, HsvgColor$GREEN.HUE);
  } else if (identical(hueRange, "blue-yellow")) {
    hueRange <- c(HsvgColor$BLUE.HUE, HsvgColor$YELLOW.HUE);
  } else if (!is.numeric(hueRange) || !length(hueRange) == 2) {
    throw("Unknown hue range: ", paste(hueRange, collapse=", "));
  }

  hueSpan <- diff(hueRange);
  hue <- hueRange[1] + M * hueSpan;  # range in [0,1]
  col <- hueIntensityColor(hue, A);
  col <- getColors(col, filter=filter);
})


setMethodS3("demo", "TwoChannelMicroarrayColor", function(static, pch=19, cex=2, ...) {
  lr <- seq(from=0, to=log(2^16-1, base=2), length=2001);
  lg <- sample(lr);

  r <- 2^lr;
  g <- 2^lg;
  r <- floor(r);
  g <- floor(g);
  color <- TwoChannelMicroarrayColor(ch1=r, ch2=g, ...);
  color <- getColors(color);
  M <- lr-lg;
  A <- (lr+lg)/2;

  plot(A,M, col=color, pch=pch, cex=cex);
})


############################################################################
# HISTORY:
# 2002-11-21
# o Added demo().
# o Cleaned up the code and the arguments. Now log will always be applied.
# 2002-11-19
# o Created from old Colors.R.
# o Added getRgbFromWavelength().
############################################################################
