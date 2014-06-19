#########################################################################/**
# @RdocClass WavelengthColor
#
# @title "Class representing colors specified by wavelength"
#
# \description{
#  @classhierarchy
#
#  @get "title".
#
#   Because the concept of colors is all about spectroscopy, the biology and
#   physics of the human eye and the brain's perception of the neurological
#   signals, there is no unique one-to-one mapping between wavelength and 
#   (R,G,B) values.
# }
#
# \section{Fields and Methods}{
#  @allmethods
#
# }
#
# @synopsis
#
# \arguments{
#   \item{wavelength}{@vector of wavelengths (in units of nm). Note that
#     there is no upper limit (\code{maxColorValue}) of possible wavelength
#     but above a certain limit they will not be visible (shown).}
#   \item{deviceGamma}{Gamma factor used by the  @seemethod "getColors" method.}
#   \item{...}{Arguments passed to the constructor of the super class.}
# }
#
# @author
#
# @examples "../incl/WavelengthColor.Rex"
#
# \references{
#   Dan Bruton, Color Science, 
#   \url{http://www.physics.sfasu.edu/astro/color.html}, 2002.\cr
#
#   efg's Computer Lab, 
#     \url{http://www.efg2.com/Lab/ScienceAndEngineering/Spectra.htm}, 2002.
# }
#
# \keyword{color}
#*/#########################################################################
setConstructorS3("WavelengthColor", function(wavelength=NULL, deviceGamma=0.80) {
  extend(OneDimensionalColor(wavelength=wavelength, maxColorValue=NA), "WavelengthColor",
    deviceGamma = as.double(deviceGamma)
  )
})


#########################################################################/**
# @RdocMethod getColors
#
# @title "Gets the set of colors in the \#rrggbb format"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# \arguments{
#   \item{filter}{If @TRUE, the colors are filter through the default
#     filter, otherwise not.}
#   \item{deviceGamma}{Gamma factor, a @numeric in [0,1].}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @vector of @character strings of length equal to the number of 
#  colors in the Color object, cf. @see "Color" class.
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("getColors", "WavelengthColor", function(this, filter=TRUE, deviceGamma=this$deviceGamma, ...) {
  wavelengthToRgb <- function(wavelength, gamma=0.80, maxValue=255) {
    is.zero <- function(x) {
      (abs(x) < .Machine$double.eps);
    } # is.zero()
  
    adjust <- function(color, factor) {
      if (is.zero(color)) {
  	res <- 0;      # Don't want 0^x = 1 for x <> 0
      } else {
  	res <- round(maxValue * (color*factor)^gamma);
      }
      as.integer(res);
    } # adjust()
  
    lambdas <- wavelength;
    rgb <- c();
    for (lambda in lambdas) {
      if (380 <= lambda && lambda < 440) {
  	r <- -(lambda - 440) / (440 - 380);
  	g <- 0.0;
  	b <- 1.0;
      } else if (lambda < 490) {
  	r <- 0.0;
  	g <- (lambda - 440) / (490 - 440);
  	b <- 1.0;
      } else if (lambda < 510) {
  	r <- 0.0;
  	g <- 1.0;
  	b <- -(lambda - 510) / (510 - 490)
      } else if (lambda < 580) {
  	r <- (lambda - 510) / (580 - 510);
  	g <- 1.0;
  	b <- 0.0;
      } else if (lambda < 645) {
  	r <- 1.0;
  	g <- -(lambda - 645) / (645 - 580);
  	b <- 0.0;
      } else if (lambda < 781) {
  	r <- 1.0;
  	g <- 0.0;
  	b <- 0.0;
      } else {
  	r <- 0.0;
  	g <- 0.0;
  	b <- 0.0;
      }
    
      # Let the intensity fall off near the vision limits
      if (380 <= lambda && lambda < 420) {
  	factor <- 0.3 + 0.7*(lambda - 380) / (420 - 380);
      } else if (lambda < 700) {
  	factor <- 1.0;
      } else if (lambda < 780) {
  	factor <- 0.3 + 0.7*(780 - lambda) / (780 - 700);
      } else {
  	factor <- 0.0;
      }
    
      r <- adjust(r, factor);
      g <- adjust(g, factor);
      b <- adjust(b, factor);
  
      rgb <- cbind(rgb, matrix(c(r,g,b), nrow=3, ncol=1));
    } # for (lambda in lambdas)
  
    rgb;
  } # wavelengthToRgb()

  # 'wavelength' in nm.
  rgb <- wavelengthToRgb(getColorSpace(this), gamma=deviceGamma);
  col <- rgb(rgb[1,], rgb[2,], rgb[3,], maxColorValue=255);

  if (filter)
    col <- viewThroughColorFilter(this, col);
  col;
})  # getColors()



#########################################################################/**
# @RdocMethod getWavelength
#
# @title "Gets the wavelengths (in nm) of the colors"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# @author
#
# \seealso{
#   @seemethod "getFrequency".
#   @seeclass
# }
#
# \keyword{color}
#*/######################################################################### 
setMethodS3("getWavelength", "WavelengthColor", function(this, ...) {
  wl <- as.vector(getColorSpace(this));
  attr(wl, "unit") <- "nm";
  wl;
})


#########################################################################/**
# @RdocMethod range
#
# @title "Gets the (wavelength) range of the colors"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# @author
#
# \seealso{
#   @seemethod "getWavelength".
#   @seeclass
# }
#
# \keyword{color}
#*/######################################################################### 
setMethodS3("range", "WavelengthColor", function(this, na.rm=TRUE, ...) {
  range(getWavelength(this), na.rm=na.rm);
})


#########################################################################/**
# @RdocMethod getFrequency
#
# @title "Gets the frequences (in THz) of the colors"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# @author
#
# \seealso{
#   @seemethod "getWavelength".
#   @seeclass
# }
#
# \keyword{color}
#*/######################################################################### 
setMethodS3("getFrequency", "WavelengthColor", function(this, ...) {
  # The wavelength [nm]
  wl <- getWavelength(this);
  # The wavelength [m]
  wl <- wl * 1e-9;
  # The speed of light [m/s]
  c <- 3e8;
  # The frequency [Hz]
  freq <- c/wl;
  # The frequency [THz];
  freq <- freq / 10^12;
  attr(freq, "unit") <- "THz";
  freq;
})



############################################################################
# HISTORY:
# 2004-10-19
# o Added Rdoc comments.
# 2003-11-19
# o Created from old Colors.R.
# 2002-12-27
# o Added getRgbFromWavelength().
# 2002-10-09
# o Added toGrayScale() and toMonochrome().
# 2002-08-23
# o getRGBorHSV(), which is used by getRGB() and getHSV(), now return NA's
#   where its corresponding input values are NA's.
# 2002-05-31
# o Added invert(), rgbToColors() and getMonochrome().
# 2002-05-11
# o BUG FIX: getHeatColors(), getTopoColors(), getTerrainColors(),
#   getCyanMagentaColors() and getRainbowColors() always returned the last
#   value as NA. This was due to an internal out-of-range array index.
# 2002-05-07
# o Added getRed(), getGreen() and getBlue() because they are simplier than
#   using getRGB() if one just want red, green or blue.
# 2002-04-21
# o getGray() now supports NA values by setting the corresponding color to
#   NA also. In previous versions gray() would generate an error if one
#   tried to generate colors where some values where NA.
# o BUG FIX: Forgot to exclude Inf's and NA's from x.range in rescale1D().
# 2002-04-03
# o Added colorToRGB().
# o Added getHeatColors(), getTopoColors(), getTerrainColors(),
#   getCyanMagentaColors(), getRainbowColors().
# 2002-03-30
# o Replaced demo() with example code.
# o Recoded with setMethodS3's etc.
# 2001-08-09
# o BUG FIX: getRGBorHSV() sometimes generated x == 0, which would give
#   an error. Added x[is.na(x)] <- 0 to solve it.
# 2001-07-02
# o getRGBorHSV() supports both upper and lower case dim argument.
# 2001-06-23
# o Added some Rdoc comments with examples.
# 2001-05-14
# o Added getInternalReferences() for improving gco() performance.
# 2001-05-04
# o Now supports formal attributes.
# 2001-04-11
# o Created from old ColorFactory, GreenColorFactory etc.
# 2001-04-03
# o Created from old DotStyler.
############################################################################
