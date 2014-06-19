########################################################################/**
# @RdocClass Color
#
# @title "Abstract class representing a set of colors"
#
# \description{
#  @classhierarchy
#
#  @get "title".
#
#  Various Color classes can have various ways of representing a set of
#  colors. Where some classes store the colors in a colorspace spanning the
#  red, green, and blue dimensions, other classes may keep a different
#  representation such as the wavelength of the light and so on.
#
#  By calling \code{getColors()} of a Color object the set of colors where
#  each color is a character string with format \code{"\#rrggbb"}, where
#  \code{rr}, \code{gg}, and \code{bb} are hexadecimal values in
#  [0x00-0xff] for the red, green and blue channel, respectively. This
#  format is understood by all plot functions in \R.
#  By default, \code{as.character()} calls \code{getColors()}. 
#  It is expected that \emph{no} subclass changes this.
#
#  \emph{Importantly}, each class implementing its own \code{getColors()} 
#  should before returning the set of colors, pass them through the 
#  user-defined color filter. This is done easiest by calling
#  \code{colors <- viewThroughColorFilter(colors)} before returning, where
#  \code{colors} is the set of colors to be filtered. If the user has not
#  specified a default color filter, the identical colors will be returned.
#  This is very useful because any color can then be filtered without
#  the user having to do it explicitly. For instance, if you want to filter
#  all the colors to see how a "color blind" person preceives the colors,
#  set the default color filter by calling the static method as
#  \code{Color$setColorFilter(DichromatColorFilter())}.
#  For more information about color filters, @see "ColorFilter".
# }
#
# \section{Fields and Methods}{ 
#  @allmethods 
#
# }
#
# @synopsis
#
# @author
#
# @examples "../incl/Color.Rex"
#
# \keyword{color}
#
# \references{
#  [1] cyberglitz, \emph{Color Primer}, webpage, 2003. 
#      \url{http://www.cyberglitz.com/primer.htm}
#  [2] ColorMatch 5K,
#      \url{http://www.colormatch.dk/}.
#      First prize in the Danish 5K Awards 2002.
# }
#
# \seealso{
#   @see "ColorFilter".
#   @see "grDevices::colors".
# }
#*/#########################################################################
setConstructorS3("Color", function() {
  extend(Object(), "Color");
}, abstract=TRUE)



#########################################################################/**
# @RdocMethod as.character
#
# @title "Gets a character representation of the colors"
#
# @synopsis
#
# \description{
#  @get "title".
#  By default this method calls @seemethod "getColors" and it is
#  \emph{expected} that this not overridden by subclasses. 
# }
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns the colors as a @vector of @character strings. For more
#  details see @seemethod "getColors".
# }
#
# \details{
#  To the develop of a new Color class: \emph{Do override this method.}
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   @seemethod "getColors".
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("as.character", "Color", function(x, ...) {
  # To please R CMD check
  this <- x;

  getColors(this);
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
#     filter, see @seemethod "viewThroughColorFilter", otherwise not.}
#
#   By default the getColors() method does not take any other arguments, 
#   but subclasses may provide optional arguments for convenience, e.g.
#   gamma correction parameters etc.
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @vector of @character strings of length equal to the number of 
#  colors in the Color object. The returned colors are character string of
#  format \code{"#nnnnnn"} where \code{n} is a hexadecimal digit.
#  For more information about this format @see "grDevices::colors".
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   @seemethod "[", 
#   @seemethod "as.character", 
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("getColors", "Color", function(this, filter=TRUE, ...) {
  throw("Not implemented.");
})



#########################################################################/**
# @RdocMethod [
#
# @title "Gets a subset of the colors by indices"
#
# @synopsis
#
# \description{
#  @get "title".
#  This is just a convenient wrapper for \code{getColors(col)[indices]}.
# }
#
# \arguments{
#   \item{indices}{A @vector of indices specifying which of the colors
#      to be returned.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a subset of the colors returned by @seemethod "getColors".
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
setMethodS3("[", "Color", function(this, indices, ...) {
   getColors(this)[indices];
}, createGeneric=FALSE)




#########################################################################/**
# @RdocMethod length
#
# @title "Gets the number of colors"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# \value{
#  Returns the number of colors as an @integer.
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
setMethodS3("length", "Color", function(x) {
  # To please R CMD check
  this <- x;

  nrow(getColorSpace(this));
}, appendVarArgs=FALSE)



#########################################################################/**
# @RdocMethod seq
#
# @title "Gets the indices of all colors"
#
# @synopsis
#
# \description{
#  @get "title". This makes it easy to iterate over all colors (also when
#  there are no colors).
# }
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns an @integer @vector of the indices of this vector.
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
setMethodS3("seq", "Color", function(this, ...) {
  seq(length=length(this));
})



#########################################################################/**
# @RdocMethod palette
#
# @title "Use these colors as the color palette in all graphics"
#
# @synopsis
#
# \description{
#  @get "title".
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   @see "grDevices::palette".
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("palette", "Color", function(this, ...) {
  palette(getColors(this));
})


#########################################################################/**
# @RdocMethod getColorSpace
#
# @title "Gets the internal representation of the colors"
#
# @synopsis
#
# \description{
#  @get "title".
#  
#  To the developer of a Color class: 
#  \emph{This method is to be implemented in a subclass.}
# }
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns the internal represenation of the colors. This can be a
#  @vector, a @matrix, but any class is possible.
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
setMethodS3("getColorSpace", "Color", abstract=TRUE);


setMethodS3("colnames", "Color", function(this, ...) {
  colnames(getColorSpace(this));
}, private=TRUE)



#########################################################################/**
# @RdocMethod getColorFilter
#
# @title "Gets the default color filter"
#
# @synopsis
#
# \description{
#  @get "title", which all colors are filtered through.
#  If @NULL, no filtering is applied.
# }
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns @see "ColorFilter" or @NULL.
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   To set or reset the default filter see @seemethod "setColorFilter".
#   To filter colors through the default filter see 
#   @seemethod "viewThroughColorFilter".
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("getColorFilter", "Color", function(static, ...) {
  getOption("colorFilter");
}, static=TRUE)



#########################################################################/**
# @RdocMethod setColorFilter
#
# @title "Sets the default color filter"
#
# @synopsis
#
# \description{
#  @get "title", which all colors will be filtered through.
#  If @NULL, no filtering will done.
# }
#
# \arguments{
#   \item{filter}{A @see "ColorFilter" or @NULL. If @NULL, no filtering
#    will be done.}
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns (invisibly) the previous @see "ColorFilter" used.
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   To get the default filter see @seemethod "getColorFilter".
#   To filter colors through the default filter see 
#   @seemethod "viewThroughColorFilter".
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("setColorFilter", "Color", function(static, filter=NULL, ...) {
  if (is.null(filter)) {
  } else if (!inherits(filter, "ColorFilter")) {
    throw("Unknown class of argument 'filter': ", data.class(filter));
  }
  opt <- options(colorFilter=filter)[[1]];
  invisible(opt);
}, static=TRUE)



#########################################################################/**
# @RdocMethod viewThroughColorFilter
#
# @title "Filters a set of colors using the default filter"
#
# @synopsis
#
# \description{
#  @get "title".
#  
#  To the developer of a Color class: 
#  \emph{This method is to be implemented in a subclass.}
# }
#
# @author
#
# \keyword{color}
#
# \seealso{
#   To set or reset the default filter see @seemethod "setColorFilter".
#   To get the default filter see @seemethod "getColorFilter".
#   @seeclass
# }
#*/######################################################################### 
setMethodS3("viewThroughColorFilter", "Color", function(this, colors, ...) {
  filter <- getOption("colorFilter");
  if (!is.null(filter)) {
    options(colorFilter=NULL);
    colors <- getColors(filter, colors);
    on.exit(options(colorFilter=filter));
  }
  as.character(colors);
}, static=TRUE)



 

############################################################################
# HISTORY:
# 2003-12-10
# o Added plotParallel() with Rdoc comments and everything.
# 2003-11-21
# o Added the argument 'filter=TRUE' to getColors() to make it possible
#   for other methods to get the RGB colors without filtering.
# o BUG FIX: "[" had a typo.
# 2003-11-20
# o Added Rdoc comments and some minor updates too.
# 2003-11-19
# o Created.
############################################################################

