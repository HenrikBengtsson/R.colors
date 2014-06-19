########################################################################/**
# @RdocClass ColorFilter
#
# @title "Class representing a color filter"
#
# \description{
#  @classhierarchy
#
#  @get "title".
#
#  This "mother" of ColorFilter classes works also as a "transparent" 
#  filter, i.e. as a filter that does not filter at all. 
# }
#
# \section{Fields and Methods}{
#  @allmethods
# }
#
# @synopsis
# 
# @author
#
# @examples "../incl/ColorFilter.Rex"
#
# \keyword{color}
#
# \seealso{
#   @see "Color".
# }
#*/#########################################################################
setConstructorS3("ColorFilter", function() {
  extend(Object(), "ColorFilter")
})



#########################################################################/**
# @RdocMethod getColors
#
# @title "Filters a set of colors"
#
# @synopsis
#
# \description{
#  @get "title" and returns a @character representation of the filtered
#  colors. 
#
#  For this class, no filtering is done, but subclasses will override this.
# }
#
# \arguments{
#   By default the getColors() method does not take any arguments, but
#   subclasses may provide optional arguments for convenience, e.g.
#   gamma correction parameters etc.
# }
#
# \value{
#  Returns a @character string @vector of the filtered colors, which is
#  of the same length as the number of input colors.
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
setMethodS3("getColors", "ColorFilter", function(this, colors, ...) {
  as.character(colors);
})



############################################################################
# HISTORY:
# 2003-11-20
# o Added Rdoc comments.
# 2003-11-19
# o Created.
############################################################################
