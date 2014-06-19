########################################################################/**
# @RdocClass SerialColorFilter
#
# @title "Class representing a set of color filters connected serially"
#
# \description{
#  @classhierarchy
#
#  @get "title". 
#
#  With this filter you can "link" together several @see "ColorFilter"s 
#  to work as it was one.
# }
#
# \section{Fields and Methods}{
#  @allmethods
# }
#
# @synopsis
# 
# \arguments{
#   \item{filters}{An ordered @list of @see "ColorFilter"s to be linked.}
# }
#
# @author
#
# \keyword{color}
#*/#########################################################################
setConstructorS3("SerialColorFilter", function(filters=NULL) {
  if (!is.null(filters)) {
    for (filter in filters) 
     if (!inherits(filter, "ColorFilter")) {
       throw("One of the 'filters' argument was of an unknown class: ", data.class(filter));
     }
  }

  extend(ColorFilter(), "SerialColorFilter",
    filters = filters
  )
})


setMethodS3("getColors", "SerialColorFilter", function(this, colors, ...) {
  for (filter in this$filters) {
    colors <- getColors(filter, colors);
  }
  colors;
})


############################################################################
# HISTORY:
# 2003-11-19
# o Created.
############################################################################
