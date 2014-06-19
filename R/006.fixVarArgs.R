# Added '...' to some base functions. These will later be
# turned into default functions by setMethodS3().

demo <- appendVarArgs(demo)
colnames <- appendVarArgs(colnames)
palette <- appendVarArgs(palette)
#length <- appendVarArgs(length)


############################################################################
# HISTORY:
# 2005-02-20
# o Created to please R CMD check.
############################################################################
