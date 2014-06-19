setMethodS3("validateArgumentColorsK", "Color", function(static, args, maxColorValue=1, K=1, ...) {
  if (!is.na(maxColorValue) && maxColorValue < 0)
    throw("Argument 'maxColorValue' can not be negative: ", maxColorValue);

  argsNames <- names(args);
  nargs <- length(args);

  if (nargs == 0) {
    return(NULL);
  } else if (nargs != K) {
    throw("Tried to create a ", K, "-dimensional color based on ", nargs, " dimensions.");
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 0. If the first argument is NULL, ignore the others and return NULL.
  #
  # Used by for instance the HsvgColor class.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.null(args[[1]]))
    return(NULL);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 1. Check how many elements 'args' contains.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  len <- unlist(lapply(args, FUN=length));
  if (all(len == 0))
    return(NULL);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 2. Check if the first argument is the only argument and the others
  #    are empty.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (K > 1 && !all(len[-1] == 0) && any(len[-1] == 0)) {
    throw("Either all arguments must be specified or just the first ('", argsNames[1], "').");
  }

  if (K > 1 && all(len[-1] == 0)) {
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # 3. Then, check if it a list, a matrix, or a data.frame.
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    x <- args[[1]];
    if (!is.list(x) && !is.matrix(x) && is.data.frame(x)) {
      throw("If only the first argument ('", argsNames[1], "') is given, it has to be either a list, a matrix or a data frame: ", data.class(x));
    }

    if (is.matrix(x))
      x <- as.data.frame(x);

    if (is.data.frame(x))
      x <- as.list(x);

    # Here we know that we are working with a list
    if (length(x) != K) {
      throw("Only the first argument ('", argsNames[1], "') was given, but it does not contain ", K, " fields: ", length(x));
    }

    # Get the field names. If possible, use these names to extract the values, 
    # otherwise assume that the columns are in order.
    names <- names(x);
    if (!is.null(names)) {
      # Work only with lower case names
      names <- tolower(names);

      # If the column names are not unique, we have to use the order.
      if (length(unique(names)) != K) {
      }
      # If the column names are the same as the names of the argument...
      else if (identical(sort(names), sort(argsNames))) {
        names(x) <- names;
        x <- x[argsNames];
      # otherwise, try to match the first character only...
      # If the field names are not unique, we have to use the order.
      } else {
        names <- substring(names, 1,1);
        if (length(unique(names)) != K) {
        } else {
          argsNames <- substring(argsNames, 1,1);
          if (length(unique(argsNames)) != K) {
          # If the field names are the same as the names of the argument...
          } else if (identical(sort(names), sort(argsNames))) {
            names(x) <- names;
            x <- x[argsNames];
          }
        }
      }
    }
    args <- x;
    rm(x);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 4. At this point we know that 'args' contains K elements, possible
  #    of different lengths though.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  len <- unlist(lapply(args, FUN=length));
  maxLen <- max(len);

  # If no values are given, return NULL.
  if (maxLen == 0)
    return(NULL);

  # If some vectors are shorter than others, loop over the values such that the
  # obtain the same lengths.
  maxLen <- max(len);
  for (kk in 1:K)
    args[[kk]] <- rep(args[[kk]], length.out=maxLen);

  # Make into a n-by-ndim matrix
  args <- as.data.frame(args);
  args <- as.matrix(args);

  # Set non-finite values to NA.
  finite <- as.vector(apply(args, MARGIN=1, FUN=function(x) all(is.finite(x))));
  args[!finite,] <- NA;


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 5. At this point we know that 'args' is a matrix with K columns with
  #    possible NAs, but no Inf or NaN.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 6. Rescale to [0,1] using maxColorValue
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (!is.na(maxColorValue)) {
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # 7. Assert that all values are in [0,1].
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (maxColorValue != 1)
      args <- (1/maxColorValue) * args;

    if (any(args < 0 | args > 1, na.rm=TRUE)) {
      throw("Out of range [0,", maxColorValue, "].");
    }
  }
 

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 8. Make into a matrix, with the specified column names
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  args <- as.matrix(args);
  colnames(args) <- argsNames;

  args;
}, static=TRUE, private=TRUE)
