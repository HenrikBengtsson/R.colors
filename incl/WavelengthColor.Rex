require(R.basic)  # image270()

lambda <- seq(from=380, to=779)  # in units of nm.
col <- WavelengthColor(lambda)

filters <- list(
  none   = ColorFilter(),
  gray   = GrayColorFilter()
)

if ("dichromat" %in% library()$results) {
  filter <- append(filter, list(
    deutan    = DichromatColorFilter(type="deutan")
  ))
}


layout(matrix(seq(filters), ncol=1))

ofilter <- Color$getColorFilter()
for (filter in filters) {
  # View the colors through the given filter
  Color$setColorFilter(filter)

  # Display the colors along the wavelength dimension
  opar <- par(mar=c(5,2,6,2)+0.1)
  image270(x=as.matrix(lambda), y=as.matrix(1), z=matrix(lambda, nrow=1), col=getColors(col), xlab=expression(paste("Top: frequency (THz), Bottom: ", lambda, "- wavelength (nm)")), ylab="", axes=FALSE)
  at <- seq(from=min(lambda), to=max(lambda), by=20)
  axis(side=1, at=at)
  freq <- round(getFrequency(WavelengthColor(at)));
  axis(side=3, at=at, labels=freq)
  main <- paste(data.class(col), ": ", paste(range(col),collapse="-"), " nm", sep="");
  if (data.class(filter) != "ColorFilter")
    main <- paste(main, "filter through a", data.class(filter));
  title(main=main);
  par(opar)
}
Color$setColorFilter(ofilter)
