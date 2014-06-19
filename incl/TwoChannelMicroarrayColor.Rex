layout(matrix(1:4, ncol=2, byrow=TRUE))

TwoChannelMicroarrayColor$demo()

if ("dichromat" %in% library()$results) {
  ofilter <- Color$setColorFilter(DichromatColorFilter())
  TwoChannelMicroarrayColor$demo()
  
  Color$setColorFilter(DichromatColorFilter(type="protan"))
  TwoChannelMicroarrayColor$demo()
}

Color$setColorFilter(GrayColorFilter())
TwoChannelMicroarrayColor$demo()

if ("dichromat" %in% library()$results) {
  Color$setColorFilter(ofilter)
}