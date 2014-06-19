library(R.colors)
rm(list=ls())

source("Color.R")

source("OneDimensionalColor.R")
source("RedColor.R")
source("GreenColor.R")
source("BlueColor.R")
source("GrayColor.R")
source("WavelengthColor.R")
source("HeatColor.R")
source("RainbowColor.R")
source("TopologyColor.R")
source("TerrainColor.R")
source("CyanMagentaColor.R")

source("TwoChannelMicroarrayColor.R");
source("TwoDimensionalColor.R")

source("ThreeDimensionalColor.R")
source("RgbColor.R")
source("HclColor.R")

source("FourDimensionalColor.R")
source("HsvgColor.R")
source("CmykColor.R")

source("ColorFilter.R")

source("RgbColorFilter.R")
source("GrayColorFilter.R")
source("NegativeColorFilter.R")
source("MonochromeColorFilter.R")
source("DichromatColorFilter.R")
source("SerialColorFilter.R")


setMethodS3("getColorSpace", "RgbColor", function(this) {
  NextMethod("getColorSpace");
})

setMethodS3("getColorSpace", "HclColor", function(this) {
  NextMethod("getColorSpace");
})

setMethodS3("getColorSpace", "HsvgColor", function(this) {
  NextMethod("getColorSpace");
})
