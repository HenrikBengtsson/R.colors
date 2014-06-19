# One-dimensional colors
ncolors <- 256
x <- seq(0,1, length=ncolors)
ch1 <- x
ch2 <- rev(x)

colors <- list(
  # One-dimensional colors
  red      = RedColor(x),
  green    = GreenColor(x),
  blue     = BlueColor(x),
  gray     = GrayColor(x),
  spectrum = WavelengthColor(seq(380,779,length=ncolors)),
  heat     = HeatColor(x),
  rainbow  = RainbowColor(x),
  topo     = TopologyColor(x),
  terrain  = TerrainColor(x),
  cm       = CyanMagentaColor(x),

  # Two-dimensional colors
  rg = TwoChannelMicroarrayColor(ch1,ch2, maxColorValue=1),
  yb = TwoChannelMicroarrayColor(ch1,ch2, hueRange=c(HsvgColor$YELLOW.HUE,HsvgColor$BLUE.HUE), maxColorValue=1),
  
  # Three-dimensional colors
  rgb  = RgbColor(x,rev(x),sample(x)),
  hcl  = HclColor(x),

  # Four-dimensional colors
  hsvg = HsvgColor(x),
  cmyk = CmykColor(x,sample(x),rev(x),x/2)
)

layout(matrix(1:16, ncol=4, byrow=TRUE))
opar <- par(mar=c(0,0,1,0)+0.5)

for(color in colors) {
  plot3d(RgbColor(color), axes=FALSE, xlab="", ylab="", zlab="", phi=35, theta=30, pch=20, cex=2)
  title(main=data.class(color), cex=0.7, line=0)
}

par(opar)
