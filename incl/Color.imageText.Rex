# One-dimensional colors
ncolors <- 256/4
x <- seq(0,1, length=ncolors)

layout(matrix(1:4, ncol=2, byrow=TRUE))
opar <- par(mar=c(0,1,3,1)+0.5)

color <- RainbowColor(x);
for (label in c("index", "rgb", "space")) {
  image(color, gridLty=1, gridLwd=3)
  imageText(color, label=label, cex=1)
}

par(opar)
