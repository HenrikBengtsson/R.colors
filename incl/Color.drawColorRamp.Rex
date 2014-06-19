# Create data
x <- seq(from=0, to=3*base::pi, length=101)
y <- sin(x)

# Generate colors
y01 <- (y-min(y))/diff(range(y))
col <- RainbowColor(y01)
rampValues <- seq(from=-1,to=1, length=51);
ramp01 <- (rampValues-min(rampValues))/diff(range(rampValues))
ramp <- RainbowColor(ramp01)

# Plot data
opar <- par(mar=c(5,4,4,6)+0.1, xpd=TRUE)
plot(x,y, col=getColors(col), lwd=2)

# Plot color ramp
usr <- par("usr")
dx <- diff(usr[1:2]);
dy <- diff(usr[3:4]);
sh <- 0.05;

# Draw horisontal ramp
drawColorRamp(col, x=usr[1], y=usr[4]+0.01*dy, width=dx, height=sh*dy, angle=0, yadj=0, borderColor=NA)

# Draw vertical ramp
xy <- drawColorRamp(ramp, x=usr[2]+0.01*dx, y=usr[3], width=dy, height=sh*dx, angle=90, xadj=0, borderColor=NA)
incl <- seq(from=1, to=length(rampValues), length=11);
text(xy$below$x[incl], xy$below$y[incl], label=sapply(rampValues[incl], FUN=sprintf, fmt="%+3.2f"), adj=c(-0.1,0.5), cex=0.8)

par(opar)
