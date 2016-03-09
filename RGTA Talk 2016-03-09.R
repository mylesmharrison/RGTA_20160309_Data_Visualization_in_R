## Greater Toronto Area (GTA) R User Group
## Data Visualization in R
## March 9, 2016
## Myles Harrison
## http://www.everydayanalytics.ca

### BASIC PLOTTING ###
# basic scatterplot 
x <- rnorm(100) 
y <- rnorm(100) 
plot(x,y)

# scatterplot matrix by default
z <- rnorm(100) 
d <- data.frame(x,y,z) 
plot(d) 

# More scatterplots 
plot(mtcars$disp, mtcars$mpg) 

plot(mtcars$disp, mtcars$mpg, pch=16, col='red', 			xlab='Displacement (cu. in.)', 
     ylab='Miles per Gallon (mpg)', 
     main='MPG vs. Displacement for mtcars data set')

plot(mtcars$disp, mtcars$mpg, pch=16, col=mtcars$gear, xlab='Displacement (cu. in.)', ylab='Miles per Gallon (mpg)', main='MPG vs. Displacement for mtcars data set')

plot(mtcars$disp, mtcars$mpg, pch=16, 
     col=mtcars$gear, cex=mtcars$cyl, 
     xlab='Displacement (cu. in.)', ylab='Miles per Gallon (mpg)', main='MPG vs. Displacement for mtcars data set')

plot(mtcars$disp, mtcars$mpg, pch=16, 
     col=mtcars$gear, cex=mtcars$cyl, 
     xlab='Displacement (cu. in.)', ylab='Miles per Gallon (mpg)', main='MPG vs. Displacement for mtcars data set')

gears <- unique(sort(mtcars$gear))
legend("topright",legend=gears,fill=gears) 
l <- lm(mpg~disp, data=mtcars) 
abline(l) 

# line plots 
y <- cumsum(rnorm(1000))
plot(y,type='l') 

y2 <- cumsum(rnorm(1000))
plot(y,type='l',lwd=2, col='red', 
     xlab='x variable', ylab='y variable', main='y vs. x')
lines(y2,lwd=1,lty=2,col='black') 

# Histograms & normal curves #
x <- rnorm(10000)
hist(x)
hist(x, breaks=100, col='red') 

h <- hist(x,breaks=100, col='red') 
plot(h$mids, h$counts, type='b', col='red', pch=16, 			xlab='bin', ylab='count') 

# Overlay / interpolate density 
plot(h, col='red')
multiplier <- h$counts / h$density
curve(dnorm(x)*multiplier[1], add=T, lwd=2) 

# Boxplots # 
boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", 	xlab="Number of Cylinders", ylab="Miles Per Gallon") 
boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", 	xlab="Number of Cylinders", ylab="Miles Per Gallon", 	col=c("red","green","blue")) 

# with jitter #
x <- rnorm(500, mean=1, sd=5)
y <- rnorm(500, mean=5, sd=6.6)
z <- rnorm(500, mean=10, sd=3.3)
d <- data.frame(x,y,z)
boxplot(d) 
stripchart(d, vertical=TRUE, method="jitter", 
           add=TRUE, pch=16, col = rgb(0,0,0,0.5), cex=0.1)

# violin plot # 
library(vioplot)
vioplot(x,y,z, col='white', names=c('x','y','z')) 
title('Violin Plot') 

# COLOR PALETTES IN PRACTICE 
y <- cumsum(rnorm(1000))
plot(y, type='o')

n = 256
palette(heat.colors(n)) 
# Create scaled colour vector 
cols <- (y - min(y))/(max(y) - min(y))*n 
# plot
plot(y,col=cols, pch=16) 

# ColorRampPalette
myPalette <- colorRampPalette(c("red","blue"))(256)
palette(myPalette)
plot(y,col=cols,pch=16) 

## RColorBrewer ##
library(RColorBrewer)

# Sequential - for continuous data from low to high
display.brewer.all(type='seq')

# Qualitative - for categorical data
display.brewer.all(type='qual')

# Diverging emphasizes mid and extremes (e.g. -ve to +ve)
display.brewer.all(type='div')

## RColorBrewer in practice ##
# dense scatterplot
x <- c(rnorm(5000, sd=3.4, mean=5), 
       rnorm(5000,sd=1.5,mean=-1), rnorm(5000,sd=2.2,mean=11)) 
y <- c(rnorm(5000, sd=1.6, mean=-2), 	rnorm(5000,sd=2.5,mean=10), rnorm(5000,sd=1,mean=5)) 
plot(x,y) 
# interpolate using KDE2D 
library(MASS) 
k <- kde2d(x,y,n=150) 
# ugly (using heat.colours) 
image(k) 

# nice (using RColorBrewer) 
cols <- colorRampPalette(brewer.pal(11,'Spectral'))(256) 
cols2 <- colorRampPalette(brewer.pal(11,'RdYlGn'))(256)
image(k, col=rev(cols)) 
image(k, col=rev(cols2)) 

## GGPLOT ##
# scatterplots
library(ggplot2)
x <- rnorm(100)
y <- rnorm(100)
qplot(x)
qplot(x,y) 

# ggplot objects 
z <- rnorm(100) 
d <- data.frame(x,y,z) 
ggplot(d, aes(x,y))
ggplot(d, aes(x,y)) + geom_point()

# Scatterplots in ggplot 
g <- ggplot(mtcars, aes(disp, mpg))
g + geom_point(colour='red', size=5)
g + geom_point(aes(colour=gear), size=5)
g + geom_point(aes(colour=factor(gear)), size=5)

g2 <- g + geom_point(aes(colour=factor(gear), size=cyl)) 
g2
g2 + scale_size(range=c(6,12))
g2 + scale_size(range=c(6,12)) + 
  stat_smooth(method="loess", colour='black') 


# line graph 
x <- seq(1, 1000) 
y <- cumsum(rnorm(1000)) 
d <- data.frame(x,y) 
ggplot(d, aes(x,y)) + geom_line() 

# add another line
y2 <- cumsum(rnorm(1000))
d <- data.frame(x,y,y2)
ggplot(d, aes(x=x)) + geom_line(aes(y=y)) + geom_line(aes(y=y2), colour='red', linetype='dashed') 

# heat mapping
x <- rnorm(5000)
y <- rnorm(5000)
d <- data.frame(x,y)
ggplot(d, aes(x,y)) + stat_bin_2d()
ggplot(d, aes(x,y)) + stat_binhex() 

# density plots calling kde2d
ggplot(d, aes(x,y)) + stat_density_2d(aes(fill=..level..),geom='polygon')
ggplot(d, aes(x,y)) + stat_density_2d(aes(fill=..level..),geom='polygon') + scale_fill_distiller(palette='Spectral') 




