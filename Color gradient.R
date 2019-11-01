# create color gradients
# https://stackoverflow.com/questions/13353213/gradient-of-n-colors-ranging-from-color-1-and-color-2
library(colorRampPalette)
colfunc <- colorRampPalette(c("red", "yellow", "green", "blue", "cyan", "magenta")) 
# change the above line depending on colors needed
colors() # to see all the list of colors available
colfunc(1000)
plot(rep(1,1000),col=colfunc(1000),pch=15,cex=3)