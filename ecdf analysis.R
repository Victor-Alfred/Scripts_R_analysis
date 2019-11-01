
################### ECDF PLOTS _ECAD

rm(list=ls())

dat <- data.frame(read.csv("all_data.csv"))
summary(dat$Genotype)
library(lattice)
library(latticeExtra)

set.seed(42)

Dhc1_noCD8 <- subset(dat, Genotype == "Dhc1_noCD8")
Dhc1_CD8 <- subset(dat, Genotype == "Dhc1_CD8")

Dhc2_noCD8 <- subset(dat, Genotype == "Dhc2_noCD8")
Dhc2_CD8 <- subset(dat, Genotype == "Dhc2_CD8")

w1118_noCD8 <- subset(dat, Genotype == "w1118_noCD8")
w1118_CD8 <- subset(dat, Genotype == "w1118_CD8")

Khc1_noCD8 <- subset(dat, Genotype == "Khc1_noCD8")
Khc1_CD8 <- subset(dat, Genotype == "Khc1_CD8")

Khc2_noCD8 <- subset(dat, Genotype == "Khc2_noCD8")
Khc2_CD8 <- subset(dat, Genotype == "Khc2_CD8")





ecdfplot(~ Dhc1_noCD8$Rel_dist + Dhc1_CD8$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)

ecdfplot(~ Dhc2_noCD8$Rel_dist + Dhc2_CD8$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)

ecdfplot(~ w1118_noCD8$Rel_dist + w1118_CD8$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)

ecdfplot(~ Khc1_noCD8$Rel_dist + Khc1_CD8$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)

ecdfplot(~ Khc2_noCD8$Rel_dist + Khc2_CD8$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)



boxplot(Rel_dist~ Genotype, data = dat, lwd = 2, ylab = 'Rel_dist')
stripchart(Rel_dist ~ Genotype, vertical = TRUE, data = dat, 
           method = "jitter", add = TRUE, pch = 20, col = 'blue')




Dhc1_noCD8_sample <- Dhc1_noCD8[-c(174:nrow(Dhc1_noCD8)),] 
Dhc1_CD8_sample <- Dhc1_CD8[-c(159:nrow(Dhc1_CD8)),] 
ecdfplot(~ Dhc1_noCD8_sample$Rel_dist + Dhc1_CD8_sample$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)

Dhc2_noCD8_sample <- Dhc2_noCD8[-c(104:nrow(Dhc2_noCD8)),] 
Dhc2_CD8_sample <- Dhc2_CD8[-c(109:nrow(Dhc2_CD8)),] 
ecdfplot(~ Dhc2_noCD8_sample$Rel_dist + Dhc2_CD8_sample$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)

Khc1_noCD8_sample <- Khc1_noCD8[-c(106:nrow(Khc1_noCD8)),] 
Khc1_CD8_sample <- Khc1_CD8[-c(104:nrow(Khc1_CD8)),] 
ecdfplot(~ Khc1_noCD8_sample$Rel_dist + Khc1_CD8_sample$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)






ggplot(subset(dat, Genotype == "Dhc1_CD8" | Genotype == "Dhc2_CD8" | Genotype == "Khc1_CD8" | Genotype == "w1118_CD8" | Genotype == "Khc2_CD8"), aes(x=Rel_dist, colour=Genotype)) + geom_density(size =1) + 
  theme_bw() + theme(axis.line = element_line(colour = "black"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.title = element_text(size =25),
                     axis.text.x = element_text(size = 18, hjust =1),
                     axis.text.y = element_text(size = 18, vjust =1.5),
                     legend.title = element_text(size = 25),
                     legend.text = element_text(size = 18),
                     legend.position = "bottom") + 
  ylab("Density") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,0.06, by = 0.01)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,90, by = 15))




require(Hmisc)
Ecdf(dat$Rel_dist, group = dat$Genotype, lty = c(1, 2, 3, 4), 
     lwd = 4, label.curves = list(method = 'arrow', 
                                  keys = "lines", lwd = 2), 
     subtitles = FALSE, xlab = "Latency", 
     ylab = "Proportion latency <= x")
axis(2, cex.axis=1.5)
axis(1, cex.axis=1.5)


require("dplyr")

sample_n(df, 10)

Dhc1_noCD8_sample <- sample_n(Dhc1_noCD8, 150)
Dhc1_CD8_sample <- sample_n(Dhc1_CD8, 121)
ecdfplot(~ Dhc1_noCD8_sample$Rel_dist + Dhc1_CD8_sample$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)

Dhc2_noCD8_sample <- sample_n(Dhc2_noCD8, 113)
Dhc2_CD8_sample <- sample_n(Dhc2_CD8, 100)
ecdfplot(~ Dhc2_noCD8_sample$Rel_dist + Dhc2_CD8_sample$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)

Khc1_noCD8_sample <- sample_n(Khc1_noCD8, 138)
Khc1_CD8_sample <- sample_n(Khc1_CD8, 164)
ecdfplot(~ Khc1_noCD8_sample$Rel_dist + Khc1_CD8_sample$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)

Khc2_noCD8_sample <- sample_n(Khc2_noCD8, 176)
Khc2_CD8_sample <- Khc2_CD8
ecdfplot(~ Khc2_noCD8_sample$Rel_dist + Khc2_CD8_sample$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)

w1118_noCD8_sample <- w1118_noCD8
w1118_CD8_sample <- w1118_CD8
ecdfplot(~ w1118_noCD8_sample$Rel_dist + w1118_CD8_sample$Rel_dist, data=dat, auto.key=list(space='right'), lwd = 2)




set.seed(42)
ecdf1 <- ecdf(Dhc1_noCD8_sample$Rel_dist)
ecdf2 <- ecdf(Dhc1_CD8_sample$Rel_dist)
plot(ecdf2, verticals=TRUE, do.points=FALSE, lwd =4, labels = FALSE,  xlab="Relative distance from cell centre",
     ylab="", cex.lab = 2, col = 'red', ylim=c(0,1), xlim=c(0,1), yaxs = "i", xaxs = "i",  bty="n")
plot(ecdf1, verticals=TRUE, do.points=FALSE, add=TRUE, col='black', lwd = 4)
axis(2, cex.axis=1.5, las = 1)
axis(1, cex.axis=1.5)
legend(0.01, 0.95, legend=c("Control region", "Experimental region"),
       col=c("black", "red"), lty=1:1, cex=1.4, lwd = 3, box.lty=0)
title(ylab=expression(italic(ecdf)), line=2.63, cex.lab=2)



set.seed(42)
ecdf1 <- ecdf(Dhc2_noCD8_sample$Rel_dist)
ecdf2 <- ecdf(Dhc2_CD8_sample$Rel_dist)
plot(ecdf2, verticals=TRUE, do.points=FALSE, lwd =4, labels = FALSE,  xlab="Relative distance from cell centre",
     ylab="", cex.lab = 2, col = 'red', ylim=c(0,1), xlim=c(0,1), yaxs = "i", xaxs = "i",  bty="n")
plot(ecdf1, verticals=TRUE, do.points=FALSE, add=TRUE, col='black', lwd = 4)
axis(2, cex.axis=1.5, las = 1)
axis(1, cex.axis=1.5)
legend(0.01, 0.95, legend=c("Control region", "Experimental region"),
       col=c("black", "red"), lty=1:1, cex=1.4, lwd = 3, box.lty=0)
title(ylab=expression(italic(ecdf)), line=2.63, cex.lab=2)



set.seed(42)
ecdf1 <- ecdf(Khc1_noCD8_sample$Rel_dist)
ecdf2 <- ecdf(Khc1_CD8_sample$Rel_dist)
plot(ecdf2, verticals=TRUE, do.points=FALSE, lwd =4, labels = FALSE,  xlab="Relative distance from cell centre",
     ylab="", cex.lab = 2, col = 'red', ylim=c(0,1), xlim=c(0,1), yaxs = "i", xaxs = "i",  bty="n")
plot(ecdf1, verticals=TRUE, do.points=FALSE, add=TRUE, col='black', lwd = 4)
axis(2, cex.axis=1.5, las = 1)
axis(1, cex.axis=1.5)
legend(0.01, 0.95, legend=c("Control region", "Experimental region"),
       col=c("black", "red"), lty=1:1, cex=1.4, lwd = 3, box.lty=0)
title(ylab=expression(italic(ecdf)), line=2.63, cex.lab=2)


set.seed(42)
ecdf1 <- ecdf(Khc2_noCD8_sample$Rel_dist)
ecdf2 <- ecdf(Khc2_CD8_sample$Rel_dist)
plot(ecdf2, verticals=TRUE, do.points=FALSE, lwd =4, labels = FALSE,  xlab="Relative distance from cell centre",
     ylab="", cex.lab = 2, col = 'red', ylim=c(0,1), xlim=c(0,1), yaxs = "i", xaxs = "i",  bty="n")
plot(ecdf1, verticals=TRUE, do.points=FALSE, add=TRUE, col='black', lwd = 4)
axis(2, cex.axis=1.5, las = 1)
axis(1, cex.axis=1.5)
legend(0.01, 0.95, legend=c("Control region", "Experimental region"),
       col=c("black", "red"), lty=1:1, cex=1.4, lwd = 3, box.lty=0)
title(ylab=expression(italic(ecdf)), line=2.63, cex.lab=2)


set.seed(42)
ecdf1 <- ecdf(w1118_noCD8_sample$Rel_dist)
ecdf2 <- ecdf(w1118_CD8_sample$Rel_dist)
plot(ecdf2, verticals=TRUE, do.points=FALSE, lwd =4, labels = FALSE,  xlab="Relative distance from cell centre",
     ylab="", cex.lab = 2, col = 'red', ylim=c(0,1), xlim=c(0,1), yaxs = "i", xaxs = "i",  bty="n")
plot(ecdf1, verticals=TRUE, do.points=FALSE, add=TRUE, col='black', lwd = 4)
axis(2, cex.axis=1.5, las = 1)
axis(1, cex.axis=1.5)
legend(0.01, 0.95, legend=c("Control region", "Experimental region"),
       col=c("black", "red"), lty=1:1, cex=1.4, lwd = 3, box.lty=0)
title(ylab=expression(italic(ecdf)), line=2.63, cex.lab=2)


#############################
# Reading from sample:  FOR THE PAPER---- FINAL FIGURES WITH INCREASED FONTS

rm(list=ls())

dat <- data.frame(read.csv("all_data.csv"))
summary(dat$Genotype)
library(lattice)
library(latticeExtra)

w1118_noCD8 <- subset(dat, Genotype == "w1118_noCD8")
w1118_CD8 <- subset(dat, Genotype == "w1118_CD8")

Khc1_noCD8_sample <- data.frame(read.csv('Khc1_noCD8_sample.csv'))
Khc1_CD8_sample <- data.frame(read.csv('Khc1_CD8_sample.csv'))

Khc2_noCD8_sample <- data.frame(read.csv('Khc2_noCD8_sample.csv'))
Khc2_CD8_sample <- data.frame(read.csv('Khc2_CD8_sample.csv'))

Dhc1_noCD8_sample <- data.frame(read.csv('Dhc1_noCD8_sample.csv'))
Dhc1_CD8_sample <- data.frame(read.csv('Dhc1_CD8_sample.csv'))

Dhc2_noCD8_sample <- data.frame(read.csv('Dhc2_noCD8_sample.csv'))
Dhc2_CD8_sample <- data.frame(read.csv('Dhc2_CD8_sample.csv'))


set.seed(42)
ecdf1 <- ecdf(w1118_noCD8$Rel_dist)
ecdf2 <- ecdf(w1118_CD8$Rel_dist)
plot(ecdf2, verticals=TRUE, do.points=FALSE, lwd =4, labels = FALSE,  xlab="Relative distance from cell centre",
     ylab="", cex.lab = 2, col = 'red', ylim=c(0,1), xlim=c(0,1), yaxs = "i", xaxs = "i",  bty="n")
plot(ecdf1, verticals=TRUE, do.points=FALSE, add=TRUE, col='black', lwd = 4)
axis(2, cex.axis=2, las = 1)
axis(1, cex.axis=2)
legend(0.01, 0.95, legend=c("Control region", "Experimental region"),
       col=c("black", "red"), lty=1:1, cex=1.4, lwd = 3, box.lty=0)
title(ylab=expression(italic(ecdf)), line=2.63, cex.lab=2)


###### FINAL PLOT SCRIPTS

tiff("w1118_CDF_final.tiff", height = 20, width = 35, units='cm', compression = "lzw", res = 300)
ggplot() + 
  stat_ecdf(data=w1118_noCD8, aes(x=Rel_dist, colour='Control region'), size = 3.5) + 
  stat_ecdf(data=w1118_CD8, aes(x=Rel_dist, colour='Experimental region'), size = 3.5) + 
  theme_bw() + theme(axis.line = element_line(colour = "black", size = 1),
                     axis.ticks.length=unit(.25, "cm"),
                     plot.margin = unit(c(1,1,1,1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks = element_line(size = 1),
                     axis.title.x = element_text(size =55, vjust = -6),
                     axis.title.y = element_text(size =55, face = "italic", vjust = 8),
                     axis.text.x = element_text(size = 50, colour = "black"),
                     axis.text.y = element_text(size = 50, colour="black"),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 50, margin = margin(t = 10)), # margin adjusts the distance between the legend texts
                     legend.key.width=unit(1,"cm"), # adjusts the line length of legend
                     legend.spacing.x = unit(0.3, 'cm'), # adjusts the distance between line and text of legend
                     legend.position=c(0.35,0.95)) + 
  ylab("ecdf") + xlab("Relative distance from cell centre") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,1, by = 0.2)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,1.2, by = 0.2))  +
  scale_color_manual(values=c("black", "red"))
dev.off()


tiff("Khc1_CDF_final.tiff", height = 20, width = 35, units='cm', compression = "lzw", res = 300)
ggplot() + 
  stat_ecdf(data=Khc1_noCD8_sample, aes(x=Rel_dist, colour='Control region'), size = 3.5) + 
  stat_ecdf(data=Khc1_CD8_sample, aes(x=Rel_dist, colour='Experimental region'), size = 3.5) + 
  theme_bw() + theme(axis.line = element_line(colour = "black", size = 1),
                     axis.ticks.length=unit(.25, "cm"),
                     plot.margin = unit(c(1,1,1,1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks = element_line(size = 1),
                     axis.title.x = element_text(size =55, vjust = -6),
                     axis.title.y = element_text(size =55, face = "italic", vjust = 8),
                     axis.text.x = element_text(size = 50, colour = "black"),
                     axis.text.y = element_text(size = 50, colour="black"),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 50, margin = margin(t = 10)), # margin adjusts the distance between the legend texts
                     legend.key.width=unit(1,"cm"), # adjusts the line length of legend
                     legend.spacing.x = unit(0.3, 'cm'), # adjusts the distance between line and text of legend
                     legend.position=c(0.35,0.95)) + 
  ylab("ecdf") + xlab("Relative distance from cell centre") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,1, by = 0.2)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,1.2, by = 0.2))  +
  scale_color_manual(values=c("black", "red"))
dev.off()


tiff("Khc2_CDF_final.tiff", height = 20, width = 35, units='cm', compression = "lzw", res = 300)
ggplot() + 
  stat_ecdf(data=Khc2_noCD8_sample, aes(x=Rel_dist, colour='Control region'), size = 3.5) + 
  stat_ecdf(data=Khc2_CD8_sample, aes(x=Rel_dist, colour='Experimental region'), size = 3.5) + 
  theme_bw() + theme(axis.line = element_line(colour = "black", size = 1),
                     axis.ticks.length=unit(.25, "cm"),
                     plot.margin = unit(c(1,1,1,1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks = element_line(size = 1),
                     axis.title.x = element_text(size =55, vjust = -6),
                     axis.title.y = element_text(size =55, face = "italic", vjust = 8),
                     axis.text.x = element_text(size = 50, colour = "black"),
                     axis.text.y = element_text(size = 50, colour="black"),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 50, margin = margin(t = 10)), # margin adjusts the distance between the legend texts
                     legend.key.width=unit(1,"cm"), # adjusts the line length of legend
                     legend.spacing.x = unit(0.3, 'cm'), # adjusts the distance between line and text of legend
                     legend.position=c(0.35,0.95)) + 
  ylab("ecdf") + xlab("Relative distance from cell centre") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,1, by = 0.2)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,1.2, by = 0.2))  +
  scale_color_manual(values=c("black", "red"))
dev.off()


tiff("Dhc1_CDF_final.tiff", height = 20, width = 35, units='cm', compression = "lzw", res = 300)
ggplot() + 
  stat_ecdf(data=Dhc1_noCD8_sample, aes(x=Rel_dist, colour='Control region'), size = 3.5) + 
  stat_ecdf(data=Dhc1_CD8_sample, aes(x=Rel_dist, colour='Experimental region'), size = 3.5) + 
  theme_bw() + theme(axis.line = element_line(colour = "black", size = 1),
                     axis.ticks.length=unit(.25, "cm"),
                     plot.margin = unit(c(1,1,1,1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks = element_line(size = 1),
                     axis.title.x = element_text(size =55, vjust = -6),
                     axis.title.y = element_text(size =55, face = "italic", vjust = 8),
                     axis.text.x = element_text(size = 50, colour = "black"),
                     axis.text.y = element_text(size = 50, colour="black"),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 50, margin = margin(t = 10)), # margin adjusts the distance between the legend texts
                     legend.key.width=unit(1,"cm"), # adjusts the line length of legend
                     legend.spacing.x = unit(0.3, 'cm'), # adjusts the distance between line and text of legend
                     legend.position=c(0.35,0.95)) + 
  ylab("ecdf") + xlab("Relative distance from cell centre") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,1, by = 0.2)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,1.2, by = 0.2))  +
  scale_color_manual(values=c("black", "red"))
dev.off()


tiff("Dhc2_CDF_final.tiff", height = 20, width = 35, units='cm', compression = "lzw", res = 300)
ggplot() + 
  stat_ecdf(data=Dhc2_noCD8_sample, aes(x=Rel_dist, colour='Control region'), size = 3.5) + 
  stat_ecdf(data=Dhc2_CD8_sample, aes(x=Rel_dist, colour='Experimental region'), size = 3.5) + 
  theme_bw() + theme(axis.line = element_line(colour = "black", size = 1),
                     axis.ticks.length=unit(.25, "cm"),
                     plot.margin = unit(c(1,1,1,1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks = element_line(size = 1),
                     axis.title.x = element_text(size =55, vjust = -6),
                     axis.title.y = element_text(size =55, face = "italic", vjust = 8),
                     axis.text.x = element_text(size = 50, colour = "black"),
                     axis.text.y = element_text(size = 50, colour="black"),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 50, margin = margin(t = 10)), # margin adjusts the distance between the legend texts
                     legend.key.width=unit(1,"cm"), # adjusts the line length of legend
                     legend.spacing.x = unit(0.3, 'cm'), # adjusts the distance between line and text of legend
                     legend.position=c(0.35,0.95)) + 
  ylab("ecdf") + xlab("Relative distance from cell centre") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,1, by = 0.2)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,1, by = 0.2))  +
  scale_color_manual(values=c("black", "red"))
dev.off()









######### ARF GFP DISTANCES
require(ggplot2)
dat <- data.frame(read.csv('Arf1GFP_rel_dist.csv'))

Control <- subset(dat, Genotype == "Control")
Khc1 <- subset(dat, Genotype == "Khc35770")
Dhc1 <- subset(dat, Genotype == "Dhc36583")
Dhc2 <- subset(dat, Genotype == "Dhc36698")

tiff("Arf1GFP_distances.tiff", height = 20, width = 35, units='cm', compression = "lzw", res = 300)
ggplot() + 
  stat_ecdf(data=Control, aes(x=Rel_dist, colour='Control'), size = 3.5) + 
  stat_ecdf(data=Khc1, aes(x=Rel_dist, colour='Kinesin RNAi'), size = 3.5) + 
  stat_ecdf(data=Dhc1, aes(x=Rel_dist, colour='Dynein RNAi'), size = 3.5) + 
  theme_bw() + theme(axis.line = element_line(colour = "black", size = 1),
                     axis.ticks.length=unit(.25, "cm"),
                     plot.margin = unit(c(1,1.5,1,1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks = element_line(size = 1),
                     axis.title.x = element_text(size =55, vjust = -6),
                     axis.title.y = element_text(size =55, face = "italic", vjust = 8),
                     axis.text.x = element_text(size = 50, colour = "black"),
                     axis.text.y = element_text(size = 50, colour="black"),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 50, margin = margin(t = 10)), # margin adjusts the distance between the legend texts
                     legend.key.width=unit(1,"cm"), # adjusts the line length of legend
                     legend.spacing.x = unit(0.3, 'cm'), # adjusts the distance between line and text of legend
                     legend.position=c(0.35,0.90)) + 
  ylab("ecdf") + xlab("Relative distance from cell centre") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,1, by = 0.2)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,1, by = 0.2))  +
  scale_color_manual(values=c("#606060", "#FF8000", "#0000FF"))
dev.off()

# remove legend keys

tiff("Arf1GFP_no_legend.tiff", height = 20, width = 35, units='cm', compression = "lzw", res = 300)
ggplot() + 
  stat_ecdf(data=Control, aes(x=Rel_dist, colour='Control'), size = 3) + 
  stat_ecdf(data=Khc1, aes(x=Rel_dist, colour='Kinesin RNAi'), size = 3) + 
  stat_ecdf(data=Dhc1, aes(x=Rel_dist, colour='Dynein RNAi'), size = 3) + 
  theme_bw() + theme(axis.line = element_line(colour = "black", size = 1),
                     axis.ticks.length=unit(.25, "cm"),
                     plot.margin = unit(c(1,1.5,1,1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks = element_line(size = 1),
                     axis.title.x = element_text(size =55, vjust = -6),
                     axis.title.y = element_text(size =55, face = "italic", vjust = 8),
                     axis.text.x = element_text(size = 50, colour = "black"),
                     axis.text.y = element_text(size = 50, colour="black"),
                     legend.position = "none") + 
  ylab("ecdf") + xlab("Relative distance from cell centre") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,1, by = 0.2)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,1, by = 0.2))  +
  scale_color_manual(values=c("#606060", "#FF8000", "#0000FF"))
dev.off()



################ Rab7DN Ecad vesicles distance from cell centre...
require(ggplot2)

dat <- data.frame(read.csv('rab7DN_results_sample.csv'))

noCD8 <- subset(dat, Genotype == "noCD8")
CD8 <- subset(dat, Genotype == "CD8")


tiff("Rab7DN_Ecad_vesicle.tiff", height = 20, width = 35, units='cm', compression = "lzw", res = 300)
ggplot() + 
  stat_ecdf(data=noCD8, aes(x=Rel_dist, colour='Control region'), size = 3.5) + 
  stat_ecdf(data=CD8, aes(x=Rel_dist, colour='Experimental region'), size = 3.5) + 
  theme_bw() + theme(axis.line = element_line(colour = "black", size = 1),
                     axis.ticks.length=unit(.25, "cm"),
                     plot.margin = unit(c(1,1.5,1,1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks = element_line(size = 1),
                     axis.title.x = element_text(size =55, vjust = -6),
                     axis.title.y = element_text(size =55, face = "italic", vjust = 8),
                     axis.text.x = element_text(size = 50, colour = "black"),
                     axis.text.y = element_text(size = 50, colour="black"),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 50, margin = margin(t = 10)), # margin adjusts the distance between the legend texts
                     legend.key.width=unit(1,"cm"), # adjusts the line length of legend
                     legend.spacing.x = unit(0.3, 'cm'), # adjusts the distance between line and text of legend
                     legend.position=c(0.35,0.90)) + 
  ylab("ecdf") + xlab("Relative distance from cell centre") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,1, by = 0.2)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,1, by = 0.2)) +
  scale_color_manual(values=c("black", "red"))
dev.off()

#######################

# Violin plots puncta per cell Arf1-GFP

require(ggplot2)

dat <- data.frame(read.csv('Arf1GFP_puncta_per_cell_collated.csv'))

dodge <- position_dodge(width = 0.1)

ggplot(data = dat, aes(x=Genotype, y=PPC)) + 
  geom_violin(position = dodge, width = 0.25, aes(fill = Genotype), lwd=0.7, alpha = 0.75) + 
  geom_boxplot(position = dodge, width=.1, outlier.colour=NA, lwd=0.7, fatten = 2) + 
  coord_cartesian(ylim = c(0, 16)) +
  scale_fill_manual(values=c("#606060", "#FF8000", "#0000FF")) +
  theme_bw() + theme(axis.line = element_line(colour = "black", size = 1),
                       axis.ticks.length=unit(.25, "cm"),
                       plot.margin = unit(c(1,1,1,1), "cm"),
                       panel.border = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.ticks = element_line(size = 1),
                       axis.title.x = element_text(size =55, vjust = -6),
                       axis.title.y = element_text(size =55, vjust = 8),
                       axis.text.x = element_text(size = 50, colour = "black"),
                       axis.text.y = element_text(size = 50, colour="black"),
                       legend.title = element_blank(),
                       legend.key.width=unit(1,"cm"), # adjusts the line length of legend
                       legend.spacing.x = unit(0.3, 'cm'), # adjusts the distance between line and text of legend
                       legend.position=c(0.35,0.95)) + 
  ylab("Arf1-GFP puncta per cell") + xlab("") 




######### ARF GFP inter-organelle DISTANCES
require(ggplot2)
dat <- data.frame(read.csv('clustering_Arf1GFP.csv'))

Control <- subset(dat, Genotype == "Control")
Khc1 <- subset(dat, Genotype == "Khc35770")
Dhc1 <- subset(dat, Genotype == "Dhc36583")
Dhc2 <- subset(dat, Genotype == "Dhc36698")

png("Arf1GFP_inter-organelle_distances_clustering.png", height = 20, width = 35, units='cm', res = 300)
ggplot() + 
  stat_ecdf(data=Control, aes(x=Norm_len, colour='Control'), size = 3) + 
  stat_ecdf(data=Khc1, aes(x=Norm_len, colour='Kinesin RNAi'), size = 3) + 
  stat_ecdf(data=Dhc1, aes(x=Norm_len, colour='Dynein RNAi'), size = 3) + 
  theme_bw() + theme(axis.line = element_line(colour = "black", size = 1),
                     axis.ticks.length=unit(.25, "cm"),
                     plot.margin = unit(c(1,1.5,1,1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks = element_line(size = 1),
                     axis.title.x = element_text(size =55, vjust = -6),
                     axis.title.y = element_text(size =55, face = "italic", vjust = 8),
                     axis.text.x = element_text(size = 50, colour = "black"),
                     axis.text.y = element_text(size = 50, colour="black"),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 40, margin = margin(t = 10)), # margin adjusts the distance between the legend texts
                     legend.key.width=unit(1,"cm"), # adjusts the line length of legend
                     legend.spacing.x = unit(0.3, 'cm'), # adjusts the distance between line and text of legend
                     legend.position=c(0.2,0.90)) + 
  ylab("ecdf") + xlab("Normalised inter-organelle distance") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,1, by = 0.2)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,1, by = 0.2))  +
  scale_color_manual(values=c("#606060", "#FF8000", "#0000FF"))
dev.off()


tiff("Arf1GFP_inter-organelle_distances_clustering_no_legend.tiff", height = 20, width = 35, units='cm', compression = "lzw", res = 300)
ggplot() + 
  stat_ecdf(data=Control, aes(x=Norm_len, colour='Control'), size = 3) + 
  stat_ecdf(data=Khc1, aes(x=Norm_len, colour='Kinesin RNAi'), size = 3) + 
  stat_ecdf(data=Dhc1, aes(x=Norm_len, colour='Dynein RNAi'), size = 3) + 
  theme_bw() + theme(axis.line = element_line(colour = "black", size = 1),
                     axis.ticks.length=unit(.25, "cm"),
                     plot.margin = unit(c(1,2,1,1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks = element_line(size = 1),
                     axis.title.x = element_text(size =55, vjust = -6),
                     axis.title.y = element_text(size =55, face = "italic", vjust = 8),
                     axis.text.x = element_text(size = 50, colour = "black"),
                     axis.text.y = element_text(size = 50, colour="black"),
                     legend.position = "none") + 
  ylab("ecdf") +  xlab("Normalised inter-organelle distance")  +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,1, by = 0.2)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,1, by = 0.2))  +
  scale_color_manual(values=c("#606060", "#FF8000", "#0000FF"))
dev.off()


# probability distribution plot_ Arf1GFP inter-organelle distance
png("Arf1GFP_inter-organelle_distances_density.png", height = 20, width = 35, units='cm', res = 300)
ggplot() + 
  geom_density(data=Control, aes(x=Norm_len, colour='Control'), size = 2) + 
  geom_density(data=Khc1, aes(x=Norm_len, colour='Kinesin RNAi'), size = 2) + 
  geom_density(data=Dhc1, aes(x=Norm_len, colour='Dynein RNAi'), size = 2) +
  theme_bw() + theme(axis.line.x = element_line(color="black", size = 2),
                     axis.line.y = element_line(color="black", size = 2),
                     axis.ticks.length=unit(.25, "cm"),
                     plot.margin = unit(c(2,2,1,1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.ticks = element_line(size = 1),
                     axis.title.x = element_text(size =55, vjust = -6),
                     axis.title.y = element_text(size =55, vjust = 8),
                     axis.text.x = element_text(size = 50, colour = "black"),
                     axis.text.y = element_text(size = 50, colour="black"),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 50, margin = margin(t = 10)), # margin adjusts the distance between the legend texts
                     legend.key.width=unit(1,"cm"), # adjusts the line length of legend
                     legend.spacing.x = unit(0.3, 'cm'), # adjusts the distance between line and text of legend
                     legend.position=c(0.8,0.90)) + 
  scale_color_manual(values=c("#606060", "#FF8000", "#0000FF")) +
  ylab("Density") +  xlab("Normalised inter-organelle distance")  +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,4, by = 0.5)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,4, by = 0.25)) + 
  coord_cartesian(clip = "off")
dev.off()
  
  

ggplot(dat, aes(x=Norm_len, colour=Genotype)) + geom_density(size =2) + 
  theme_bw()  + theme(axis.line = element_line(colour = "black", size = 3),
                      axis.ticks.length=unit(.25, "cm"),
                      plot.margin = unit(c(2,1.5,1,1), "cm"),
                      panel.border = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.background = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.ticks = element_line(size = 1),
                      axis.title.x = element_text(size =55, vjust = -6),
                      axis.title.y = element_text(size =55, face = "italic", vjust = 8),
                      axis.text.x = element_text(size = 50, colour = "black"),
                      axis.text.y = element_text(size = 50, colour="black"),
                      legend.title = element_blank(),
                      legend.text = element_text(size = 50, margin = margin(t = 10)), # margin adjusts the distance between the legend texts
                      legend.key.width=unit(1,"cm"), # adjusts the line length of legend
                      legend.spacing.x = unit(0.3, 'cm'), # adjusts the distance between line and text of legend
                      legend.position=c(0.8,0.90)) + 
  scale_color_manual(values=c("#606060", "#FF8000", "#0000FF")) +
  ylab("pdf")  + xlab("Normalised inter-organelle distance") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,4, by = 0.5)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,1, by = 0.2)) + 
  coord_cartesian(clip = "off")

