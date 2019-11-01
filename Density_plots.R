setwd("Desktop/")
library("ggplot2")

Dhc_angles <- as.data.frame(read.csv("Dhc_angles.csv"))

ggplot(Dhc_angles, aes(x=Angles, colour=Genotype)) + geom_density()

ggplot(Dhc_angles, aes(x=Angles, colour=Genotype)) + geom_density(size =1) + 
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


##############################

angles_noCD8 <- read.csv("angles_no_CD8.csv")

ggplot(angles_noCD8, aes(x=Angles, colour=Genotype)) + geom_density(size =1) + 
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



####### Good density plots

tiff("Arf1GFP_inter-organelle_distances_density.tiff", height = 20, width = 35, units='cm', compression = "lzw", res = 300)
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
  ylab("pdf") +  xlab("Normalised inter-organelle distance")  +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0,4, by = 0.5)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0,4, by = 0.2)) + 
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
                     breaks = seq(0,1, by = 0.2))