library(ggplot2)

# Initial plot
d <- ggplot(dat, aes(x=ANGLE_CLASS, y=DISP_NORM, fill=DIRECTION)) + 
  geom_bar(stat="identity", position="identity") +
  scale_y_continuous(
    limits=c(-380000, 90000), # lowered the min limit slightly
    breaks=c(90000,0,-90000,-180000,-270000,-350000),
    labels=abs ,  # note the use of abs
    expand=c(0,0)) + # use expand so axis start exactly at limits
  scale_x_continuous(
    limits=c(16,30),  # added x-axis limits (min is < your min break)
    breaks=seq(18,30,by=2), 
    labels=seq(18,30,by=2) , 
    expand=c(0,0)) + 
  xlab("small RNA length [nt]") + 
  ylab("normalized small RNA counts") + 
  scale_fill_manual(values = c("red", "blue"))

d <- d + theme_bw() + 
  theme(axis.line = element_blank(), # remove both axis lines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border=element_blank())

# Add in segments for the axis - allow a gap at the corner
d + 
  geom_segment(x=17,xend=30,y=-380000,yend=-380000) + # x-axis
  geom_segment(x=16,xend=16,y=-350000,yend=90000) # y-axis