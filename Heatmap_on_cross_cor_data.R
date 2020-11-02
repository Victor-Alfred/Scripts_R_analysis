
# dat <- data.frame(read.csv('collated_data.csv'))
rm(list = ls())
dat <- data.frame(read.csv('collated_data.csv'))
str(dat)

# script to remove outliers

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

set.seed(1)
dat_new <- remove_outliers(dat$cross_corr)
## png()
par(mfrow = c(1, 2))
boxplot(x)
boxplot(y)


# Working nicely for now

library(ggplot2)
library(RColorBrewer)
ggplot(data = dat, mapping = aes(x=mov_label, y=as.factor(frame_interval))) + 
  geom_tile(aes(fill = cross_corr), colour = "white") +
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust =1)) +
  scale_fill_gradient(
    name = "cc_coeff", # changes legend title
    low = "blue",
    high = "orange",
    limit = c(min(dat$cross_corr), 1),
    space = "Lab",
    guide = "colourbar"
  ) + scale_y_discrete(limits = rev(levels(dat$frame_interval)))

# aes(x = reorder(the_factor, desc(the_factor)), ...)
# Perhaps nonspecific drift might have affected the displacement....
# correct for drift with rigid body transformation


# plotting subtraction of frames

library(ggplot2)
library(RColorBrewer)
ggplot(data = dat, mapping = aes(x=mov_label, y=as.factor(frame_interval))) + 
  geom_tile(aes(fill = px_ratio), colour = "white") +
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust =1)) +
  scale_fill_gradient(
    name = "rel_px_diff", # changes legend title
    low = "blue",
    high = "orange",
    limit = c(min(dat$px_ratio), 1),
    space = "Lab",
    guide = "colourbar"
  ) + scale_y_discrete(limits = rev(levels(dat$frame_interval)))





################################################################

# dat <- data.frame(read.csv('collated_data.csv')) for frame subtraction
rm(list = ls())
dat <- data.frame(read.csv('collated_data.csv'))
str(dat)

# Working nicely for now
b <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)

library(ggplot2)
library(RColorBrewer)
ggplot(data = dat, mapping = aes(x=mov_label, y=as.factor(frame_interval))) + 
  geom_tile(aes(fill = px_ratio), colour = "white") +
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust =1)) +
  scale_fill_gradientn(limits = c(0,0.5),
                       colours=c("navyblue", "darkmagenta", "darkorange1"),
                       breaks=b, labels=format(b)) + 
  scale_y_discrete(limits = rev(levels(dat$frame_interval)))

# aes(x = reorder(the_factor, desc(the_factor)), ...)
# Perhaps nonspecific drift might have affected the displacement....
# correct for drift with rigid body transformation



################################################################




p <- ggplot(dat, aes(x=mov_label, y=cross_corr)) + 
  geom_boxplot()







# Alternative scripts... still in dev

library(ggplot2)
library(RColorBrewer)
ggplot(data = dat, aes(x=mov_label, y=frame_interval, fill=cross_corr)) + 
  geom_tile() + 
  theme(axis.title.y=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks.y=element_blank()) +
  scale_fill_gradient(
    name = "Guide", # changes legend title
    low = "blue",
    high = "orange",
    limit = c(min(dat$cross_corr), 1),
    space = "Lab",
    guide = "colourbar"
  )

dat$Y1 <- cut(dat$cross_corr,breaks = c(1, 0.9, 0.8, 0.7, 0.6, 0.5),right = FALSE)


library(ggplot2)
library(RColorBrewer)
ggplot(data = dat, aes(x=mov_label, y=frame_interval)) + 
  geom_tile(aes(fill = Y1), colour = "white") +
    scale_fill_brewer(palette = "PiYG") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
  
  
# Diverging
# BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
# 
# Qualitative
# Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
# 
# Sequential
# Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, 
# OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, 
# YlGnBu, YlOrBr, YlOrRd



######## STATS ############

library(FSA)
library(PMCMR)

attach(dat)
kruskal.test(cross_corr ~ mov_label, data = dat)

dunnTest(cross_corr ~ mov_label, data = dat, method = "bh")

posthoc.kruskal.dunn.test(x = cross_corr, g = mov_label, p.adjust.method = "bonferroni")


