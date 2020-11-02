rm(list = ls())
dat <- data.frame(read.csv('collated_data.csv'))
str(dat)

# dat_new <- subset(dat, mov_label == "mov-1" |
#                     mov_label == "mov-2" |
#                     mov_label == "mov-3" |
#                     mov_label == "mov-4" |
#                     mov_label == "mov-5")

library(ggplot2)
library(dplyr)
library(plyr)
library(ggpmisc)
library(lme4)
library(lattice)


ggplot(dat) + 
  aes(x=dat$mov_label, y=dat$cross_corr) + 
  geom_boxplot() + geom_jitter(width = 0.2) + 
  xlab("") +
  ylab("Cross_corr_coef") + 
  theme_gray(base_size = 14) + 
  theme_bw() + theme_light() +
  theme(
    axis.title.y = element_text(size =20, vjust = 2),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )


ggplot(dat) + 
  aes(x=dat$mov_label, y=dat$px_ratio) + 
  geom_boxplot() + geom_jitter(width = 0.2) + 
  xlab("") +
  ylab("Relative_px_diff") + 
  theme_gray(base_size = 14) + 
  theme_bw() + theme_light() +
  theme(
    axis.title.y = element_text(size =20, vjust = 2),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )


#########################################

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(size=2) +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


ggplotRegression(lm(cross_corr ~ px_ratio, data = dat)) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title.x=element_text(size=20), 
        axis.title.y=element_text(size=20, vjust = 2),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18)) + 
        xlab("Relative_px_diff") +
        ylab("Cross_corr") +
  stat_poly_eq(parse=T, aes(label = ..eq.label..), formula=y~x)


fit <- lm(cross_corr ~ px_ratio, dat)
summary(fit)

confint(fit, 'px_ratio', level=0.95)




#################### Regression per group

# with trend line --- lm
ggplot(dat_new) + 
  aes(x=dat_new$px_ratio, y=dat_new$cross_corr, 
      col=mov_label) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", se = FALSE) + 
  xlab("px_ratio") +
  ylab("cross_corr") +
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x=element_text(size=20), 
        axis.title.y=element_text(size=20, vjust = 2),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18)
    )

###

ggplot(dat) + 
  aes(x=dat$px_ratio, y=dat$cross_corr, 
      col=as.factor(mov_label)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", se = FALSE) + 
  xlab("px_ratio") +
  ylab("cross_corr") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

















plot(robustness_data$von_mises_SD, robustness_data$cell_area)
plot(robustness_data$eccentricity, robustness_data$von_mises_SD)

#######################

ggplot() + 
  geom_point(data=na.omit(robustness_data),
             aes(x=eccentricity, y=von_mises_SD, colour =as.factor(direction)), size =2) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


# with trend line --- lm
ggplot(na.omit(robustness_data)) + 
  aes(x=robustness_data$eccentricity, y=robustness_data$von_mises_SD, 
      col=as.factor(robustness_data$direction)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", se = FALSE) + 
  xlab("Eccentricity") +
  ylab("SD") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())



#####################

ggplot(robustness_data) + 
  aes(x=robustness_data$cell_area, y=robustness_data$von_mises_SD, 
      col=as.factor(Genotype)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", se = FALSE) + 
  xlab("Cell Area") +
  ylab("SD") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

########################################

ggplot(robustness_data) + 
  aes(x=robustness_data$von_mises_SD, y=robustness_data$transport_deviation, 
      col=as.factor(Genotype)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", se = FALSE) + 
  xlab("SD") +
  ylab("Dev") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

########################################

robustness_data$transport_deviation = abs(robustness_data$cell_direction - robustness_data$transport_direction)

ddply(robustness_data,~Genotype,summarise,mean=mean(transport_deviation),sd=sd(transport_deviation))

robustness_data$transport_alignment = robustness_data$eccentricity *
  robustness_data$von_mises_SD * 
  robustness_data$transport_deviation

ddply(robustness_data,~Genotype,summarise,mean=mean(transport_alignment),sd=sd(transport_alignment), 
      N = length(transport_alignment))

write.csv(robustness_data, "wt_spas_robustness.csv")



