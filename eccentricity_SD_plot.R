setwd("C:/Users/spind/OneDrive - sheffield.ac.uk/1_Live_analysis/white/") # on Home PC

setwd("/Users/victoralfred/Desktop/Live_analysis/white")


library(ggplot2)
library(dplyr)
library(plyr)
library(ggpmisc)
library(lme4)
library(lattice)
robustness_data <- as.data.frame(read.csv("wt_spas_robustness.csv"))
str(robustness_data)


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

####################

# with trend line --- lm
ggplot(robustness_data) + 
  aes(x=robustness_data$eccentricity, y=robustness_data$von_mises_SD, 
      col=as.factor(Genotype)) +
  geom_point(size = 2) +
  stat_smooth(method = "lm", se = FALSE) + 
  xlab("Eccentricity") +
  ylab("SD") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  stat_poly_eq(parse=T, aes(label = ..eq.label..), formula=y~x)

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


ggplot(robustness_data) + 
  aes(x=robustness_data$Genotype, y=robustness_data$transport_deviation) + 
  geom_boxplot() + geom_jitter(width = 0.2)

##########################

ggplot(robustness_data) + 
  aes(x=robustness_data$eccentricity, y=robustness_data$transport_alignment, 
      col=as.factor(Genotype)) +
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


ggplotRegression(lm(von_mises_SD ~ eccentricity, data = robustness_data)) + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none", 
        axis.title=element_text(size=15), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15)) + 
  stat_poly_eq(parse=T, aes(label = ..eq.label..), formula=y~x)


fit <- lm(von_mises_SD ~ eccentricity, robustness_data)
summary(fit)

confint(fit, 'eccentricity', level=0.95)


