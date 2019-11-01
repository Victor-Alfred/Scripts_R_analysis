# an R function to calculate relative values and standard deviation
# compared to a control

eye_area <- read.csv("eye_area.csv")
wing_area <- read.csv("wing_area.csv")

str(c(eye_area, wing_area))

library(dplyr)

rel_mean_sd <- function(x){
    require(dplyr) # call the dplyr library to use the filter,select functions

    control_mean <- filter(x, Genotype == "yw") %>% select(mean_area) %>% unlist
    all_means <- select(x, mean_area) %>% unlist
    rel_mean <- ((all_means-control_mean)/control_mean) * 100
    all_sd <- select(x, sd_area) %>% unlist
    rel_sd <- (all_sd/all_means) * 100
    out <- data.frame(rel_mean, rel_sd)
    colnames(out) <- c("relative mean", "relative_sd")
    rownames(out) <- x[, 1]
    
    return(out)
}

# use the rel_mean_sd function to the dataframes, and save the output to csv

rel_eye_area <- rel_mean_sd(eye_area)
write.csv(rel_eye_area, "rel_eye_area.csv")

rel_wing_area <- rel_mean_sd(wing_area)
write.csv(rel_wing_area, "rel_wing_area.csv")


