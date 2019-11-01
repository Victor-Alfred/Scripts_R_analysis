
#   https://stackoverflow.com/questions/30242065/trying-to-merge-multiple-csv-files-in-r

multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=TRUE)})
  Reduce(function(x,y) {merge(x,y)}, datalist)
}

dat <- multmerge(.....path)
dat <- multmerge("~/Desktop/REPEAT analysis/khc35770 -- 1/analysis/1/rel_distances")


library(dplyr)
library(readr)
dat <- list.files(full.names = TRUE) %>% lapply(read_csv) %>% bind_rows 



# Working script to merge csv files in folder path
setwd(....)

rm(list=ls())
file_list <- list.files()
require(data.table)
dataset = rbindlist(lapply( file_list, fread ))
write.csv(dataset, "all_combined.csv")


# clean distance data
rm(list=ls())

dat <- data.frame(read.csv('all_distances.csv'))
dat_new <- unique(dat)
write.csv(dat_new, 'distances_unique.csv')

rm(list = ls())
dat <- data.frame(read.csv('Image5_rel_distances.csv'))
dat_new <- unique(dat)
write.csv(dat_new, 'distances_unique.csv')






