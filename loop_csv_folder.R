
# gsub("frame_|.txt", "", "frame_10_obj")
# 
# # parse_number from library(readr)
# readr::parse_number("frame_10_obj")
# 
# gsub("^(?:[^_]+_){1}([^_]+).*", "\\1", "frame_10_obj")
# 
# 
rm(list = ls())

library(stringr)
library(readr)
files<-list.files()
big.df <- vector('list',length(files))

for (i in 1:length(files)){
  tmp<-read.table(files[i],header=FALSE,sep=" ")
  tmp1 <- tmp[1]
  tmp1$frame<-parse_number(files[i])
  names(tmp1) <- c("values","frame")
  tmp1[tmp1 == "N/A"]  <- NA
  tmp2<-na.omit(tmp1)
  big.df[[i]] <- tmp2
}


final.df <- do.call('rbind', big.df)

means_df <- aggregate(final.df[, 1], list(final.df$frame), mean)
write.csv(means_df, "1_means_combined.csv")


# from https://stackoverflow.com/questions/46610277/loop-through-csv-files-issue-completing-task-for-each-individual-file
# ## Run a for loop to complete the same tasks for each
# for (i in 1:length(files)){
#   ## Read table
#   tmp<-read.table(files[i],header=FALSE,sep=" ")
#   ## Keep certain columns
#   tmp1 <- tmp[c(2:5,9,10,12,13)]
#   #Name the remaining columns
#   names(tmp1) <- 
#     c("GMT_Date","GMT_Time","LMT_Date","LMT_Time","Latitude","Longitude","PDOP","2D_3D")
#   #Add column for collar ID
#   tmp1$AnimalID<-gsub("^(?:[^_]+_){1}([^_]+).*", "\\1", files[i])
#   #Cleanup dataframe by removing records with NAs
#   tmp1[tmp1 == "N/A"]  <- NA
#   tmp2<-na.omit(tmp1)
#   big.df[[i]] <- tmp2
#   
# }
# final.df <- do.call('rbind', big.df)


# Also check this
# https://stackoverflow.com/questions/40063507/how-to-loop-through-a-folder-of-csv-files-in-r


