
setwd("/Users/ioanna/Desktop/cell tracking/hkb stGFP movies/10082019_hkbstgfp/image3 block1/")


getwd()

####### 
rm(list=ls())  # clears previous variables
x = 'Results1.csv'    # enter name of main file
y = 'Tracks Statistics_PMECs.csv'   # enter name of subsetting file
z = 'PMEC.csv'.  # enter desired name of output file

tracks_coord <- function(a, b){
  a = data.frame(read.csv(x))
  str(a)
  b = data.frame(read.csv(y))
  str(b)
  b$TRACK_ID <- b$Track.ID
  filtered_data <- subset(a, TRACK_ID %in% b$TRACK_ID)
  str(filtered_data)
  write.csv(filtered_data, z)
}

tracks_coord(x,y)
