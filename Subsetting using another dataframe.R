d.f <- data.frame(ViableNoEGTA)

# To extract a list from a dataframe
Gene <- d.f[ apply( d.f , 1 , function(x) any(unlist(x) %in% MembNotch_reduced$GeneSymbol) ) , ]

# to see whats left in a dataframe after extracting a list
WhatsLeft <- df[ ! df$GeneSymbol %in% AffectNotchTrafficking$GeneSymbol, ]

(cl <- kmeans(control$QC1__NoOfAnalysedCells, 2, nstart = 25))
plot(control$QC1__NoOfAnalysedCells, col = cl$cluster, pch=20)
points(cl$centers, col = 1:5, pch = 8)

control$GeneSymbol[which(cl$cluster==1)]
control$GeneSymbol[which(cl$cluster==2)]
control$GeneSymbol[which(cl$cluster==3)]

cl <- kmeans(na.omit(control[14:47]), 3, nstart = 25)


# USE THIS......
filtered_data <- subset(main_data, TRACK_ID %in% filter_data$TRACK_ID)



tracks_calc <- function (a, b){
  str(a)
  str(b)
  b$TRACK_ID <- b$Track.ID
  str(b)
  filtered_data <- subset(a, TRACK_ID %in% b$TRACK_ID)
  str(filtered_data)
  write.csv(filtered_data, 'filtered_data.csv')
}

tracks_calc(main_data, filter_data )

a = 'Results_copy.csv'

b = 'Tracks_Statistics_PMECs.csv'