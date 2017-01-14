complete <- function(dir, id = 1:332){
  
  final <- data.frame()
  for (i in id)
  {
    link <- sprintf("%s/%03d.csv",dir,i)  
    dataset <- read.csv(link)
    good <- complete.cases(dataset)       #Finding complete cases for the datasets
    fg <- dataset[good,]
    newrow <- c(i, nrow(fg))
    final <- rbind(final, newrow)         #Appending the values of file name and number of complete observations
  }
  colnames(final) <- c("monitorID", "nobs")
  final
}
