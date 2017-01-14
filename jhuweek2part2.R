complete <- function(dir, id = 1:332){
  
  final <- data.frame()
  for (i in id)
  {
    link <- sprintf("%s/%03d.csv",dir,i)
    dataset <- read.csv(link)
    good <- complete.cases(dataset)
    fg <- dataset[good,]
    newrow <- c(i, nrow(fg))
    final <- rbind(final, newrow)
  }
  colnames(final) <- c("monitorID", "nobs")
  final
}