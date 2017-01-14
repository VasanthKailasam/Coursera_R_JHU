pollutantmean <- function(dir, pollutant, id=1:332){
  
 m <- 0  
 l <- 0
 for (i in id)
  {
    link <- sprintf("%s/%03d.csv",dir,i)    #dynamically changing the directory and file address to fetch different files
    dataset <- read.csv(link)
    temp <- sum(dataset[[pollutant]], na.rm=TRUE)
    if(is.na(temp))
      {
        print(i)
        temp <- 0
      }
    m <- m+ temp
    l <- l + length(which(!is.na(dataset[pollutant])))     #calculating the length of complete observations for 2 columns. 
  }
  total <- m/l
  total
}
  
