corr <- function(dir, threshold = 0){
  
  cvalues <- c()
  for (i in 1:332)
  {
    link <- sprintf("%s/%03d.csv",dir,i)
    dataset <- read.csv(link)
    cob <- complete(dir, i)
    if(cob[2] > threshold)
    {
    oat <- cor(dataset$sulfate, dataset$nitrate, use ="complete.obs")
    cvalues <- c(cvalues, oat)
    }
    
  }
  cvalues
  
}
