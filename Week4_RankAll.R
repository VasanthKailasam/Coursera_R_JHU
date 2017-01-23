##Function: This function calculates the rank for each hospital within a state, after cleaning and preparing the data. And
##based on the user specified Rank and disease, it returns the concerned hospital details. 

rankall <- function(disease, num = "best") {
  if(disease %in% c("heart attack", "heart failure", "pneumonia"))
  {
    if(disease == "heart attack") x <- 11
    if(disease == "heart failure") x <- 17
    if(disease == "pneumonia") x <- 23
  }
  else
  {
    print("invalid outcome")
    stop()
  }
  outcome <- read.csv("rprog%2Fdata%2FProgAssignment3-data/outcome-of-care-measures.csv", 
                      colClasses = "character")
  outcome[,x] <- as.numeric(outcome[,x])
  good <- complete.cases(outcome[,2], outcome[,7], outcome[,x])
  outcome <- outcome[good,]
  if(num == "best") num <- 1
  totstates <- as.factor(outcome[,7])
  final <- data.frame()
  for( i in levels(totstates))
  {
    out_del <- outcome %>%
      filter(outcome[,7] == i) 
    out_new <- arrange(out_del, out_del[,x], out_del[,2])
    if(num =="worst") num <- nrow(out_new)
    hospnam <- out_new[num,2]
    statenam <- i
    cha <- data.frame(hospnam, statenam)
    final <- rbind(final, cha)
  }
  colnames(final) <- c("hospital", "state")
  final  
}
