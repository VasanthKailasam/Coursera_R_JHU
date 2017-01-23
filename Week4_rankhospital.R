#Calculates rank of each hospital and returns the hospital name based on the rank

rankhospital <- function(state, disease, num = "best") {
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
  if(state %in% outcome[,7]){}
  else {
    print("invalid state")
    stop()
  }
  if(num == "best") num <- 1
  out_v1 <- outcome %>%
    filter(outcome[,7] == state) 
  out_v2 <- arrange(out_v1,out_v1[,x],out_v1[,2])
  if(num =="worst") num <- nrow(out_v2)
  hospname <- out_v2[num,2]
  hospname
}
