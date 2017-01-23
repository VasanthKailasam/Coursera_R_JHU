##Dataset: 
##The data for this assignment come from the Hospital Compare web site (http://hospitalcompare.hhs.gov)
##run by the U.S. Department of Health and Human Services. The purpose of the web site is to provide data and
##information about the quality of care at over 4,000 Medicare-certified hospitals in the U.S. This dataset essentially
##covers all major U.S. hospitals. This dataset is used for a variety of purposes, including determining
##whether hospitals should be fined for not providing high quality care to patients

##Function:
## This function finds the best hospital for a given state and a given disease (heart attack, heart failure, pneumonia)

best <- function(state, disease)
{
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
  good <- complete.cases(outcome)
  outcome <- outcome[good,]
  if(state %in% outcome[,7]){}
  else {
    print("invalid state")
    stop()
  }
  out_v1 <- outcome %>%
    filter(outcome[,7] == state)
  out_v2 <- out_v1 %>%
    filter(out_v1[,x] == min(out_v1[,x]))
  vecthosp <- sort(out_v2[,2])
  vecthosp[1]
  
}
