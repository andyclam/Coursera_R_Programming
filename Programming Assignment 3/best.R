best <- function(state, outcome) {
      ## Read outcome data
      mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Check that state and outcome are valid
      if(nchar(state) != 2){
            stop("invalid state")     
      }
      if(pmatch(outcome, names(mydata)) == FALSE){
            stop("invalid outcome")
      }
      
      ## Return hospital name in that state with lowest 30-day death rate
      
}