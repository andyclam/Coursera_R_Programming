best <- function(state, outcome) {
      ## Read outcome data
      mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      states <- read.csv("states.csv", head = FALSE)
      
      ## Check that state and outcome are valid
      if(any(states[,1] == state) == FALSE){ 
            stop("invalid state")
      }
      outcomes <- c("heart attack", "heart failure", "pneumonia")
      if(!(tolower(outcome) %in% outcomes)){
            stop("invalid outcome")
      }
      
      ## Return hospital name in that state with lowest 30-day death rate
      mydatasub <- mydata[which(mydata[,7] == state),]
      outcome2 <- sub(" ", ".", outcome)
      searchterm <- paste("hospital.30.day.death..mortality..rates.from.", outcome2, sep = "")
      mortalityCol <- which(tolower(names(mydatasub)) == searchterm)
      mydatasub[,mortalityCol] <- as.double(mydatasub[,mortalityCol])
      minMort <- min(mydatasub[,mortalityCol], na.rm = TRUE)
      tie <- which(mydatasub[,mortalityCol] == minMort)
      breaktie <- sort(tie)
      mydatasub[breaktie[1], 2]
}