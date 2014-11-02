rankhospital <- function(state, outcome, num = "best") {
      
      
      ## Read outcome data
      mydata <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
      
      ## Check that state and outcome are valid
      states <- read.csv("states.csv", head = FALSE)
      if(any(states[,1] == state) == FALSE){ 
            stop("invalid state")
      }
      outcomes <- c("heart attack", "heart failure", "pneumonia")
      if(!(tolower(outcome) %in% outcomes)){
            stop("invalid outcome")
      }
      
      
      ## Return hospital name in that state with the given rank 30-day death rate
      mydatasub <- mydata[which(mydata[,7] == state),]
      outcome2 <- sub(" ", ".", outcome)
      searchterm <- paste("hospital.30.day.death..mortality..rates.from.", outcome2, sep = "")
      mortalityCol <- which(tolower(names(mydatasub)) == searchterm)
      mydatasub[,mortalityCol] <- as.double(mydatasub[,mortalityCol])
      sorted <- sort(mydatasub[,mortalityCol])
      if(num == 'best'){
            num <- 1
      }
      if(num == 'worst'){
            num <- sum(!is.na(mydatasub[,mortalityCol]))
      }
      x <- which(sorted == sorted[num])
      mortrate <- sorted[num]
      
      tie <- which(mortrate == mydatasub[,mortalityCol])
      
      tied <- mydatasub[tie,]
      tied <- sort(tied$Hospital.Name)
      
      tied[num - x[1] + 1]
      #mydatasub[which(mydatasub[,mortalityCol] == mortrate)[1], 2]
      
}