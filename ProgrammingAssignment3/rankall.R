rankall <- function(outcome, num = "best") {
      ## Read outcome data
      mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      states <- factor(sort(unique(mydata$State)))
      states2 <- sort(unique(mydata$State))
      
      ## Check that state and outcome are valid

      outcomes <- c("heart attack", "heart failure", "pneumonia")
      if(!(tolower(outcome) %in% outcomes)){
            stop("invalid outcome")
      }
      
      ## For each state, find the hospital of the given rank
#      if(num == 'best'){
 #           num <- 1
  #    }

      
      
      outcome2 <- sub(" ", ".", outcome)
      searchterm <- paste("hospital.30.day.death..mortality..rates.from.", outcome2, sep = "")
      mortalityCol <- which(tolower(names(mydata)) == searchterm)
      
      mydatasub <- data.frame(cbind('Hospital.Name' = mydata$Hospital.Name, 'State' = mydata$State, 'Mortality' = as.numeric(mydata[,mortalityCol])))
      mydatasub <- mydatasub[!is.na(mydatasub[,3]),]
      
      mydatasub <- mydatasub[order(mydatasub[,1]),]
      mydatasub <- mydatasub[order(mydatasub[,3], decreasing = FALSE),]
      
      result <- data.frame()
      for(i in 1:length(states)){
            sub <- mydatasub[which(mydatasub$State==states2[i]), ]
            sub[,3] <- as.numeric(levels(sub[, 3]))[sub[, 3]]
            sub <- sub[order(sub[,3]),]
            values <- as.numeric(levels(sub[, 3]))[sub[, 3]]
            if(num == "best"){
                  rowNum <- which.min(values)
            } else if(num == "worst"){
                  rowNum <- which.max(values)
            } else{
                  rowNum <- num
            }
            result <- rbind(result, data.frame('hospital' = sub[rowNum, ]$Hospital.Name, 'state' = states2[i]))
      }     

      result
      
}
