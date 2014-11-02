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
      if(num == 'best'){
            num <- 1
      }

      
      outcome2 <- sub(" ", ".", outcome)
      searchterm <- paste("hospital.30.day.death..mortality..rates.from.", outcome2, sep = "")
      mortalityCol <- which(tolower(names(mydata)) == searchterm)
      
      mydatasub <- as.data.frame(cbind('Hospital.Name' = mydata$Hospital.Name, 'State' = mydata$State, 'Mortality' = as.double(mydata[,mortalityCol])))
      mydatasub <- mydatasub[!is.na(mydatasub[,3]),]
      mydatasub <- mydatasub[order(mydatasub[1]),]
      
      mydatasplit <- split(mydatasub, mydatasub$State)
      sorted <- lapply(mydatasplit, 
                       function(x){
                             x[order(x[1]),]
                       }
      )

      ordered <- lapply(sorted, 
                        function(x){
                              order(x$Mortality)
                        }
      )
      
      
      ranked <- data.frame()
      num2 <- num
      for(i in 1:length(states)){
            if(num2 == 'worst'){
                  num <- max(ordered[[i]])
            }
            if(length(which(ordered[[i]] == num)) > 0){
                  ranked <- rbind(ranked, sorted[[i]][which(ordered[[i]] == num),])
            }else{
                  ranked <- rbind(ranked, cbind('Hospital.Name' = '<NA>', 'State' = states2[i], 'Mortality' = 0))     
            }
            
      }
      
      #ranked <- as.data.frame(ranked)
      
      
      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name
#      result <- data.frame()
 #     for(i in 1:length(states2)){
  #          result <- data.frame(rbind(result, cbind('hospital' = ranked[[1]][which(states2[i] == ranked[2])], 'state' = states2[i])))
#
 #     }
      
      result <- cbind('hospital' = ranked[1], 'state' = ranked[[2]])
      names(result) <- c('hospital', 'state')
      rownames(result) <- states2
      result
      
}