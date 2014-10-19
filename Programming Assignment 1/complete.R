complete <- function(directory, id = 1:332) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      
      ## 'id' is an integer vector indicating the monitor ID numbers
      ## to be used
      
      ## Return a data frame of the form:
      ## id nobs
      ## 1  117
      ## 2  1041
      ## ...
      ## where 'id' is the monitor ID number and 'nobs' is the
      ## number of complete cases
      list <- list.files(directory, full.names = TRUE)
      datalist <- data.frame()
      result <- data.frame(id = integer(), nobs = integer())
      for (i in id){
            #read in data based on list[]
            datalist <- rbind(datalist, read.csv(list[i]))
            #
            datalistsubset <- datalist[which(datalist[,"ID"] %in% i),]
            #counts all complete cases in subset
            nobscount <- sum(complete.cases(datalistsubset))
            #adds on data to result dataframe
            result <- rbind(result, c(i, nobscount))
      }
      #assigns column names to result dataframe
      colnames(result) = c("ID", "nobs")
      result
}