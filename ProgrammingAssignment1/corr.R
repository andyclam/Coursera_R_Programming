corr <- function(directory, threshold = 0) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      
      ## 'threshold' is a numeric vector of length 1 indicating the
      ## number of completely observed observations (on all
      ## variables) required to compute the correlation between
      ## nitrate and sulfate; the default is 0
      
      ## Return a numeric vector of correlations
      
      result <- vector("numeric")
      tempresult <- vector("numeric")
      list <- list.files(directory, full.names = TRUE)
      datalist <- data.frame()
      
      for (i in 1:length(list)){
            # append all data to datalist dataframe
            datalist <- read.csv(list[i])
            complete <- complete.cases(datalist)
            nobs <- sum(complete)
            if(nobs > threshold){
                  #calculate correclation between sulfate and nitrate for cases that are complete
                  tempresult <- cor(datalist$sulfate, datalist$nitrate, use = "pairwise.complete.obs") 
                  result <- c(result, tempresult)
            }
      }
      result
}
