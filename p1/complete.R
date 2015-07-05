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
        ids <- c()
        num <- c()
	  for (i in id) {
		tab <- read.csv(file.path(directory,dir(directory)[i]), header = T, sep = ",")
            good <- complete.cases(tab)
            tab = tab[good,]
		ids <- c(ids, i)
            num <- c(num, length(tab[["sulfate"]]))           
	  }
	  data.frame(id = ids, nobs = num)
}