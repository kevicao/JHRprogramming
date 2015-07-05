rankhospital <- function(state, outcome, num = "best") {
	## read outcome data
	raw <- read.csv('outcome-of-care-measures.csv',colClasses = 'character')

	#check input
	stateList = unique(raw$State)
      if (!(state %in% stateList)) stop("invalid state") 
	if (!(outcome %in% c('heart attack','heart failure','pneumonia'))) stop("invalid outcome") 
      
	#return hospital name
	if (outcome == "heart attack") newdata <- raw[raw$State == state & !is.na(raw[,11]), c(11,2)]
	if (outcome == "heart failure") newdata <- raw[raw$State == state & !is.na(raw[,17]), c(17,2)]
	if (outcome == "pneumonia") newdata <- raw[raw$State == state  & !is.na(raw[,23]), c(23,2)]
      newdata[,1] = suppressWarnings(as.numeric(newdata[,1]))
      newdata = newdata[!is.na(newdata[,1]),]
      ii = order(newdata[,1],newdata[,2])
      if (num == "best") index = 1
      else if (num == "worst") index = length(ii)
      else index = num

      newdata[ii[index],2]
}