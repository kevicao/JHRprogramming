rankall <- function(outcome, num = "best") {
	## read outcome data
	raw <- read.csv('outcome-of-care-measures.csv',colClasses = 'character')

	#check input
	stateList = unique(raw$State)
	if (!(outcome %in% c('heart attack','heart failure','pneumonia'))) stop("invalid outcome") 
      
	#return hospital name
	if (outcome == "heart attack") newdata <- raw[!is.na(raw[,11]), c(7,11,2)]
	if (outcome == "heart failure") newdata <- raw[!is.na(raw[,17]), c(7,17,2)]
	if (outcome == "pneumonia") newdata <- raw[!is.na(raw[,23]), c(7,23,2)]
      newdata[,2] = suppressWarnings(as.numeric(newdata[,2]))
      newdata = newdata[!is.na(newdata[,2]),]
       
      hos = c()
      states = c()
      for (i in stateList) {
          final = newdata[newdata[,1] == i, ]
          ii = order(final[,1],final[,2],final[,3])

          if (num == "best") index = 1
          else if (num == "worst") index = length(ii)
          else index = num

          hos = append(hos, final[ii[index],3])
          states = append(states, i)
      }
   
      data.frame(hospital = hos, state = states)

}