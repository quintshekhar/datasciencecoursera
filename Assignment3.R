                        # Programming Assignment 3

# 1. Plot the 30-day mortality rates for heart attack

outcome <- read.csv("./Assignment3/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])



# 2. Finding the best hospital in a state

best <- function(state, outcome) {
  
  ## Read outcome data
  
  outcome_care <- read.csv("./Assignment3/outcome-of-care-measures.csv",colClasses = "character")  

  ## Check that state and outcome are valid
    
  if(outcome== "pneumonia"){
    morality_col<-23
  }else if(outcome=="heart attack"){
    morality_col<-11
  }else if(outcome=="heart failure"){
    morality_col<-17
  }else{
    # print('Error in best("NY", "hert attack") : invalid outcome')
    stop('invalid outcome')
  }
  
  if(!any(unique(outcome_care$State)==state)){
    stop('Invalid state')
  }
  
  ## rate
  
  res2 <- outcome_care[outcome_care$State==state,c(2,morality_col)]
  res<-res2[res2[2]!="Not Available",]
  res<-res[order(res$Hospital.Name),]
  morality_col_data<-res[,2]
  min_rate<-min(as.numeric(morality_col_data))
  # min_rate
  
  ## Return hospital name in that state with lowest 30-day death
  hospital_name<-res[as.numeric(morality_col_data)==min_rate,1]
  hospital_name
  
}

# 3 Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  
  outcome_care <- read.csv("./Assignment3/outcome-of-care-measures.csv",colClasses = "character")  
  
  ## Check that state and outcome are valid
  
  if(outcome== "pneumonia"){
    morality_col<-23
  }else if(outcome=="heart attack"){
    morality_col<-11
  }else if(outcome=="heart failure"){
    morality_col<-17
  }else{
    # print('Error in best("NY", "hert attack") : invalid outcome')
    stop('invalid outcome')
  }
  
  if(!any(unique(outcome_care$State)==state)){
    stop('Invalid state')
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  res2 <- outcome_care[outcome_care$State==state,c(2,morality_col)]
  res<-res2[res2[2]!="Not Available",]
  names(res)<-c("hospital name","rate")
  res[,2]<-as.numeric(res[,2])
  res<-res[order(res[2],res[1]),]
  
  if(num=="best"){
    res[1,1]  
  }else if(num=="worst"){
    res[nrow(res),1]
  }else{
    res[num,1]
  }
  
  #res
}

# 4 Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  
  outcome_care <- read.csv("./Assignment3/outcome-of-care-measures.csv",colClasses = "character")  
  states<-unique(outcome_care$State);
  res_count<-1
  res_data_frame<-data.frame(hospital= character(0),state= character(0),stringsAsFactors=FALSE)
  
  ## Check that state and outcome are valid
  
  if(outcome== "pneumonia"){
    morality_col<-23
  }else if(outcome=="heart attack"){
    morality_col<-11
  }else if(outcome=="heart failure"){
    morality_col<-17
  }else{
    # print('Error in best("NY", "hert attack") : invalid outcome')
    stop('invalid outcome')
  }
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  for(state in states){
    res_data_frame[res_count,]<-c(rankhospital(state,outcome,num),state)
    res_count<-res_count+1
  }
  
  res_data_frame[order(res_data_frame[2]),]

}