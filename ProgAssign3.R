## DataScience Coursera  : R Programming : Programming Assignment 3 


## 1. MoralityRateforHeartAttacks  : Plot 30 Day Morality Rate for Heart Attacks. 

MoralityRateforHeartAttacks <- function()
{
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome[, 11] <- as.numeric(outcome[, 11])
  
  hist(outcome[, 11])
}

## 2. BestHospitalbyOutcomeandState : Find the best hospital in State. 

BestHospitalbyOutcomeandState <- function(state,disease)
{
  ## Read outcome data , extract states and outcomes. 
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  states <-  unique(outcome[,7]) 
  if (!is.element(state,states)) stop("Invalid State")
  
  outcomes<-names(outcome)
  outcomes<-outcomes[grepl("Lower.*Mortality.*",outcomes)]
 
  i=1
  for (s in outcomes) {
    s<-unlist(strsplit(s,"from"))
    s<-unlist(strsplit(s[2],"\\."))
    s<-paste(s[s!=""],collapse  = " ")
    outcomes[i]<-s
    i<-i+1
  }  
  #print(outcomes)
  if(!is.element(disease,outcomes))
    {
       print ("Please using the one of the following :")
       print (outcomes)
       stop("Invalid Outcome")
  }
  
  #Build regex for lower mortality column 
  
  disease<- sub(" ", ".", disease, fixed = TRUE)
  str<-paste("Lower.*Mortality.*",disease,sep="")
  col<- names(outcome)[grepl(str,names(outcome))]
  outcome<-outcome[outcome$State==state,]
  row<- which(outcome[,col]==min(as.numeric(outcome[,col]),na.rm = TRUE))[1]
  #print(row)
  outcome[row,"Hospital.Name"] 
}
