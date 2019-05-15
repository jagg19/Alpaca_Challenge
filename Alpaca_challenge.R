#Solving the Alpaca challenge in two different ways. First, we get the indexes and in the Second way we get the actual dates.


S<-AlpacaforR::get_bars("SPY",from = "2018-01-01",to = "2018-12-31",timeframe = "day")
D<-AlpacaforR::get_bars("SPY",from = "2018-01-01",to = "2018-12-31",timeframe = "day")




#Create is.prime function
is.prime <- function(num){
  #2 is prime
  if (num == 2){TRUE} 
  #if num is divisble by any number between itself and 1, then its not prime.
  else if(any(num %% 2:(num-1) == 0)){FALSE} 
  else{TRUE}
}




################################
#Using a list to get the indexes
################################


#for each day in our dataframes
for(i in 1:nrow(S[[1]])){
  #are the results of both the SPY and DIA dataframe prime?
  if(is.prime(as.integer(S[[1]][i,]$c * 10**2)) == TRUE & is.prime(as.integer(D[[1]][i,]$c * 10**2)) == TRUE){
    #if yes, is the date on a thursday? else next iteration
    if(weekdays(as.Date(S[[1]][i,]$d)) == "Thursday"){
      answer <- i
      print(answer)
      next
    }
  }
}





###############################
#Using a dataframe to get dates 
###############################

#Creating dataframes
S <- S[[1]]
D <- D[[1]]


#Get rows for thursday trading days
thursdays <- which(weekdays(S$d) == "Thursday")


#Filter SPY & DIA dataframe for only thursday trading days
S <- S %>% filter(row_number() %in% thursdays)
D <- D %>% filter(row_number() %in% thursdays)


answers <- NA
for(i in 1:nrow(S)){
  #If the result for SPY is prime, then check is the result for DIA is also prime.
  if(is.prime(as.integer(S$c[i] * (10^2)))){
    #If it is also prime, then return the date or return nothing.
    if(is.prime(as.integer(D$c[i] * (10^2)))){
      answers <- c(answers,D$d[i])
    }
  }
  if(i==nrow(S)){
    answers<-answers[-1]
    print(as.POSIXct(answers, origin = "1970-01-01"))
  }
}
