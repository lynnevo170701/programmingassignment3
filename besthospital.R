#FINDING THE BEST HOSPITAL IN A STATE

best <- function(state, outcome) {
  chosen_state <- state
  #Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  test_outcome_df <- select(outcome_data, "hospital name" = Hospital.Name, State, 
                            "heart attack" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                            "heart failure" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                            "pneumonia" = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  
  #Check that state and outcome are valid
  #using str_detect function from stringr to find the string
  #by using sum function and compare with value 0, if the string of "state" is invalid
  #it will return FALSE (FALSE = 0) so the sum of all FALSE(s) is still 0
  #it will execute the stop function with the error message
  #otherwise it will return TRUE (TRUE = 1 > 0) and it keeps checking with 
  #the next condition "outcome". This works similar as "state".
  
  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  if (sum(str_detect(outcome_data[, 7], chosen_state)) == 0 ) {
    stop("invalid state")
  } else if (sum(str_detect(outcome_list, outcome)) == 0) {
    stop("invadlid outcome")
  } 
  
  #Preparing tables 
  #changing wide data into long data and change the "rate" column to numeric to
  #sort the data in a correct order but because the column is a list of different elements
  #so we have to unlist() first 
  long_test_outcome_df <- test_outcome_df %>% 
    pivot_longer(
      cols = c('heart attack',
               'heart failure',
               'pneumonia'),
      names_to = "Type",
      values_to = "Rate",
      values_drop_na = TRUE
    ) 
  long_test_outcome_df[, 4] <- as.numeric(unlist(long_test_outcome_df[, 4]))
  
  #create a best_hospital table with only the match state and type. Because of handling ties,
  #we use distinct() function to remove all the rows that have the same state and type. 
  best_hospital_in_a_state <- long_test_outcome_df %>% 
    filter(State == chosen_state & Type == outcome) %>% 
    arrange(Rate, `hospital name`) %>% 
    distinct(State, Type, .keep_all = TRUE)
  
  #Return hospital name in that state with lowest 30-day death rate
  best_hospital_in_a_state$`hospital name`
}

