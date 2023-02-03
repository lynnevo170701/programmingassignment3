rankhospital <- function(state, outcome, num  = "best") {
  chosen_state <- state
  #Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  test_outcome_df <- select(outcome_data, "hospital name" = Hospital.Name, State, 
                            "heart attack" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                            "heart failure" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                            "pneumonia" = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)

  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  if (sum(str_detect(outcome_data[, 7], chosen_state)) == 0 ) {
    stop("invalid state")
  } else if (sum(str_detect(outcome_list, outcome)) == 0) {
    stop("invadlid outcome")
  } 
  
  #Preparing tables 
  #changing wide data into long data
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
  
  #after sorting by State and Type and Rate in an ascending order, 
  #we group data by State and Type to rank in a correct way 
  ranking_hospital_in_a_state <- long_test_outcome_df %>% 
    filter(State == chosen_state & Type == outcome) %>% 
    arrange(Rate, `hospital name`) %>%
    drop_na() %>% 
    mutate(id = row_number())
  
  #Return hospital name in that state with lowest 30-day death rate
  #i variable to find the index 
  if (num == "best") {
    num <- 1
    ranking_hospital_in_a_state$`hospital name`[1]
  } else if (num == "worst") {
    ranking_hospital_in_a_state <- ranking_hospital_in_a_state %>% 
      filter(row_number() == n())
    ranking_hospital_in_a_state$`hospital name`[1]
  } else {
    ranking_hospital_in_a_state$`hospital name`[num]
  }
}