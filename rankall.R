# FINDING A LIST OF HOSPITALS WITH CORRESPONDING RANKINGS

library(tidyverse)
library(stringr)

rankall <- function(outcome, num = "best") {

  outcome_data <- read.csv("outcome-of-care-measures.csv")
  test_outcome_df <- select(outcome_data, "hospital name" = Hospital.Name, State, 
                            "heart attack" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                            "heart failure" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                            "pneumonia" = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  
  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  if (sum(str_detect(outcome_list, outcome)) == 0) {
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
    filter(Type == outcome) %>% 
    arrange(State, Rate, `hospital name`)  %>% 
    drop_na(Rate) %>% 
    group_by(State) %>% 
    mutate(id = row_number())
  
  split_data <- split(ranking_hospital_in_a_state, ranking_hospital_in_a_state$State)
  state_list <- names(split_data)
  hospital_name <- c()
  
  if (num == "best") {
    num <- 1
    ranking_hospital_in_a_state <- ranking_hospital_in_a_state %>% 
      group_by(State) %>% 
      filter(id == 1) 
    df <- data.frame(hospital = ranking_hospital_in_a_state$`hospital name`, state = ranking_hospital_in_a_state$State)
    df
  } else if (num == "worst") {
    ranking_hospital_in_a_state <- ranking_hospital_in_a_state %>% 
      group_by(State) %>% 
      filter(row_number() == n()) 
    df <- data.frame(hospital = ranking_hospital_in_a_state$`hospital name`, state = ranking_hospital_in_a_state$State)
    df
  } else {
    for (x in 1:length(state_list)) {
      sub_df <- split_data[[x]]
      if (num %in% sub_df[["id"]]) {
        hospital_name <- append(hospital_name, sub_df[[1]][num])
      } else {
        hospital_name <- append(hospital_name, NA)
      }
    }
    df <- data.frame(hospital = hospital_name, state = state_list)
    df
  }
}