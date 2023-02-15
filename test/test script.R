state_test <- function(state){
  if (str_detect(outcome$State, state) ) {
    state
  } else {
    stop("invalid state")
  }
}

outcome_test <- function(outcome){
  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  if (sum(str_detect(outcome_list, outcome)) > 0 ) {
    outcome
  } else {
    stop("invadlid outcome")
  }
}


test_outcome_df <- select(outcome_df, "hospital name" = Hospital.Name, State, 
                          "heart attack" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                          "heart failure" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                          "pneumonia" = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)

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
best_hospital_in_a_state <- long_test_outcome_df %>% 
  filter(Type == "heart attack") %>% 
  arrange(State, Rate)  %>% 
  drop_na(Rate) %>% 
  group_by(State) %>% 
  mutate(id = row_number())

test_name <- function(state, outcome){
  i <- which(c(state == best_hospital_in_a_state$State & outcome == best_hospital_in_a_state$Type))
  best_hospital_in_a_state$Hospital.Name[i]
}

test <- function(chosen_state, outcome) {
  test_outcome_df <- select(outcome_df, "hospital name" = Hospital.Name, State, 
                            "heart attack" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                            "heart failure" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                            "pneumonia" = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  test_outcome_df <- test_outcome_df[test_outcome_df$State == chosen_state,]
  col_indices <- grep(outcome, colnames(test_outcome_df))
  col_chosen <- c(1,2,col_indices)
  test_outcome_df <- test_outcome_df[col_chosen]
  test_outcome_df[, 3] <- as.numeric(test_outcome_df[, 3])
  test_outcome_df <- test_outcome_df[order(outcome, `hospital name`),]
  test_outcome_df
}


test_outcome_df <- test_outcome_df[test_outcome_df$State == "TX",]
col_indices <- grep("heart failure", colnames(test_outcome_df))
col_chosen <- c(1,2,col_indices)
test_outcome_df_1 <- test_outcome_df[col_chosen]
test_outcome_df_1[, 3] <- as.numeric(test_outcome_df_1[, 3])
test_outcome_df_1 <- test_outcome_df_1[order(test_outcome_df_1$`heart failure`),]

table <- split(best_hospital_in_a_state, best_hospital_in_a_state$State)
state_list <- names(table)
hospital_name <- c()
for (x in 1:length(state_list)) {
  sub_df <- table[[x]]
  if (5 %in% sub_df[[5]]) {
    hospital_name <- append(hospital_name, sub_df[[1]][5])
  } else {
    hospital_name <- append(hospital_name, NA)
  }
}
print(hospital_name)

if (2 %in% sub_df[[5]]) {
  hospital_name <- append(hospital_name, sub_df[[1]][2])
} else {
  hospital_name <- append(hospital_name, NA)
}