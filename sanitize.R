# Load the data
survey = read_csv('ld_results_final__for_rob_recoded_trimmed.csv', col_names = TRUE)


# Mush all th etext columns together to increase power
survey$combined_words <- paste(survey$linked_data_description, survey$linked_data_benefits, survey$additional_thoughts)

# Tokenize the words
tokenized_survey <- survey %>% unnest_tokens(word, combined_words)



tokenized_survey <- tokenized_survey %>% anti_join(stop_words)

# Columns to remove due to PII or general non-usefullness
drops <-  c(
  'response_type',
  'progress',
  'finished',
  'linked_data_description',
  'linked_data_benefits',
  'additional_thoughts'
)

sanitized_survey <- tokenized_survey[, !(names(tokenized_survey) %in% drops)]
write.csv(sanitized_survey, file="sanitized_survey.csv")