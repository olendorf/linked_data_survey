###################
## Automate package install and load

is_installed <- function(package_name) is.element(package_name, installed.packages()[,1])

# If a package is not installed, install it. Then load the package.
install_and_load <- function(package_name) {
  if(!is_installed(package_name)) {
    install.packages(package_name)
  }
  library(package_name, character.only = TRUE)
}

install_packages <- function(packages) {
  for(package in packages) {
    install_and_load(package)
  }
}

# Install and load libraries
install_packages(c("dplyr", "tidytext", "readr", "ggplot2", "stringr"))

# Load the data
survey = read_csv('ld_results_final__for_rob_recoded_trimmed.csv', col_names = TRUE)

# Mush all th etext columns together to increase power
survey$combined_words <- paste(survey$linked_data_description, survey$linked_data_benefits, survey$additional_thoughts)


# Tokenize the words
tokenized_survey_word_counts <- survey %>% 
  unnest_tokens(word, combined_words) %>%
  count(response_id, word, sort=TRUE) %>%
  ungroup()

total_words <- tokenized_survey_word_counts %>%
  group_by(response_id) %>%
  summarize(total = sum(n))

tokenized_survey_word_counts <- left_join(tokenized_survey_word_counts, total_words)


freq_by_rank <- tokenized_survey_word_counts %>%
  group_by(response_id) %>%
  mutate(rank = row_number(), 'term frequency' = n/total)


tokenized_tf_idf <- tokenized_survey_word_counts %>%
  bind_tf_idf(term_col = word, document_col = response_id, n_col = n)

tokenized_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(response_id) %>%
  top_n(2)


# Remove stop words and join
data("stop_words")
tokenized_survey <- tokenized_survey %>% anti_join(stop_words)

# raw count after tokenizing by word
tokenized_word_counts <- tokenized_survey %>% 
  count(word, sort = TRUE)

  


# review bigrams

survey$participant_number = 1:nrow(survey)

combined_bigrams <- survey %>%
  unnest_tokens(bigram, combined_words, token = "ngrams", n = 2) %>% 
  select(participant_number, bigram) %>% 
  arrange(participant_number)

bigram_counts <- combined_bigrams %>% 
  count(bigram, sort = TRUE)

bigram_tf_idf <- combined_bigrams %>%
  count(participant_number, bigram) %>%
  bind_tf_idf(bigram, participant_number, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf



