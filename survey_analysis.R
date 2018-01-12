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
install_packages(c("topicmodels", "dplyr", "tidytext", "readr", "ggplot2", "stringr", "dataMeta"))

# Load the data
survey = read_csv('ld_results_final__for_rob_recoded_trimmed.csv', col_names = TRUE)


# Mush all th etext columns together to increase power
survey$combined_words <- paste(survey$linked_data_description, survey$linked_data_benefits, survey$additional_thoughts)

# Tokenize the words
tokenized_survey <- survey %>% unnest_tokens(word, combined_words)

# Remove stop words and join
data("stop_words")
tokenized_survey <- tokenized_survey %>% anti_join(stop_words)


# NRC changes the structre a bit since each word in the word column can get 
# multiple NRC sentiments. Making a new dataframe makes it a little easier
# for the other sentiment measure.
tokenized_survey_nrc <- tokenized_survey %>% inner_join(get_sentiments("nrc"))

# The rest can just go in to the main data frame.
tokenized_survey <- tokenized_survey %>% inner_join(get_sentiments("bing"))
tokenized_survey$bing_sentiment <- tokenized_survey$sentiment

tokenized_survey$sentiment <- NULL

tokenized_survey <- tokenized_survey %>% inner_join(get_sentiments("afinn"))
tokenized_survey$afinn_sentiment <- tokenized_survey$score

tokenized_survey$score <- NULL


bing_word_counts <- tokenized_survey %>% 
  count(word, bing_sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(bing_sentiment) %>%
  top_n(7) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = bing_sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~bing_sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


###
# 
# Topic modeling
#
###


frequencies <- tokenized_survey %>% 
               group_by(response_id) %>% 
               count(word, sort = TRUE) %>% 
               left_join(tokenized_survey %>% 
               group_by(response_id) %>% 
               summarise(total = n()))

top_terms <- survey_topics %>% group_by(topic) %>%
top_n(10, beta) %>%
ungroup() %>%
arrange(topic, -beta)


top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


