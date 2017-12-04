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

survey = read_csv('ld_results_final__for_rob_recoded_trimmed.csv', col_names = TRUE)
survey$combined_words <- paste(survey$linked_data_description, survey$linked_data_benefits, survey$additional_thoughts)
tokenized_survey <- survey %>% unnest_tokens(word, combined_words)
data("stop_words")
tokenized_survey <- tokenized_survey %>% anti_join(stop_words)
tokenized_survey <- tokenized_survey %>% inner_join(get_sentiments("bing"))
tokenized_survey$bing_sentiment <- tokenized_survey$sentiment

tokenized_survey$sentiment <- NULL

tokenized_survey <- tokenized_survey %>% inner_join(get_sentiments("nrc"))
tokenized_survey$nrc_sentiment <- tokenized_survey$sentiment

tokenized_survey$sentiment <- NULL

tokenized_survey <- tokenized_survey %>% inner_join(get_sentiments("afinn"))
tokenized_survey$afinn_sentiment <- tokenized_survey$score

tokenized_survey$score <- NULL



