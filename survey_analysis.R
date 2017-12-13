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
install_packages(c("dplyr", "tidytext", "readr", "ggplot2", "stringr", "dataMeta"))

# Load the data
survey = read_csv('ld_results_final__for_rob_recoded_trimmed.csv', col_names = TRUE)

# [1] "start_date"                  "end_date"                    "response_type"              
# [4] "progress"                    "duration"                    "finished"                   
# [7] "date_recorded"               "response_id"                 "work_area"                  
# [10] "work_area_other_explanation" "work_area_merged"            "linked_data_description"    
# [13] "description_quality"         "description_opinion"         "linked_data_benefits"       
# [16] "benefits_quality"            "benefits_score"              "additional_thoughts"        
# [19] "combined_words" 

column_types <- c(0, 0, 0, 
                  0, 0, 1, 
                  0, 0, 1,
                  0, 1, 0, 
                  0, 0, 0, 
                  0, 0, 0, 
                  0)

column_descriptions <- c("Date the survey was started",
                         "Date the survey was completed",
                         "Where the survey was filled out [IP Address = web]",
                         "Percentage of the survey completed",
                         "The time taken to complete the survey in seconds",
                         "If the survey was completed",
                         "Date the survey was completed",
                         "Unique anonymous identifier for the response",
                         "Respondants area of work",
                         "Explanation of other response in work area",
                         "Recoded work area to group like explanations",
                         "Respondants description of what linked data is to a non expert",
                         "Quality of the descriptiong scored independently by the authors",
                         "If the respondant was postive, negative or neutral towards linked data, estimated by the authors",
                         "Respondants description of the benefits of linked data",
                         "The quality of the benefits answer, scored indepenently by the authors",
                         "If the respondants felt linked data was beneficial, not beneficial or neutral",
                         "Respondants can add additional comments",
                         "The three text responsed, linked_data_description, linked_data_benefits and additional_thoughts combined"
                         )



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




