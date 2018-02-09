# ld_compiled_scores.R

library(tidyverse)
library(readxl)
library(tidytext)
library(wordcloud)
library(ggplot2)

data(stop_words)


ld_results_rob_adam <- read_excel("ld_results_rob_adam.xlsx")

df <- ld_results_rob_adam %>%
  mutate(benefits_total = benefits_score + adam_benefits_score)



# word cloud
text_df <- df %>%
  unnest_tokens(word, linked_data_benefits) %>% 
  anti_join(stop_words)
text_df %>% count(word) %>% with(wordcloud(word, n, max.words = 60))
dev.copy(jpeg,filename="plots/benefits_wordcloud.jpg");
dev.off ();

# word cloud of high benefits
text_df_high_benefits <- df %>%
  filter(benefits_total == 2) %>%
  unnest_tokens(word, linked_data_benefits) %>% 
  anti_join(stop_words)

text_df_high_benefits %>% count(word) %>% with(wordcloud(word, n, max.words = 60))
dev.copy(jpeg,filename="plots/high_benefits_wordcloud.jpg");
dev.off ();


# term frequency

benefits_words <- df %>%
  filter(benefits_total == 2) %>%
  unnest_tokens(word, linked_data_benefits) %>%
  count(response_id, word, sort = TRUE) %>%
  ungroup()

total_words <- benefits_words %>% 
  group_by(response_id) %>% 
  summarize(total = sum(n))

benefits_words <- left_join(benefits_words, total_words)

freq_by_rank <- benefits_words %>% 
  group_by(response_id) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = response_id)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


benefits_words <- benefits_words %>%
  bind_tf_idf(word, response_id, n)
benefits_words


benefits_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))
