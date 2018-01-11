# ld_compiled_scores.R

library(tidyverse)
library(readxl)

ld_results_rob_adam <- read_excel("ld_results_rob_adam.xlsx")

df <- ld_results_rob_adam %>%
  mutate(benefits_total = benefits_score + adam_benefits_score)

