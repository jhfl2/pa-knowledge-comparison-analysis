#Assumes you have run previous code first
#Again thanks to Claude
library(tidyverse)

# First, analyze correlation patterns between all stages
# Calculate mean scores for each question by programme and stage
stage_question_means <- data %>%
  group_by(bank_no, programme, stage) %>%
  summarise(mean_score = mean(score), .groups = "drop") %>%
  # Create a combined group identifier
  mutate(group = paste(programme, stage, sep = "_"))

# Create wide format data for correlation analysis
question_scores_wide <- stage_question_means %>%
  select(bank_no, group, mean_score) %>%
  pivot_wider(names_from = group, values_from = mean_score)

# Remove questions with missing values for any group
question_scores_complete <- question_scores_wide %>%
  drop_na()

# Calculate correlation matrix
cor_matrix <- cor(question_scores_complete %>% select(-bank_no), 
                  use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Visualize correlation patterns
# This can be done with a simple correlation matrix plot
library(corrplot)
corrplot(cor_matrix, method = "color", 
         type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45,
         title = "Correlations Between Groups")

# Analyze whether PA1 correlates better with any specific medical stage
pa1_correlations <- cor_matrix["PA_1", grep("Medical", colnames(cor_matrix))]
pa2_correlations <- cor_matrix["PA_2", grep("Medical", colnames(cor_matrix))]

print("PA Stage 1 correlations with Medical stages:")
print(pa1_correlations)

print("PA Stage 2 correlations with Medical stages:")
print(pa2_correlations)

# Examine progression for specific questions with extreme differences
interesting_questions <- c("M3433", "M0087", "M1455", "M3497", "M3425", "M3411", "M9041", "M9036")

# Create a dataset for these questions
progression_data <- stage_question_means %>%
  filter(bank_no %in% interesting_questions)

# Plot progression for these questions
ggplot(progression_data, aes(x = stage, y = mean_score, color = programme, group = programme)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ bank_no) +
  theme_minimal() +
  labs(title = "Performance Progression on Selected Questions",
       y = "Mean Score") +
  scale_y_continuous(limits = c(0, 1))