#Data are available here: https://osf.io/w2q5z/files/osfstorage

#Big thanks to Claude for the help with the code

library(tidyverse)
library(ggrepel) # For better text label placement

data <- read_csv("Data.csv") %>%
  clean_names()

# Filter for all three groups we want to compare
data_filtered <- data %>%
  filter(
    (programme == "Medical" & stage == 5) |
      (programme == "PA" & stage == 2) |
      (programme == "F1" & stage == 1)
  )

# Calculate mean scores per question for each group
question_means <- data_filtered %>%
  group_by(bank_no, programme) %>%
  summarise(mean_score = mean(score), .groups = "drop") %>%
  pivot_wider(names_from = programme, values_from = mean_score)

# We might have some NA values if certain questions weren't answered by all groups
question_means <- question_means %>% filter(!is.na(Medical) & !is.na(PA) & !is.na(F1))

# Calculate the differences between scores
question_means <- question_means %>%
  mutate(
    diff_PA_Medical = PA - Medical,
    diff_PA_F1 = PA - F1,
    diff_Medical_F1 = Medical - F1,
    extreme_diff = ifelse(abs(diff_PA_Medical) > 0.5 | abs(diff_PA_F1) > 0.5, TRUE, FALSE)
  )

# Calculate correlation coefficients
cor_PA_Med <- cor(question_means$Medical, question_means$PA, use = "complete.obs")
cor_PA_F1 <- cor(question_means$F1, question_means$PA, use = "complete.obs")
cor_Med_F1 <- cor(question_means$Medical, question_means$F1, use = "complete.obs")

# Create a combined correlation text
cor_text <- sprintf(
  "Correlations: PA-Medical = %.3f, PA-FY1 = %.3f, Medical-FY1 = %.3f",
  cor_PA_Med, cor_PA_F1, cor_Med_F1
)

# Create a function to generate pairwise scatter plots
create_scatter <- function(data, x_var, y_var, x_label, y_label, title) {
  # Compute correlation
  cor_val <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
  
  # Create diff variable name based on title
  diff_var <- case_when(
    title == "PA-Medical" ~ "diff_PA_Medical",
    title == "PA-F1" ~ "diff_PA_F1",
    title == "Medical-F1" ~ "diff_Medical_F1",
    TRUE ~ NA_character_
  )
  
  # Create the plot
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(aes(color = extreme_diff, size = extreme_diff)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    geom_smooth(method = "lm", color = "blue", se = TRUE) +
    scale_color_manual(values = c("black", "red")) +
    scale_size_manual(values = c(1, 3)) +
    xlim(0, 1) + 
    ylim(0, 1) +
    labs(
      title = title,
      subtitle = paste("Correlation:", round(cor_val, 3)),
      x = x_label,
      y = y_label
    ) +
    # Label extreme questions
    geom_text_repel(
      data = data %>% filter(abs(!!sym(diff_var)) > 0.7),
      aes(label = bank_no),
      size = 3,
      max.overlaps = 15
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Create three pairwise comparison plots
plot1 <- create_scatter(question_means, "Medical", "PA", "Medical Student Score", "PA Student Score", "PA-Medical")
plot2 <- create_scatter(question_means, "F1", "PA", "FY1 Doctor Score", "PA Student Score", "PA-F1")
plot3 <- create_scatter(question_means, "Medical", "F1", "Medical Student Score", "FY1 Doctor Score", "Medical-F1")

# Combine the plots with the patchwork package
library(patchwork)
combined_plot <- (plot1 | plot2) / plot3 + 
  plot_annotation(
    title = "Question Performance Comparison Across Groups",
    subtitle = cor_text
  )

# Display the combined plot
combined_plot

# Create a dataset of extreme outliers across any comparison
extreme_outliers <- question_means %>%
  filter(abs(diff_PA_Medical) > 0.6 | abs(diff_PA_F1) > 0.6 | abs(diff_Medical_F1) > 0.6) %>%
  select(bank_no, Medical, PA, F1, diff_PA_Medical, diff_PA_F1, diff_Medical_F1) %>%
  arrange(desc(abs(diff_PA_Medical)))

# Print the most extreme questions
print("Questions with the most extreme differences:")
print(extreme_outliers)