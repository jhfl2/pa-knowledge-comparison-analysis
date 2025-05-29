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
plot1 <- create_scatter(question_means, "Medical", "PA", "Medical Student Score", "PA Student Score", "PA-Medical")+
  labs(title = "PA-Medical Stage 5")
plot2 <- create_scatter(question_means, "F1", "PA", "FY1 Doctor Score", "PA Student Score", "PA-F1")
plot3 <- create_scatter(question_means, "Medical", "F1", "Medical Student Score", "FY1 Doctor Score", "Medical-F1")+
  labs(title = "Medical Stage 5 - F1")

# Combine the plots with the patchwork package
library(patchwork)
combined_plot <- (plot1 | plot2) / plot3 + 
  plot_annotation(
    title = "Question Performance Comparison Across Groups",
    subtitle = cor_text,caption = "Red dots indicate questions with greatest disparity"
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

# Find questions where PA and Medical student performance are within 10% of each other
similar_performance_threshold <- 0.10  # 10 percentage points

similar_questions <- question_means %>%
  mutate(
    abs_diff_PA_Medical = abs(diff_PA_Medical),
    similar_performance = abs_diff_PA_Medical <= similar_performance_threshold
  ) %>%
  filter(similar_performance == TRUE)

# Count and percentage of similar questions
total_questions <- nrow(question_means)
similar_count <- nrow(similar_questions)
similar_percentage <- round((similar_count / total_questions) * 100, 1)


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
  scale_y_continuous(limits = c(-.2, 1))


#Calculate correlation between PA Stage 2 and Medical Stage 4
pa2_med4_correlation <- question_scores_wide %>%
  select(PA_2, Medical_4) %>%
  drop_na() %>%
  summarise(
    correlation = cor(PA_2, Medical_4),
    n_questions = n()
  )

# Also calculate confidence intervals
cor_test_result <- cor.test(question_scores_wide$PA_2, 
                            question_scores_wide$Medical_4, 
                            use = "complete.obs")

cat("PA Stage 2 vs Medical Stage 4 correlation: r =", 
    round(pa2_med4_correlation$correlation, 3), "\n")
cat("95% CI: [", round(cor_test_result$conf.int[1], 3), ",", 
    round(cor_test_result$conf.int[2], 3), "]\n")


# Define criteria for "progressive medical knowledge" questions:
# 1. Medical Stage 1 performs poorly (< 50%)
# 2. Medical Stage 5 performs well (> 60%) 
# 3. Shows substantial improvement (>30% increase from Stage 1 to 5)
# 4. Shows consistent upward progression (not just random variation)

progressive_analysis <- question_scores_wide %>%
  select(bank_no, Medical_1, Medical_2, Medical_3, Medical_4, Medical_5, PA_1, PA_2, F1_1) %>%
  drop_na() %>%
  mutate(
    # Calculate improvement from Medical Stage 1 to 5
    med_improvement = Medical_5 - Medical_1,
    
    # Check if it meets our criteria for progressive knowledge
    starts_low = Medical_1 < 0.50,  # Starts difficult
    ends_high = Medical_5 > 0.60,   # Ends well-learned  
    shows_improvement = med_improvement > 0.30,  # Shows substantial improvement
    
    # Check for consistent progression (Stage 2 > Stage 1, Stage 3 > Stage 2, etc.)
    consistent_progression = (Medical_2 > Medical_1) & 
      (Medical_3 > Medical_2) & 
      (Medical_4 > Medical_3) & 
      (Medical_5 > Medical_4),
    
    # Final classification
    is_progressive = starts_low & ends_high & shows_improvement & consistent_progression
  )

# Identify progressive medical knowledge questions
progressive_questions <- progressive_analysis %>%
  filter(is_progressive == TRUE)

# Count how many questions meet criteria
total_questions <- nrow(progressive_analysis)
progressive_count <- nrow(progressive_questions)
progressive_percentage <- round((progressive_count / total_questions) * 100, 1)

progressive_summary <- progressive_questions %>%
  select(bank_no, Medical_1, Medical_2, Medical_3, Medical_4, Medical_5, PA_1, PA_2, F1_1, med_improvement) %>%
  arrange(desc(med_improvement)) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

print(progressive_summary)


# Average performance on progressive questions
avg_med1 <- mean(progressive_questions$Medical_1, na.rm = TRUE)
avg_med2 <- mean(progressive_questions$Medical_2, na.rm = TRUE)
avg_med3 <- mean(progressive_questions$Medical_3, na.rm = TRUE)
avg_med4 <- mean(progressive_questions$Medical_4, na.rm = TRUE)
avg_med5 <- mean(progressive_questions$Medical_5, na.rm = TRUE)
avg_pa1 <- mean(progressive_questions$PA_1, na.rm = TRUE)
avg_pa2 <- mean(progressive_questions$PA_2, na.rm = TRUE)
avg_f1 <- mean(progressive_questions$F1_1, na.rm = TRUE)

avg_performance_data <- data.frame(
  Stage = c("Med-1", "Med-2", "Med-3", "Med-4", "Med-5", "PA-1", "PA-2", "FY1"),
  Programme = c("Medical", "Medical", "Medical", "Medical", "Medical", "PA", "PA", "FY1"),
  Performance = c(avg_med1, avg_med2, avg_med3, avg_med4, avg_med5, avg_pa1, avg_pa2, avg_f1),
  Stage_Numeric = c(1, 2, 3, 4, 5, 1, 2, 1)
)

# Create the aggregate plot
aggregate_plot <- ggplot(avg_performance_data, aes(x = Stage_Numeric, y = Performance, 
                                                   color = Programme, group = Programme)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:5, labels = c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5")) +
  scale_y_continuous(limits = c(-.2, 1), labels = scales::percent_format()) +
  scale_color_manual(values = c("Medical" = "#2E8B57", "PA" = "#4169E1", "FY1" = "#DC143C")) +
  labs(
    title = paste("Average Performance on Progressive Medical Knowledge Questions (n =", progressive_count, ")"),
    subtitle = "Questions requiring progressive development through medical training",
    x = "Training Stage",
    y = "Mean Performance",
    color = "Programme"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) 
print(aggregate_plot)

# 2. Create individual plots for top progressive questions (most improvement)
top_progressive <- progressive_questions %>%
  arrange(desc(med_improvement)) %>%
  head(24)  # Show top n most progressive questions


# Reshape data for plotting
plot_data <- top_progressive %>%
  select(bank_no, Medical_1, Medical_2, Medical_3, Medical_4, Medical_5, PA_1, PA_2, F1_1) %>%
  pivot_longer(cols = -bank_no, names_to = "Group", values_to = "Performance") %>%
  mutate(
    Programme = case_when(
      str_starts(Group, "Medical") ~ "Medical",
      str_starts(Group, "PA") ~ "PA", 
      str_starts(Group, "F1") ~ "FY1"
    ),
    Stage = case_when(
      Group == "Medical_1" ~ 1,
      Group == "Medical_2" ~ 2, 
      Group == "Medical_3" ~ 3,
      Group == "Medical_4" ~ 4,
      Group == "Medical_5" ~ 5,
      Group == "PA_1" ~ 1,
      Group == "PA_2" ~ 2,
      Group == "F1_1" ~ 1
    )
  )

# Create individual question plots
individual_plots <- ggplot(plot_data, aes(x = Stage, y = Performance, color = Programme, group = Programme)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ bank_no, scales = "free_y", ncol = 6) +
  scale_x_continuous(breaks = 1:5, labels = c("1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(-.2, 1), labels = scales::percent_format()) +
  scale_color_manual(values = c("Medical" = "#2E8B57", "PA" = "#4169E1", "FY1" = "#DC143C")) +
  labs(
    title = "Individual Progressive Medical Knowledge Questions",
    subtitle = "Questions showing largest improvement through medical school",
    x = "Training Stage", 
    y = "Performance",
    color = "Programme"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

print(individual_plots)

pa2_scores <- progressive_questions$PA_2
med5_scores <- progressive_questions$Medical_5
f1_scores <- progressive_questions$F1_1

# Calculate means and SDs
pa2_mean <- mean(pa2_scores)
med5_mean <- mean(med5_scores)
f1_mean <- mean(f1_scores)

pa2_sd <- sd(pa2_scores)
med5_sd <- sd(med5_scores)
f1_sd <- sd(f1_scores)

# Calculate Cohen's d
cohens_d_pa_med <- (med5_mean - pa2_mean) / sqrt((pa2_sd^2 + med5_sd^2) / 2)
cohens_d_pa_f1 <- (f1_mean - pa2_mean) / sqrt((pa2_sd^2 + f1_sd^2) / 2)

# Perform t-tests
t_test_pa_med <- t.test(med5_scores, pa2_scores)
t_test_pa_f1 <- t.test(f1_scores, pa2_scores)

# PA Stage 2 vs. Medical Stage 5
cor_pa2_med5 <- cor(progressive_questions$PA_2, progressive_questions$Medical_5, use = "complete.obs")
cat("Correlation between PA Stage 2 and Medical Stage 5:", round(cor_pa2_med5, 3), "\n")

# PA Stage 2 vs. FY1
cor_pa2_f1 <- cor(progressive_questions$PA_2, progressive_questions$F1_1, use = "complete.obs")
cat("Correlation between PA Stage 2 and FY1:", round(cor_pa2_f1, 3), "\n")

# Medical Stage 5 vs. FY1
cor_med5_f1 <- cor(progressive_questions$Medical_5, progressive_questions$F1_1, use = "complete.obs")
cat("Correlation between Medical Stage 5 and FY1:", round(cor_med5_f1, 3), "\n")

pa_excel_analysis <- question_scores_wide %>%
  select(bank_no, Medical_1, Medical_2, Medical_3, Medical_4, Medical_5, PA_1, PA_2, F1_1) %>%
  drop_na() %>%
  mutate(
    # Calculate medical school progression
    med_improvement = Medical_5 - Medical_1,
    
    # Criteria for PA-excel questions:
    # 1. PAs perform well (PA_2 > 60%)
    # 2. Medical students show minimal progression (< 30% improvement)
    # 3. Medical students perform poorly overall (Medical_5 < 32%)
    # 4. FY1s also perform poorly (F1_1 < 30%)
    
    pa_performs_well = PA_2 > 0.60,
    minimal_med_progression = med_improvement < 0.3,
    med_performs_poorly = Medical_5 < 0.32,
    fy1_performs_poorly = F1_1 < 0.3,
    
    # Final classification
    is_pa_domain = pa_performs_well & minimal_med_progression & med_performs_poorly & fy1_performs_poorly
  ) %>%
  filter(is_pa_domain)




# Reshape data for plotting
plot_data <- pa_excel_analysis %>%
  select(bank_no, Medical_1, Medical_2, Medical_3, Medical_4, Medical_5, PA_1, PA_2, F1_1) %>%
  pivot_longer(cols = -bank_no, names_to = "Group", values_to = "Performance") %>%
  mutate(
    Programme = case_when(
      str_starts(Group, "Medical") ~ "Medical",
      str_starts(Group, "PA") ~ "PA", 
      str_starts(Group, "F1") ~ "FY1"
    ),
    Stage = case_when(
      Group == "Medical_1" ~ 1,
      Group == "Medical_2" ~ 2, 
      Group == "Medical_3" ~ 3,
      Group == "Medical_4" ~ 4,
      Group == "Medical_5" ~ 5,
      Group == "PA_1" ~ 1,
      Group == "PA_2" ~ 2,
      Group == "F1_1" ~ 1
    )
  )

# Create the plot
pa_excel_plot <- ggplot(plot_data, aes(x = Stage, y = Performance, color = Programme, group = Programme)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 3) +
  facet_wrap(~ bank_no, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = 1:5, labels = c("1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(-.2, 1), labels = scales::percent_format()) +
  scale_color_manual(values = c("Medical" = "#2E8B57", "PA" = "#1f77b4", "FY1" = "#d62728")) +
  labs(
    title = "Questions Where PAs Excel with Limited Medical School Progression",
    subtitle = "PAs demonstrate knowledge in domains which may not be emphasised in medical curricula",
    x = "Training Stage", 
    y = "Performance",
    color = "Programme"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(override.aes = list(size = 4)))

print(pa_excel_plot)
