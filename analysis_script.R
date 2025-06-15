## -------------------------------------------------------
## The Effect of Pre-Bedtime Habits on Sleep Quality
## Analysis Script
## Author: Felice Dong
## -------------------------------------------------------

## Load required packages
library(tidyverse)
library(ggplot2)
library(pubtheme)
library(estimatr)
library(broom)
library(modelsummary)
library(lubridate)
library(readxl)
library(knitr)
library(kableExtra)

## -------------------------------------------------------
## 1. DATA PREPARATION AND CLEANING
## -------------------------------------------------------

# Import dataset
data <- read_csv("sleep_data_clean.csv")

## -------------------------------------------------------
## 2. EXPLORATORY DATA ANALYSIS
## -------------------------------------------------------

# Create complete dataset including missing values for visualization
complete_data <- data %>%
  complete(
    date = seq(min(date), max(date), by = "1 day"),
    name = unique(data$name)) %>%
  mutate(
    has_data = if_else(
      is.na(assignment),
      "Missing",  # Will be colored gray
      name))

# Visualize data availability by participant
data_availability_plot <- ggplot(complete_data, aes(x = date, y = name)) +
  geom_tile(aes(fill = has_data), color = "white") +
  geom_text(aes(label = format(date, "%b %d")), size = 3) +
  scale_fill_manual(
    values = c("Elyse" = publightblue, "Felice" = publightred, 
               "Missing" = "gray90")) +
  labs(
    title = "Sleep Data Nights by Participant",
    x = "Date",
    y = "Participant") +
  theme_pub() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank())

# Save the plot
ggsave("data_availability_plot.png", data_availability_plot, 
       width = 16, height = 3.5)

# Box plots for sleep quality by treatment
sleep_quality_boxplot <- ggplot(data, aes(x = assignment, y = sleep_quality_app, 
                                          fill = assignment)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Distribution of Sleep Quality by Treatment",
       x = "Treatment",
       y = "Sleep Quality Score") +
  theme_pub()

# Save the plot
ggsave("sleep_quality_boxplot.png", sleep_quality_boxplot)

# Sleep quality by treatment and participant
sleep_quality_facet <- ggplot(data, aes(x = assignment, y = sleep_quality_app, 
                                        fill = assignment)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Distribution of Sleep Quality by Treatment",
       x = "Treatment",
       y = "Sleep Quality Score") +
  facet_wrap(~ name) +
  theme_pub()

# Save the plot
ggsave("sleep_quality_facet.png", sleep_quality_facet)

# Sleep efficacy (app) by treatment and participant
efficacy_app_facet <- ggplot(data, aes(x = assignment, y = efficacy_app, 
                                       fill = assignment)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Distribution of Sleep Efficacy by Treatment (App)",
       x = "Treatment",
       y = "Sleep Efficacy Score") +
  facet_wrap(~ name) +
  theme_pub()

# Save the plot
ggsave("efficacy_app_facet.png", efficacy_app_facet)

# Sleep efficacy (watch) by treatment and participant
efficacy_aw_facet <- ggplot(data, aes(x = assignment, y = efficacy_aw, 
                                      fill = assignment)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Distribution of Sleep Efficacy by Treatment (AW)",
       x = "Treatment",
       y = "Sleep Efficacy Score") +
  facet_wrap(~ name) +
  theme_pub()

# Save the plot
ggsave("efficacy_aw_facet.png", efficacy_aw_facet)

## -------------------------------------------------------
## 3. SUMMARY STATISTICS
## -------------------------------------------------------

# Overall summary statistics by treatment assignment
summary <- data %>% 
  group_by(assignment) %>%
  summarise(
    app_mean_sleep_quality = mean(sleep_quality_app, na.rm = TRUE),
    app_mean_sleep_efficacy = mean(efficacy_app, na.rm = TRUE),
    app_sd_sleep_quality = sd(sleep_quality_app, na.rm = TRUE),
    app_sd_sleep_efficacy = sd(efficacy_app, na.rm = TRUE), 
    aw_mean_sleep_efficacy = mean(efficacy_aw, na.rm = TRUE),
    aw_sd_sleep_efficacy = sd(efficacy_aw, na.rm = TRUE)
  )

# Summary statistics for Felice
summary_felice <- data %>% 
  filter(name == "Felice") %>%
  group_by(assignment) %>%
  summarise(
    app_mean_sleep_quality = mean(sleep_quality_app, na.rm = TRUE),
    app_mean_sleep_efficacy = mean(efficacy_app, na.rm = TRUE),
    app_sd_sleep_quality = sd(sleep_quality_app, na.rm = TRUE),
    app_sd_sleep_efficacy = sd(efficacy_app, na.rm = TRUE), 
    aw_mean_sleep_efficacy = mean(efficacy_aw, na.rm = TRUE),
    aw_sd_sleep_efficacy = sd(efficacy_aw, na.rm = TRUE)
  )

# Summary statistics for Elyse
summary_elyse <- data %>%
  filter(name == "Elyse") %>%
  group_by(assignment) %>%
  summarise(
    app_mean_sleep_quality = mean(sleep_quality_app, na.rm = TRUE),
    app_mean_sleep_efficacy = mean(efficacy_app, na.rm = TRUE), 
    app_sd_sleep_quality = sd(sleep_quality_app, na.rm = TRUE),
    app_sd_sleep_efficacy = sd(efficacy_app, na.rm = TRUE), 
    aw_mean_sleep_efficacy = mean(efficacy_aw, na.rm = TRUE),
    aw_sd_sleep_efficacy = sd(efficacy_aw, na.rm = TRUE)
  )

# Export summary tables
write.csv(summary, "summary_overall.csv", row.names = FALSE)
write.csv(summary_felice, "summary_felice.csv", row.names = FALSE)
write.csv(summary_elyse, "summary_elyse.csv", row.names = FALSE)

# Prepare data for bar plots
summary_long <- summary %>%
  pivot_longer(cols = c(app_mean_sleep_quality, app_mean_sleep_efficacy,
                        aw_mean_sleep_efficacy),
               names_to = "Outcome", 
               values_to = "Mean_Value") %>%
  mutate(assignment = factor(assignment, 
                             labels = c("Phone", "Book")))

summary_long_felice <- summary_felice %>%
  pivot_longer(cols = c(app_mean_sleep_quality, app_mean_sleep_efficacy,
                        aw_mean_sleep_efficacy),
               names_to = "Outcome", 
               values_to = "Mean_Value") %>%
  mutate(assignment = factor(assignment, 
                             labels = c("Phone", "Book")))

summary_long_elyse <- summary_elyse %>%
  pivot_longer(cols = c(app_mean_sleep_quality, app_mean_sleep_efficacy,
                        aw_mean_sleep_efficacy),
               names_to = "Outcome", 
               values_to = "Mean_Value") %>%
  mutate(assignment = factor(assignment, 
                             labels = c("Phone", "Book")))

# Bar plot for Felice's outcomes
felice_outcomes_plot <- ggplot(summary_long_felice, 
                               aes(x = Mean_Value, y = Outcome, 
                                   fill = assignment)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Sleep Outcomes by Assignment (Felice)",
    x = "Mean Score (%)",
    y = NULL
  ) +
  scale_y_discrete(labels = c(
    "app_mean_sleep_quality" = "Sleep Quality (App)",
    "app_mean_sleep_efficacy" = "Sleep Efficacy (App)",
    "aw_mean_sleep_efficacy" = "Sleep Efficacy (Watch)"
  )) +
  geom_text(aes(label = round(Mean_Value, 2)), 
            position = position_dodge(width = 1), 
            hjust = 1.1,
            color = pubbackgray) +
  theme_pub()

# Save the plot
ggsave("felice_outcomes_plot.png", felice_outcomes_plot)

# Bar plot for Elyse's outcomes
elyse_outcomes_plot <- ggplot(summary_long_elyse, 
                              aes(x = Mean_Value, y = Outcome, 
                                  fill = assignment)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Sleep Outcomes by Assignment (Elyse)",
    x = "Mean Score (%)",
    y = NULL
  ) +
  scale_y_discrete(labels = c(
    "app_mean_sleep_quality" = "Sleep Quality (App)",
    "app_mean_sleep_efficacy" = "Sleep Efficacy (App)",
    "aw_mean_sleep_efficacy" = "Sleep Efficacy (Watch)"
  )) +
  geom_text(aes(label = round(Mean_Value, 2)), 
            position = position_dodge(width = 1), 
            hjust = 1.1,
            color = pubbackgray) +
  theme_pub()

# Save the plot
ggsave("elyse_outcomes_plot.png", elyse_outcomes_plot)

# Bar plot for overall outcomes
overall_outcomes_plot <- ggplot(summary_long, 
                                aes(x = Mean_Value, y = Outcome, 
                                    fill = assignment)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Sleep Outcomes by Assignment",
    x = "Mean Score (%)",
    y = NULL
  ) +
  scale_y_discrete(labels = c(
    "app_mean_sleep_quality" = "Sleep Quality (App)",
    "app_mean_sleep_efficacy" = "Sleep Efficacy (App)",
    "aw_mean_sleep_efficacy" = "Sleep Efficacy (Watch)"
  )) +
  geom_text(aes(label = round(Mean_Value, 2)), 
            position = position_dodge(width = 1), 
            hjust = 1.1,
            color = pubbackgray) +
  theme_pub()

# Save the plot
ggsave("overall_outcomes_plot.png", overall_outcomes_plot)

## -------------------------------------------------------
## 4. TREATMENT EFFECT ESTIMATION
## -------------------------------------------------------

# Calculate treatment effects using difference-in-means
sq_app_ate <- difference_in_means(sleep_quality_app ~ assignment, data = data)
se_app_ate <- difference_in_means(efficacy_app ~ assignment, data = data)
se_aw_ate <- difference_in_means(efficacy_aw ~ assignment, data = data)
sub_rating_ate <- difference_in_means(sub_rating ~ assignment, data = data)

# Compile treatment effects and confidence intervals
te <- data.frame(
  sample = c("Sleep Quality (App)", "Sleep Efficacy (App)", 
             "Sleep Efficacy (AW)"),
  effect = c(sq_app_ate %>% pluck(coefficients), 
             se_app_ate %>% pluck(coefficients), 
             se_aw_ate %>% pluck(coefficients)), 
  conf.low = c(sq_app_ate %>% pluck("conf.low"), 
               se_app_ate %>% pluck("conf.low"), 
               se_aw_ate %>% pluck("conf.low")),
  conf.high = c(sq_app_ate %>% pluck("conf.high"), 
                se_app_ate %>% pluck("conf.high"), 
                se_aw_ate %>% pluck("conf.high"))
)

# Create forest plot of treatment effects
te_plot <- ggplot(te, aes(x = sample, y = effect)) +
  geom_point(color = pubdarkred) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, color = pubdarkred) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Treatment Effects and Confidence Intervals",
       x = "Measures", y = "Treatment Effect") +
  theme_pub()

# Save the plot
ggsave("treatment_effects_plot.png", te_plot)

# Compile subjective rating treatment effects
te_sub <- data.frame(
  sample = c("Subjective Ratings"),
  effect = c(sub_rating_ate %>% pluck(coefficients)), 
  conf.low = c(sub_rating_ate %>% pluck("conf.low")), 
  conf.high = c(sub_rating_ate %>% pluck("conf.high"))
)

# Create plot for subjective rating treatment effects
sub_rating_plot <- ggplot(te_sub, aes(x = sample, y = effect)) +
  geom_point(color = pubdarkred) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, color = pubdarkred) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Subjective Ratings",
       x = "Measure", y = "Treatment Effect") +
  theme_pub()

# Save the plot
ggsave("subjective_ratings_plot.png", sub_rating_plot)

## -------------------------------------------------------
## 5. REGRESSION ANALYSIS
## -------------------------------------------------------

# Sleep Quality Models
model1_sq <- lm_robust(sleep_quality_app ~ assignment, 
                       data = data, se_type = "HC2")
model2_sq <- lm_robust(sleep_quality_app ~ assignment + stress + caffeine + 
                         smoking + alcohol + exercise, 
                       data = data, se_type = "HC2")
model3_sq <- lm_robust(sleep_quality_app ~ assignment + name, 
                       data = data, se_type = "HC2")
model4_sq <- lm_robust(sleep_quality_app ~ assignment * name, 
                       data = data, se_type = "HC2")

# Sleep Efficacy App Models
model1_se <- lm_robust(efficacy_app ~ assignment, 
                       data = data, se_type = "HC2")
model2_se <- lm_robust(efficacy_app ~ assignment + stress + caffeine + 
                         smoking + alcohol + exercise, 
                       data = data, se_type = "HC2")
model3_se <- lm_robust(efficacy_app ~ assignment + name, 
                       data = data, se_type = "HC2")
model4_se <- lm_robust(efficacy_app ~ assignment * name, 
                       data = data, se_type = "HC2")

# Sleep Efficacy Watch Models
model1_se_aw <- lm_robust(efficacy_aw ~ assignment, 
                          data = data, se_type = "HC2")
model2_se_aw <- lm_robust(efficacy_aw ~ assignment + stress + caffeine + 
                            smoking + alcohol + exercise, 
                          data = data, se_type = "HC2")
model3_se_aw <- lm_robust(efficacy_aw ~ assignment + name, 
                          data = data, se_type = "HC2")
model4_se_aw <- lm_robust(efficacy_aw ~ assignment * name, 
                          data = data, se_type = "HC2")

# Create model lists
models_sq <- list(model1_sq, model2_sq, model3_sq, model4_sq)
models_se <- list(model1_se, model2_se, model3_se, model4_se)
models_se_aw <- list(model1_se_aw, model2_se_aw, model3_se_aw, model4_se_aw)
model_names <- c("Basic", "With Covariates", 
                 "With Fixed Effects", "Heterogeneity")

# Function to extract treatment effects from models
extract_stats <- function(model, model_name) {
  coef_tbl <- tidy(model)
  assignment_row <- coef_tbl %>% filter(term == "assignmentRead")
  
  data.frame(
    Model = model_name,
    Coefficient = assignment_row$estimate,
    SE = assignment_row$std.error,
    p_value = assignment_row$p.value,
    CI_Lower = assignment_row$conf.low,
    CI_Upper = assignment_row$conf.high,
    R_squared = model$r.squared
  )
}

# Generate regression tables
reg_table_sq <- map2_dfr(models_sq, model_names, extract_stats)
reg_table_se_app <- map2_dfr(models_se, model_names, extract_stats)
reg_table_se_aw <- map2_dfr(models_se_aw, model_names, extract_stats)

# Export regression tables
write.csv(reg_table_sq, "reg_table_sleep_quality.csv", row.names = FALSE)
write.csv(reg_table_se_app, "reg_table_sleep_efficacy_app.csv", 
          row.names = FALSE)
write.csv(reg_table_se_aw, "reg_table_sleep_efficacy_watch.csv", 
          row.names = FALSE)

# Combine all coefficient data for plotting
coef_data <- bind_rows(
  reg_table_sq %>% mutate(Outcome = "Sleep Quality"),
  reg_table_se_app %>% mutate(Outcome = "Sleep Efficacy (App)"),
  reg_table_se_aw %>% mutate(Outcome = "Sleep Efficacy (Watch)")
)

# Create coefficient plot
coef_plot <- ggplot(coef_data, aes(x = Model, y = Coefficient, color = Outcome)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.5), 
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Treatment Effects Across Models and Outcomes",
    x = "Model Type",
    y = "Estimated Effect of Reading vs. Phone"
  ) +
  theme_pub() +
  coord_flip()

# Save the plot
ggsave("coefficient_plot.png", coef_plot, width = 12, height = 6)

## -------------------------------------------------------
## 6. CONDITIONAL AVERAGE TREATMENT EFFECTS (CATEs)
## -------------------------------------------------------

# Calculate CATEs
cate_results <- data.frame(
  Outcome = rep(c("Sleep Quality", "Sleep Efficacy (App)", 
                  "Sleep Efficacy (Watch)"), each = 2),
  Participant = rep(c("Elyse", "Felice"), 3),
  Effect = c(
    # Sleep Quality
    coef(model4_sq)["assignmentRead"],
    coef(model4_sq)["assignmentRead"] + 
      coef(model4_sq)["assignmentRead:nameFelice"],
    # Sleep Efficacy (App)
    coef(model4_se)["assignmentRead"],
    coef(model4_se)["assignmentRead"] + 
      coef(model4_se)["assignmentRead:nameFelice"],
    # Sleep Efficacy (Watch) 
    coef(model4_se_aw)["assignmentRead"],
    coef(model4_se_aw)["assignmentRead"] + 
      coef(model4_se_aw)["assignmentRead:nameFelice"]
  )
)

# Function to calculate standard errors for CATEs
se_calc <- function(model, name = "Felice") {
  coef_matrix <- vcov(model)
  if(name == "Elyse") {
    return(sqrt(coef_matrix["assignmentRead", "assignmentRead"]))
  } else {
    # SE for sum of coefficients (b1 + b3)
    return(sqrt(
      coef_matrix["assignmentRead", "assignmentRead"] + 
        coef_matrix["assignmentRead:nameFelice", "assignmentRead:nameFelice"] + 
        2 * coef_matrix["assignmentRead", "assignmentRead:nameFelice"]
    ))
  }
}

# Add standard errors to the CATE results
cate_results$SE <- c(
  # Sleep Quality
  se_calc(model4_sq, "Elyse"),
  se_calc(model4_sq, "Felice"),
  # Sleep Efficacy (App)
  se_calc(model4_se, "Elyse"),
  se_calc(model4_se, "Felice"),
  # Sleep Efficacy (Watch)
  se_calc(model4_se_aw, "Elyse"),
  se_calc(model4_se_aw, "Felice")
)

# Add confidence intervals
cate_results$CI_Lower <- cate_results$Effect - 1.96 * cate_results$SE
cate_results$CI_Upper <- cate_results$Effect + 1.96 * cate_results$SE

# Export CATE results
write.csv(cate_results, "cate_results.csv", row.names = FALSE)

# Visualize CATEs
cate_plot <- ggplot(cate_results, 
                    aes(x = Participant, y = Effect, color = Participant)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                width = 0.2, 
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = pubdarkgray) +
  facet_wrap(~Outcome) +
  geom_text(aes(label = round(Effect, 1)), 
            vjust = -0.5, 
            hjust = 1.2,
            position = position_dodge(width = 0.5), 
            color = pubdarkgray) +
  labs(
    title = "Treatment Effects by Participant",
    x = "Participant",
    y = "Effect of Reading vs. Phone"
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

# Save the plot
ggsave("cate_plot.png", cate_plot)

## -------------------------------------------------------
## 7. FORMATTED REGRESSION TABLES
## -------------------------------------------------------

# Create coefficient labels for regression tables
coef_labels <- c(
  "assignmentRead" = "Book (vs. Phone)",
  "nameFelice" = "Felice (vs. Elyse)",
  "stress" = "Stress Level",
  "caffeine" = "Caffeine Consumption",
  "smokingYes" = "Smoking",
  "alcoholYes" = "Alcohol Consumption",
  "exerciseYes" = "Exercise",
  "assignmentRead:nameFelice" = "Book × Felice"
)

# Create formatted tables for each outcome
table_sq <- modelsummary(models_sq, 
                         coef_map = coef_labels,
                         title = "Table 1: Effect of Reading vs. Phone on Sleep Quality",
                         stars = TRUE,
                         gof_omit = "IC|Log|F|RMSE",
                         output = "table_sleep_quality.html")

table_se <- modelsummary(models_se, 
                         coef_map = coef_labels,
                         title = "Table 2: Effect of Reading vs. Phone on Sleep Efficacy (App)",
                         stars = TRUE,
                         gof_omit = "IC|Log|F|RMSE",
                         output = "table_sleep_efficacy_app.html")

table_se_aw <- modelsummary(models_se_aw, 
                            coef_map = coef_labels,
                            title = "Table 3: Effect of Reading vs. Phone on Sleep Efficacy (Watch)",
                            stars = TRUE,
                            gof_omit = "IC|Log|F|RMSE",
                            output = "table_sleep_efficacy_watch.html")

# Print summary of heterogeneity models to console
cat("\nSLEEP QUALITY HETEROGENEITY MODEL\n")
print(summary(model4_sq))

cat("\nSLEEP EFFICACY (APP) HETEROGENEITY MODEL\n")
print(summary(model4_se))

cat("\nSLEEP EFFICACY (WATCH) HETEROGENEITY MODEL\n")
print(summary(model4_se_aw))

# Thank you for a great semester! 