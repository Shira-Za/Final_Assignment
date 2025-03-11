# Final Assignment R Beginners Course
# Shira Zadok, ID 318958311

#### Part 1 - Choosing the Data : ----
# This data set is the gathered data from an experiment conducted in our lab. 
# This experiment examines the “Gaze Aversion” phenomena and whether people grasp 
# other peoples’ gaze aversion as a thinking process. To manipulate gaze aversion, 
# we asked participants to look at 9 different locations on the screen and filmed 
# their response. We used these videos in three experiments - in one, the videos
# were shown without changes (we only clipped them to be a few seconds long), 
# in the second, the eyes were cut out, and only the face was shown, and in the 
# third, only the eyes were shown. These participants were the interviewees group. 
# Then, we showed another group of participants the videos and asked them to answer 
# a few questions about the short videos they observed - they were the observers group. 
# So, I chose this data set to assist a colleague with the analysis of the data 
# and also because I find this project interesting and relevant to understanding 
# social interactions better. 

#### Pre-processing: ----
#loading data and libraries:
setwd("~/Documents/2nd_Degree/Courses/R course/FA_SZ")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggdist)
library(patchwork)

df <- read.csv("all_data.csv")

# Data cleaning:
df <- df |>
  filter(!is.na(AQ_Par)) |>
  select(participant, action, action_code, response_key.keys, response_key.corr, 
         response_key.rt, intvwee, expName, AQ_Par, AQ_intvwee) |>
  mutate(AQ_Par = factor(AQ_Par, levels = c(0, 1), labels = c("Low", "High")),
         AQ_intvwee = factor(AQ_intvwee, levels = c(0, 1), labels = c("Low", "High")),
         expName = factor(expName, levels = c("calssify_interact", "calssify_interact_onlyeyes", "calssify_interact_noeyes"), 
                          labels = c("classify_interact", "Only_eyes", "No_eyes")),
         action = factor(action, levels = c('Tap', 'Listen', 'Solve'), labels = c('Wait', 'Listen', 'Think'))
         )

# Ensure numeric variables are not factors or logical
df$response_key.keys <- as.numeric(df$response_key.keys)
df$response_key.corr <- as.numeric(df$response_key.corr)
df$action_code <- as.numeric(df$action_code)

# Exploring the Data:

# Response times across experiments:
p1 = ggplot(df, aes(x = response_key.rt, fill = expName)) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  labs(title = "RT Distribution by Experiment", 
       x = "Response Time (s)", y = "Density")

p2 = ggplot(df, aes(x = expName, y = response_key.rt, fill = expName)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "RT Across Experiments", 
       x = "Experiment", y = "Response Time (s)")

# Accuracy Across Experiments
p3 = ggplot(df, aes(x = expName, y = response_key.corr, fill = expName)) +
  stat_gradientinterval() +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  labs(title = "Accuracy by Experimental Condition", y = "Accuracy", x = "Condition") +
  theme_minimal()

# Response Time vs Accuracy
p4 = ggplot(df, aes(x = response_key.rt, y = response_key.corr, color = expName)) +
  geom_jitter(position = position_jitter(width = 0.2, height = 0), size = 1.5, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Response Time vs Accuracy", x = "Response Time (s)", y = "Correct Response")

# Reaction Time by AQ_Par
p5 = ggplot(df, aes(x = AQ_Par, y = response_key.rt, fill = AQ_Par)) +
  stat_halfeye(alpha = 0.6) +
  labs(title = "Reaction Time by Participant AQ", y = "Reaction Time (s)", x = "AQ Group") +
  theme_minimal()

# Accuracy by AQ_Par
p6 = ggplot(df, aes(x = AQ_Par, y = response_key.corr, fill = AQ_Par)) +
  stat_halfeye() +
  labs(title = "Accuracy by Participant AQ", y = "Accuracy", x = "AQ Group") +
  theme_minimal()

final_p = (p1 | p2) / (p3 | p4) / (p5 | p6)
print(final_p)

#### Reasearch Question: ----

# How do AQ levels (High vs. Low) of both the participant (AQ_Par) and the 
# interviewer (AQ_intvwee) influence performance in a classification task across
# different experimental conditions? Specifically, how do AQ levels predict the
# likelihood of a correct response, reaction time, sensitivity, precision, 
# F1-score, and accuracy?

#Based on the research question, the outcome variables include the likelihood 
#of a correct response, reaction time, sensitivity, precision, F1-score, 
#and accuracy, which are used to assess performance in a classification task. 
#The predictor variables include the participant's AQ level (AQ_Par) and the 
#interviewee's AQ level (AQ_intvwee), both of which are categorical variables 
#with two levels: "Low" (0) and "High" (1). Additionally, the experimental condition 
#(expName) is a categorical variable representing different manipulations of the 
#stimuli shown to participants. These predictors will help determine how AQ 
#levels and experimental conditions influence performance in the task.

#Using new function we haven't learned:
library(datawizard)  # New package for descriptive statistics we didn't learn about.

# Function to calculate summary measures using describe_distribution()
calculate_summary <- function(df) {
  action_code_target <- 2  # Define the target action code for "Think"
  
  df |>
    group_by(expName) |>
    summarise(
      number = n(),
      accuracy_stats = list(describe_distribution(as.numeric((action_code == action_code_target & response_key.keys == action_code_target) |
                                                               (action_code != action_code_target & response_key.keys != action_code_target)))),
      n_think_stats = list(describe_distribution(as.numeric(response_key.keys == action_code), na.rm = TRUE)),
      n_choseThink_stats = list(describe_distribution(as.numeric(response_key.keys[action == "Think"] == action_code_target), na.rm = TRUE)),
      sensitivity = ifelse(sum(action_code == action_code_target) > 0,
                           mean((action_code == action_code_target) & (response_key.keys == action_code_target), na.rm = TRUE) / 
                             mean(action_code == action_code_target, na.rm = TRUE), 0),
      precision = ifelse(sum(response_key.keys == action_code_target, na.rm = TRUE) > 0,
                         mean((action_code == action_code_target) & (response_key.keys == action_code_target), na.rm = TRUE) / 
                           mean(response_key.keys == action_code_target, na.rm = TRUE), 0),
      F1_score = ifelse(sensitivity + precision > 0, 2 * (sensitivity * precision) / (sensitivity + precision), 0)
    ) |>
    mutate(
      accuracy_mean = sapply(accuracy_stats, `[[`, "Mean"),
      accuracy_sd = sapply(accuracy_stats, `[[`, "SD"),
      n_think_mean = sapply(n_think_stats, `[[`, "Mean"),
      n_think_sd = sapply(n_think_stats, `[[`, "SD"),
      n_choseThink_mean = sapply(n_choseThink_stats, `[[`, "Mean"),
      n_choseThink_sd = sapply(n_choseThink_stats, `[[`, "SD")
    ) |>
    select(-accuracy_stats, -n_think_stats, -n_choseThink_stats)  # Remove list columns after extraction
}


# Generate summary tables divided by AQ groups and experimental conditions
summary_AQ_Par <- df |>
  group_by(AQ_Par) |>
  group_modify(~ calculate_summary(.x))
summary_AQ_intvwee <- df |>
  group_by(AQ_intvwee) |>
  group_modify(~ calculate_summary(.x))

# Print results
print(summary_AQ_Par)
print(summary_AQ_intvwee)

#### Analyzing the Data ----

# Loading libraries:
library(pROC)  # For ROC curve
library(effectsize)
library(ggpubr)
library(broom)  # For tidy model output

# Linear Regression: Predicting Reaction Time 
rt_model <- lm(response_key.rt ~ AQ_Par * AQ_intvwee * expName, data = df)
summary(rt_model)

# Plot reaction time effects
plot1 = ggplot(df, aes(x = expName, y = response_key.rt, fill = AQ_Par)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~ AQ_intvwee) +
  theme_minimal() +
  labs(title = "RT by Experiment and Intervieew AQ Levels", 
       y = "Reaction Time (s)", x = "Intervieew AQ Levels")

#Interpreting the results:
#This model examines how the participant's AQ level (AQ_Par), the interviewee's
#AQ level (AQ_intvwee), and the experimental condition (expName) influence rts.

#The intercept is 1.60, meaning that for low-AQ participants with low-AQ 
#interviewees in the "classify_interact" experiment, the predicted response time
#is approximately 1.60 seconds.

#AQ_Par High interaction with expName Only_eyes:
#This interaction term (0.366) is statistically significant (p = 0.0304), indicating 
#that high-AQ participants take longer to respond in the "Only_eyes" condition 
#compared to low-AQ participants.

#AQ_intvwee High interaction with expName Only_eyes:
#The interaction between high-AQ interviewees and the "Only_eyes" condition (0.396)
#is also statistically significant (p = 0.0235). High-AQ interviewees increase 
#participants' reaction time more in the "Only_eyes" condition compared to low-AQ 
#interviewees.

#Other interactions and main effects are not significant, indicating that these 
#variables (participant’s AQ, experimental condition, etc.) do not have a substantial 
#impact on response time.

#Conclusion:
#High-AQ participants tend to respond more slowly in the "Only_eyes" condition, 
#particularly when paired with high-AQ interviewees. 


# Logistic Regression: Predicting Accuracy (Correct Response):
acc_model <- glm(response_key.corr ~ AQ_Par * AQ_intvwee * expName, family = binomial, data = df)
summary(acc_model)

# Calculate odds ratios
odds_ratios <- exp(cbind(OR = coef(acc_model), confint(acc_model)))
print(odds_ratios)

# Plot accuracy
plot2 = ggplot(df, aes(x = expName, y = response_key.corr, fill = AQ_Par)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", alpha = 0.6) +
  facet_wrap(~ AQ_intvwee) +
  theme_minimal() +
  labs(title = "Accuracy by Experiment and Intervieew AQ Levels", 
       y = "Proportion Correct", x = "Intervieew AQ Levels")

#Interpreting the results:
#This model investigates how the participant's AQ level (AQ_Par), the interviewee's 
#AQ level (AQ_intvwee), and the experimental condition (expName) influence the 
#likelihood of a correct response (response_key.corr). The outcome is binary 
#(correct or incorrect), and the coefficients represent log-odds.

#The intercept (-0.434) is statistically significant (p < 0.001), indicating that 
#the log-odds of a correct response are negative for low-AQ participants with 
#low-AQ interviewees in the "classify_interact" condition. This corresponds to a 
#predicted probability of less than 50% for a correct response in this reference group.

#AQ_intvwee High interaction with expName Only_eyes:
# This interaction term (-0.470) is statistically significant (p = 0.0158), 
#suggesting that high-AQ interviewees reduce the likelihood of correct responses 
#in the "Only_eyes" condition compared to low-AQ interviewees. This is a notable 
#finding, as it indicates that participants struggle more to correctly classify 
#responses when high-AQ interviewees are shown in this condition.

#Coefficients for other main effects and interactions are not significant, indicating 
#no substantial effect on the likelihood of a correct response.

#Overall Model:
#The AIC (7608) indicate the model's goodness of fit. The model shows some 
#statistically significant interaction effects, but the overall fit is modest, 
#and it does not fully explain the variability in correct responses.

#Conclusion:
#The presence of high-AQ interviewees seems to influence the likelihood of correct 
#responses, especially in the "Only_eyes" condition, where it reduces accuracy. 
#However, the individual AQ level of participants does not appear to have a strong 
#effect on performance. Significant interactions suggest that different experimental 
#conditions and the AQ level of interviewees play a more critical role in predicting accuracy.


# Compute sensitivity, precision, F1-score per group:
df_metrics <- df |>
  group_by(AQ_Par, AQ_intvwee, expName) |>
  summarise(
    Sensitivity = sum(response_key.corr == 1) / n(),
    Precision = sum(response_key.corr == 1) / sum(response_key.corr == 1 | response_key.corr == 0),
    F1 = 2 * (Precision * Sensitivity) / (Precision + Sensitivity)
  ) |>
  pivot_longer(cols = c(Sensitivity, Precision, F1), names_to = "Metric", values_to = "Value")

# Plot classification metrics
plot3 = ggplot(df_metrics, aes(x = expName, y = Value, fill = AQ_Par)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.6) +
  facet_grid(Metric ~ AQ_intvwee) +
  theme_minimal() +
  labs(title = "Classification Metrics by Experiment and Interviewees AQ Levels",
       y = "Value", x = "Interviewee AQ Level")

# ROC Curve: 
roc_curve <- roc(df$response_key.corr, fitted(acc_model))

plot4 = ggplot(data.frame(FPR = roc_curve$specificities, TPR = roc_curve$sensitivities),
       aes(x = 1 - FPR, y = TPR)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve for Logistic Regression",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal()

#Interpreting the results:
#AUC value: 0.5282
#The AUC value is very close to 0.5, suggesting the model is performing nearly as 
#well as random guessing in discriminating between correct and incorrect responses.
#This indicates that the model has a low discriminative power, and further 
#improvements are needed for better classification.


Final_p2 = (plot1 | plot2) / (plot3 | plot4)
print(Final_p2)








