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
#loading data:
setwd("~/Documents/2nd_Degree/Courses/R course/FA_SZ")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggdist)
library(patchwork)

df <- read.csv("all_data.csv")
df <- df |>
  filter(!is.na(AQ_Par)) |>
  select(participant, action, action_code, response_key.keys, response_key.corr, 
         response_key.rt, intvwee, expName, AQ_Par, AQ_intvwee) |>
  mutate(Q_Par = factor(AQ_Par, levels = c(0, 1), labels = c("Low", "High")),
         AQ_intvwee = factor(AQ_intvwee, levels = c(0, 1), labels = c("Low", "High")),
         expName = factor(expName, levels = c("calssify_interact", "calssify_interact_onlyeyes", "calssify_interact_noeyes"), 
                          labels = c("classify_interact", "Only_eyes", "No_eyes")),
         action = factor(action, levels = c('Tap', 'Listen', 'Solve'), labels = c('Wait', 'Listen', 'Think'))
         )

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

#Library we haven't learned:
library(datawizard)  # New package for descriptive statistics we didn't learn about.

# Function to calculate summary measures using describe_distribution()
calculate_summary <- function(df) {
  action_code_target <- 2  # Define the target action code for "Think"
  
  df |>
    group_by(expName) |>
    summarise(
      number = n(),
      accuracy_stats = list(describe_distribution((action_code == action_code_target & response_key.keys == action_code_target) |
                                                    (action_code != action_code_target & response_key.keys != action_code_target))),
      n_think_stats = list(describe_distribution(response_key.keys == action_code, na.rm = TRUE)),
      n_choseThink_stats = list(describe_distribution(response_key.keys[action == "Think"] == action_code_target, na.rm = TRUE)),
      sensitivity = ifelse(sum(action_code == action_code_target) > 0,
                           mean(action_code == action_code_target & response_key.keys == action_code_target) / 
                             mean(action_code == action_code_target), 0),
      precision = ifelse(sum(response_key.keys == action_code_target) > 0,
                         mean(action_code == action_code_target & response_key.keys == action_code_target) / 
                           mean(response_key.keys == action_code_target), 0),
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
