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
         response_key.rt, intvwee, expName, AQ_Par, AQ_intvwee)

df$AQ_Par <- factor(df$AQ_Par, levels = c(0, 1), labels = c("Low", "High"))
df$AQ_intvwee <- factor(df$AQ_intvwee, levels = c(0, 1), labels = c("Low", "High"))
df$expName <- factor(df$expName)
df$action <- factor(df$action)

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
