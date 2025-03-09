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
