# Final Assignment Repository - Shira Zadok
This repository is for gathering the Final Assignment of R Beginners Course.  

## The reason I chose this dataset:
This data set is the gathered data from an experiment conducted in our lab. This experiment examines the “Gaze Aversion” phenomena and whether people grasp other peoples’ gaze aversion as a thinking process. To manipulate gaze aversion, we asked participants to look at 9 different locations on the screen and filmed their response. We used these videos in three experiments - in one, the videos were shown without changes (we only clipped them to be a few seconds long), in the second, the eyes were cut out, and only the face was shown, and in the third, only the eyes were shown. These participants were the interviewees group. Then, we showed another group of participants the videos and asked them to answer a few questions about the short videos they observed - they were the observers group. So, I chose this data set to assist a colleague with the analysis of the data and also because I find this project interesting and relevant to understanding social interactions better.

## Exploring the data - The graphs:
![ExplorativeData](https://github.com/user-attachments/assets/b7c2d3f7-5188-48a5-8b63-b1d73294b886)

## Research question:
How do AQ levels (High vs. Low) of both the participant (AQ_Par) and the interviewer (AQ_intvwee) influence performance in a classification task across different experimental conditions? Specifically, how do AQ levels predict the likelihood of a correct response, reaction time, sensitivity, precision, F1-score, and accuracy?

## Linear Regression Results Interpretation
The linear regression analysis revealed that the only significant interaction effect was between participants’ high AQ group and the Only_eyes experiment, with an estimated coefficient of 0.36591 (p = 0.0304). This suggests that when the participant has a high AQ and the experimental condition shows only the eyes, there is a negative relationship with the response time, meaning it took participants longer to respond in this experiment compared to others. Additionally, there was a significant interaction between interviewees’ high AQ group and the Only_eyes experiment (p = 0.0235), indicating that the high AQ of the interviewee also increased the response time in the "only eyes" experiment. Other relations were not significant, suggesting that these factors did not notably influence the response time. The overall model explains only a small portion of the variance (R-squared = 0.00469), indicating that other factors, not included in the model, might be contributing to the response time.

## Logistic Regression Results Interpretation
The logistic regression analysis showed that the only significant interaction effect was between the interviewee’s AQ (High) and the experimental condition “Only eyes” (p = 0.0158). This suggests that when the interviewee has a high AQ and the experimental condition shows only the eyes, there is a decrease in the likelihood of a correct response. This implies that the high AQ of the interviewee negatively affects the participant’s accuracy in this condition. The coefficients for other main effects, such as AQ_Par High and expName, and most interaction effects were not significant, indicating that these factors did not significantly impact the probability of a correct response. The model's fit, as indicated by the residual deviance, suggests that the predictors used explain a modest amount of the variability in the classification of responses.

## Classification Metrics: Sensitivity, Precision, and F1-Score Interpretation
The classification metrics (Sensitivity, Precision, and F1-Score) reveal mixed results across different combinations of AQ levels and experimental conditions. For instance, when both the participant (AQ_Par) and interviewee (AQ_intvwee) had low AQ, the "classify_interact" condition showed the highest values for all metrics (Sensitivity = 0.393, Precision = 0.393, F1 = 0.393). In contrast, when both had high AQ, the performance was slightly lower, especially in the "Only_eyes" and "No_eyes" conditions. Notably, the F1-Score, which balances precision and sensitivity, was highest for the "classify_interact" condition, regardless of AQ levels. These results suggest that participants tend to perform better in identifying interactions when both AQ levels are low, and that the inclusion of only the eyes or no eyes at all reduces classification performance, particularly when AQ levels are high.

## ROC Curve and AUC Interpretation
The ROC analysis indicated an AUC of 0.5282, which is very close to 0.5, suggesting that the model has poor discriminative power. The AUC value indicates that the model is performing almost no better than random guessing in classifying correct vs. incorrect responses. This highlights that the model’s ability to correctly differentiate between the two categories (correct vs. incorrect) is minimal. Therefore, using a different model is required to improve the performance in this analysis. 

## Final analysis plots:
![AnalyzedResults](https://github.com/user-attachments/assets/e6e199ae-9165-4285-9b32-e9cd3ef52f02)

