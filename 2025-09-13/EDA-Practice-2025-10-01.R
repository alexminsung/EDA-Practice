# HR Dataset - 2025-09-13 Project - cont'd 2025-10-01

# Packages used in the project
library(tidyverse)
library(pROC)

# Load the dataset
hr <- read.csv("2025-09-13/HRDataset_v14.csv") 

# Clean the data to include only relevant columns and convert Performance Score to Binary
# Refer to project report for why following columns were selected
# (0 = Meet performance, 1 = Does not meet performance)
hr_truncated <- hr %>% select(PerformanceScore,EngagementSurvey, EmpSatisfaction, Absences) %>%
  mutate(PerformanceScore = case_when(
    grepl("Exceeds", PerformanceScore) ~ 0,
    grepl("Fully Meets", PerformanceScore) ~ 0,
    grepl("Needs Improvement", PerformanceScore) ~ 1,
    grepl("PIP", PerformanceScore) ~ 1
  ))

# Fit the logistic regression model
model <- glm(PerformanceScore ~ ., data = hr_truncated, family = binomial)

# Draw the ROC curve comparing the actual y-values to the predicted/fitted y-values
par(pty = "s")
model.roc <- roc(hr_truncated$PerformanceScore, model$fitted.values, plot = TRUE, legacy.axes = TRUE,
                 percent = TRUE, xlab = "FP Rate", ylab = "TP Rate")

# Using the ROC curve, determine the optimal threshold
optimal.threshold <- coords(model.roc, "best", ret="threshold", transpose = FALSE)
print(optimal.threshold)

# The threshold is identical to the excel produced threshold of 0.4

# Now that optimal threshold is confirmed, repeat the steps above using training/testing data
# Apply Cross Validation and determine model effectiveness