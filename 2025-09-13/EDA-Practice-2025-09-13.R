# HR Dataset - 2025-09-13 Project

# Model Diagnostic code used from: 
# https://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/


# Packages used in the project
library(tidyverse)
library(broom)
library(gghighlight)
library(logistf)

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

# Predict the probability of suboptimal performance
probabilities <- predict(model, type = "response")
predicted.class <- ifelse(probabilities > 0.5, "does not meet", "meet")
head(predicted.class)


# Testing LR assumptions
predictors <- colnames(hr_truncated)
# Remove non-numerical variables
hr_linearity <- hr_truncated %>% select(-PerformanceScore) %>% mutate(logit = log(probabilities/(1-probabilities))) %>% 
  gather(key = "predictors", value = "predictor.value", -logit)

# Create the scatterplot to check for linear relationship between cont predictors and the logit outcome 
# This assumption is satisfied if the continuous predictors exhibit a linear relationship with the logit outcome
ggplot(hr_linearity, aes(logit,predictor.value)) + 
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# Testing for Influential outliers
# Display diagnostic plot 4, Cook's Distance and label the top 3 outliers
plot(model, which = 4, id.n = 3)

# Not all outliers are influential. Standard residual errors can be used to check if the data contain potential influential obs
# Data points with an absolute standardized residuals above 3 represent possible outliers and may deserve closer attention.

# The following R code computes the standardized residuals (.std.resid) and the Cook’s distance (.cooksd) 
# Using the R function augment() [broom package].

# Extract model results
model.data <- augment(model) %>% 
  mutate(index = 1:n()) 

# Plot the standard residuals
# If the plot of the residuals show a random pattern, then this assumption may be satisfied
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = as.character(PerformanceScore)), alpha = .5) +
  theme_bw() + guides(color = guide_legend(title = "Performance Score"))

# Filter potential influential point greater than 3
# Problematic point(s) may be removed from the data. 
model.data %>% 
  filter(abs(.std.resid) > 3)

# Identify the problematic row
model.data %>% 
  mutate(row = row_number()) %>% 
  filter(abs(.std.resid) > 3) %>% select(row) 

# Highlight the point on the previous plot
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = as.character(PerformanceScore)), alpha = .5) +
  theme_bw() + guides(color = guide_legend(title = "Influential Outlier")) + gghighlight(.std.resid > 3)

# Test for multicollinearity
# As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity
# It can be assessed using the R function vif() [car package], which computes the variance inflation factors:
car::vif(model)

# Remove the probablematic row 
hr_v2 <- hr_truncated[-85,]
model2 <- glm(PerformanceScore ~ ., data = hr_v2, family = binomial)

