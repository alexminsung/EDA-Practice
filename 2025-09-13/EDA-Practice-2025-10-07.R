# HR Dataset - 2025-09-13 Project - cont'd 2025-10-07
# Credit to https://rforhr.com/kfold.html

# Packages used in the project
library(tidyverse)
library(caret)

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

# Set random seed for subsequent random selection and assignment operations
set.seed(1999)

# Partition data and create index matrix of selected values
index <- createDataPartition(hr_truncated$PerformanceScore, p=.8, list=FALSE, times=1)

# Create test and train data frames
train_df <- hr_truncated[index,]
test_df <- hr_truncated[-index,]

# Re-label values of outcome variable for train_df
train_df$PerformanceScore[train_df$PerformanceScore==1] <- "unsatisfactory"
train_df$PerformanceScore[train_df$PerformanceScore==0] <- "satisfactory"

# Re-label values of outcome variable for test_df
test_df$PerformanceScore[test_df$PerformanceScore==1] <- "unsatisfactory"
test_df$PerformanceScore[test_df$PerformanceScore==0] <- "satisfactory"

# Convert the outcome variable into factor to allow interpreting outcomes easier 
train_df$PerformanceScore <- as.factor(train_df$PerformanceScore)
test_df$PerformanceScore <- as.factor(test_df$PerformanceScore)


# Specify the type of training method used (Cross-Validation) and the number of (k = 10) folds
ctrlspecs <- trainControl(method="cv", 
                          number=10, 
                          savePredictions="all",
                          classProbs=TRUE)

# Set random seed for subsequent random selection and assignment operations
set.seed(1999)

# Specify logistic regression model to be estimated using training data
# and k-fold cross-validation process
model1 <- train(PerformanceScore ~ EngagementSurvey + EmpSatisfaction + Absences, data=train_df, 
                method="glm", 
                family=binomial, 
                trControl=ctrlspecs)

# Print information about model
print(model1)

# Based on the information provided, the model is 97% accurate with Cohen's Kappe value of 0.7 which is considered
# Substantial. 

# Print results of final model estimated using training data
summary(model1)

# Coefficients of the trained model remains consistent with the base model using all the data (from the previous practice 2025-10-01).
# Estimate the importance of different predictors
varImp(model1)

# Predict outcome using model from training data based on testing data
predictions <- predict(model1, newdata=test_df)

# Create confusion matrix to assess model fit/performance on test data
confusionMatrix(data=predictions, test_df$PerformanceScore)

# The overall outcome when faced with the test/validation data appear relatively accurate (93%) with Substantial Kappa value (0.7)
# With 95% confidence, we can say that the accuracy will lie between 84% to 98%. 
# However, the p-value is greater than 0.05, which suggests the model is not statistically significant.
# This may mean that the model we created is no better at predicting than the null hypothesis (NIR - where no predictors are provided).
# This may suggest the model is not suitable and changes need to be made (e.g., potential unbalanced dataset)