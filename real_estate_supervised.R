# project group about usa real estate dataset

## 1. Load required libraries
library(readr)
library(lattice)
library(ggplot2)
library(caret)
library(ROCR)
library(nnet)
library(randomForest)
library(e1071)
library(MASS)
library(mlbench)
library(plyr)
library(dplyr)
library(car)
library(PerformanceAnalytics)
library(corrplot)
library(tidyverse)

## 2. Load and Preprocess the dataset

# Load the dataset
real_estate_dataset <- read_csv("/Users/stefanialavarda/Desktop/group_project/realtor.data.zip.csv")

# Sample 2400 rows and remove unnecessary columns
set.seed(123)
data_house <- real_estate_dataset %>% 
  sample_n(2400) %>% 
  select(-brokered_by, -zip_code, -state, -city, -street)

# Remove missing values
data_house <- na.omit(data_house) 

View(data_house)
str(data_house)
summary(data_house)

# Convert categorical variables to factors
data_house <- data_house %>%
  mutate(
    status = as.factor(status),
    city = as.factor(city),
    state = as.factor(state)
  )

# Convert prev_sold_date to binary 
data_house <- data_house %>%
  mutate(prev_sold_date = as.Date(prev_sold_date)) %>%  
  mutate(estate_periods = case_when(
    prev_sold_date >= as.Date("1960-01-01") & prev_sold_date < as.Date("2000-01-01") ~ "Beforo 00s",
    prev_sold_date >= as.Date("2000-01-01") & prev_sold_date < as.Date("2008-01-01") ~ "Pre-Recession",
    prev_sold_date >= as.Date("2008-01-01") & prev_sold_date < as.Date("2014-01-01") ~ "Recession",
    prev_sold_date >= as.Date("2014-01-01") ~ "Post Recession"
  ))

data_house <- data_house %>% select(-prev_sold_date)

data_house$estate_periods <- as.factor(data_house$estate_periods)

# Convert price to binary (target variable)
median_price <- median(data_house$price, na.rm = TRUE)
data_house <- data_house %>%
  mutate(price_binary = ifelse(price <= median_price, "low", "high"))

data_house$price_binary <- as.factor(data_house$price_binary)

data_house <- data_house %>% select(-price)

## 3. Exploratory Data Analysis 
summary(data_house)

# Count of high and low price houses
table(data_house$price_binary) 

# Visualization categorical variables

# Visualization numerical variables

## 4. Correlation Analysis
# Compute correlation matrix for numerical variables
my_data <- data_house[sapply(data_house, is.numeric)]
cor_matrix <- cor(my_data)
round(cor_matrix, 2)

scatterplotMatrix(my_data, regLine = TRUE)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = cor_matrix, col = col, symm = TRUE, Colv = NA, Rowv = NA)

corrplot(cor_matrix, type = "upper", 
         tl.col = "black", tl.srt = 45)

chart.Correlation(my_data, histogram=TRUE, pch=19)

## 5. Split Dataset into Training and Testing Sets
set.seed(123)  
train_index <- createDataPartition(data_house$price_binary, p = 0.6, list = FALSE)
train_data <- data_house[train_index, ]
test_data  <- data_house[-train_index, ]

# Train models
model_rf <- train(price_binary ~ ., data = train_data, method = "rf")
pred_rf  <- predict(model_rf, test_data)

model_svm <- train(price_binary ~ ., data = train_data, method = "svmLinear")
pred_svm  <- predict(model_svm, test_data)

model_nn <- train(price_binary ~ ., data = train_data, method = "nnet", trace = FALSE, linout = FALSE)
pred_nn  <- predict(model_nn, test_data)

model_logistic <- train(price_binary ~ ., data = train_data, method = "glm", family = "binomial")
pred_logistic  <- predict(model_logistic, test_data)

# Confusion matrices
cat("\nConfusion Matrix - SVM\n")
print(confusionMatrix(test_data$price_binary, pred_svm))

cat("\nConfusion Matrix - Random Forest\n")
print(confusionMatrix(test_data$price_binary, pred_rf))

cat("\nConfusion Matrix - Neural Network\n")
print(confusionMatrix(test_data$price_binary, pred_nn))

cat("\nConfusion Matrix - Logistic Regression\n")
print(confusionMatrix(test_data$price_binary, pred_logistic))

# ROC curves
pred_svm_rocr <- prediction(as.numeric(pred_svm), as.numeric(test_data$price_binary))
pred_rf_rocr  <- prediction(as.numeric(pred_rf), as.numeric(test_data$price_binary))
pred_nn_rocr  <- prediction(as.numeric(pred_nn), as.numeric(test_data$price_binary))
pred_logistic_rocr <- prediction(as.numeric(pred_logistic), as.numeric(test_data$price_binary))

roc_svm_perf <- performance(pred_svm_rocr, measure = "tpr", x.measure = "fpr")
roc_rf_perf  <- performance(pred_rf_rocr, measure = "tpr", x.measure = "fpr")
roc_nn_perf  <- performance(pred_nn_rocr, measure = "tpr", x.measure = "fpr")
roc_logistic_perf <- performance(pred_logistic_rocr, measure = "tpr", x.measure = "fpr")

roc_svm_df <- data.frame(FPR = unlist(roc_svm_perf@x.values),
                         TPR = unlist(roc_svm_perf@y.values),
                         Model = "SVM")

roc_rf_df  <- data.frame(FPR = unlist(roc_rf_perf@x.values),
                         TPR = unlist(roc_rf_perf@y.values),
                         Model = "Random Forest")

roc_nn_df  <- data.frame(FPR = unlist(roc_nn_perf@x.values),
                         TPR = unlist(roc_nn_perf@y.values),
                         Model = "Neural Network")

roc_logistic_df <- data.frame(FPR = unlist(roc_logistic_perf@x.values),
                              TPR = unlist(roc_logistic_perf@y.values),
                              Model = "Logistic Regression")

roc_data <- rbind(roc_svm_df, roc_rf_df, roc_nn_df, roc_logistic_df)

# Plot ROC curves
ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1) +
  geom_abline(linetype = "dashed", color = "red") +
  theme_bw() +
  labs(title = "ROC Curve Comparison - Heart Disease Data", 
       x = "False Positive Rate", 
       y = "True Positive Rate", 
       color = "Models") +
  theme(plot.title = element_text(hjust = 0.5))

## 6. Model
