
house$log_income <- log(house$median_income)
house$log_bedrooms <- log(house$total_bedrooms)
house$log_value <- log(house$median_house_value)


set.seed(42)
house <- house %>% sample_n(4000)

# Data Splitting:
train_index <- createDataPartition( 
  y <- house$median_house_value,
  p =.66,
  list = FALSE)
train_lm <- house[train_index, ]
test_lm <- house[-train_index, ]

# Resampling (k-folds cross validation):

lm_cv <- trainControl(method = 'cv', number = 10)

mod_loglog <- train(log_value~. -median_house_value -median_income -households -population -total_bedrooms -total_rooms,
                    data=train_lm,
                    method = 'lm',
                    trControl = lm_cv
)
trainControl(summaryFunction = mod_loglog)
summary(mod_loglog)

# Get the predictions and actual values
log_predictions <- predict(mod_loglog, newdata = train_lm)  # Log scale predictions
log_observations <- train_lm$log_value  # Actual target values on the log scale

# Exponentiate the predictions and observations to get them on the original scale
predictions_original_scale <- exp(log_predictions)
observations_original_scale <- exp(log_observations)

# Calculate RMSE on the original scale
rmse_original_scale <- sqrt(mean((predictions_original_scale - observations_original_scale)^2))

# Print the RMSE on the original scale
print(rmse_original_scale)

final_model <- mod_loglog$finalModel



ggqqplot(residuals(final_model)) # mostly normally distributed
train_lm$fit<-fitted.values(final_model)
train_lm$res<-residuals(final_model)
ggdensity(train_lm, x = "res", fill = "lightgray", title = "Residuals") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ncvTest(final_model) # a higher p value, but the null is still rejected
spreadLevelPlot(final_model) # improved

reg.robust<-lm_robust(log_value ~ . -median_house_value -median_income -households -population -total_bedrooms -total_room, train_lm,se_type = "stata")
summary(reg.robust)



