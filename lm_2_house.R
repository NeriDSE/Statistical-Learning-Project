# Linear Regressions on subset dataset

# Data Subsetting
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

# Linear Regressions with caret (includes cv):

# linear-linear
mod <- train(
  median_house_value~median_income,
  data=train_lm,
  method = 'lm',
  trControl = lm_cv
)

summary(mod)
trainControl(summaryFunction = mod)
coef(mod$finalModel)
# RMSE      Rsquared   MAE     
# 82801.36  0.4967056  62167.05

# With log, 3 models:
# log - linear

mod2 <- train(log(median_house_value)~median_income,
             data=train_lm,
             method = 'lm',
             trControl = lm_cv
)

summary(mod2)
trainControl(summaryFunction = mod2)
coef(mod2$finalModel)

# Conversion of RMSE and MAE
predictions_mod_loglog<- predict( mod2, newdata = test_lm )
pred_loglog_scaled<- exp(predictions_mod_loglog)
pred_loglog_scaled
rmse_log <- postResample(pred_loglog_scaled, test_lm$median_house_value) 
rmse_log

#   RMSE        Rsquared     MAE      
#  123554.51   0.3196007   69939.75

# linear - log

mod3 <- train(median_house_value~log(median_income),
              data=train_lm,
              method = 'lm',
              trControl = lm_cv
)

summary(mod3)
coef(mod3$finalModel)
trainControl(summaryFunction = mod3)
# RMSE      Rsquared     MAE     
# 86516.72  0.452279    66582.12

# log - log:

mod4 <- train(log(median_house_value)~log(median_income),
              data=train_lm,
              method = 'lm',
              trControl = lm_cv
)

summary(mod4)
coef(mod4$finalModel)
trainControl(summaryFunction = mod4)

# RMSE       R squared    MAE      
# 0.41         0.47     0.32

mod5 <- train(median_house_value~log(median_income) + I(log(median_income)^2),
              data=train_lm,
              method = 'lm',
              trControl = lm_cv
)

summary(mod5)
coef(mod5$finalModel)
trainControl(summaryFunction = mod5)

#  RMSE       Rsquared       MAE     
# 82011.03      0.50       61831.63

# Full Linear Regression, with all relevant variables

mod_full <- train(median_house_value~. -log_median_value -log_income,
              data=train_lm,
              method = 'lm',
              trControl = lm_cv
)

mod_full <- train(median_house_value~. -log_value -log_income -log_bedrooms,
                  data=train_lm,
                  method = 'lm',
                  trControl = lm_cv
)
summary(mod_full)
coef(mod_full$finalModel)
trainControl(summaryFunction = mod_full)


#  RMSE      Rsquared    MAE     
# 69067.55   0.651466  49788.65

# Collinearity check: 

varImp(mod_full) 
# only income, proximity INLAND, population and age matter based on the absolute value of the t-squared

vif(mod_full$finalModel)
sqrt(vif(mod_full$finalModel)) > 2 
# As predicted households, total_rooms and bedrooms are  collinear, especially households and bedrooms

# on test
predictions_mod_full<- predict( mod_full, newdata = test_lm )
metrics_full <- postResample( pred = predictions_mod_full, obs =test_lm$median_house_value )
metrics_full

# RMSE        Rsquared    MAE 
# 68697.85    0.6535656   50121.83   (test results)

train_lm$ocean_proximity <- relevel(train_lm$ocean_proximity, ref = "INLAND")



mod_improved <- train(median_house_value~. -households -total_rooms -population -log_value -log_bedrooms -log_income,
                  data=train_lm,
                  method = 'lm',
                  trControl = lm_cv
)

summary(mod_improved)
coef(mod_improved$finalModel)
trainControl(summaryFunction = mod_improved)


varImp(mod_improved)
vif(mod_improved$finalModel)

# RMSE      Rsquared   MAE     
# 72173.07  0.6203239  52894.52


# predictions on the test set:
predictions_mod_improved<- predict( mod_improved, newdata = test_lm )
metrics_improved <- postResample( pred = predictions_mod_improved , obs =test_lm$median_house_value )
metrics_improved

# RMSE       Rsquared    MAE 
# 7.60e+04  5.9406e-01  5.501762e+04 


