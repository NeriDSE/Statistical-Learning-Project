# Linear Regressions on subset dataset

# Data Subsetting
set.seed(42)
house <- house %>% sample_n(4000)

y <- house$median_house_value
x <- house$ocean_proximity
m <- house$median_income

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
# log - predictor

mod2 <- train(log_median_value~median_income,
             data=train_lm,
             method = 'lm',
             trControl = lm_cv
)

summary(mod2)
trainControl(summaryFunction = mod2)
coef(mod2$finalModel)
#RMSE      Rsquared   MAE      
#0.426019  0.4516735  0.3319836

# y - log

mod3 <- train(median_house_value~log(median_income),
              data=train_lm,
              method = 'lm',
              trControl = lm_cv
)

summary(mod3)
coef(mod3$finalModel)
trainControl(summaryFunction = mod3)
# RMSE      Rsquared  MAE     
# 86516.72  0.452279  66582.12

# log - log:

mod4 <- train(log(median_house_value)~log(median_income),
              data=train_lm,
              method = 'lm',
              trControl = lm_cv
)

summary(mod4)
coef(mod4$finalModel)
trainControl(summaryFunction = mod4)

# RMSE       Rsquared   MAE      
# 0.4159811  0.4779729  0.3231643

mod5 <- train(median_house_value~log(median_income) + I(log(median_income)^2),
              data=train_lm,
              method = 'lm',
              trControl = lm_cv
)

summary(mod5)
coef(mod5$finalModel)
trainControl(summaryFunction = mod5)

# RMSE      Rsquared   MAE     
# 82011.03  0.5077138  61831.63

# Full Linear Regression, with all relevant variables
mod_full <- train(median_house_value~. -log_median_value -log_income + I(log_income)^2,
              data=train_lm,
              method = 'lm',
              trControl = lm_cv
)
summary(mod_full)
coef(mod_full$finalModel)
trainControl(summaryFunction = mod_full)

# RMSE       Rsquared   MAE     
# 70482.73   0.6348176  51066.19, good r squared. (train)


# Trying to take out the collinear and irrelevant variables according to VIF and VarImp

varImp(mod_full) # only income, proximity INLAND, population and age matter, based on the absolute value of the t-squared
vif(mod_full$finalModel)
sqrt(vif(mod_full$finalModel)) > 2 # as predicted households, total_rooms and bedrooms are a mess, especially households and bedrooms

predictions_mod_full<- predict( mod_full, newdata = test_lm )
metrics_full <- postResample( pred = predictions_mod_full, obs =test_lm$median_house_value )
metrics_full

# RMSE        Rsquared    MAE 
# 68697.85    0.6535656   50121.83   (test results)

train_lm$ocean_proximity <- relevel(train_lm$ocean_proximity, ref = "INLAND")
mod_improved <- train(median_house_value~. -log_median_value -log_income -households -total_bedrooms,
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
# 72173.07  0.6203239  52894.52 not too shabby..
# the only issue would be the irrelevance of ISLAND. I may just take it out of the dataset.I have to consult with professors before I do that.

# predictions on the test set:
predictions_mod_improved<- predict( mod_improved, newdata = test_lm )
metrics_improved <- postResample( pred = predictions_mod_improved , obs =test_lm$median_house_value )
metrics_improved

# RMSE      Rsquared  MAE 
# 71530.85 0.6244322  52338.92 (test results)

mod_loglog <- train(log_median_value~. -median_house_value -median_income -households -total_bedrooms,
                      data=train_lm,
                      method = 'lm',
                      trControl = lm_cv
)
trainControl(summaryFunction = mod_loglog)

predictions_mod_loglog<- predict( mod_loglog, newdata = test_lm )

metrics_improved <- postResample( pred = predictions_mod_loglog , obs =test_lm$log_median_value )

metrics_improved
# lower RMSE, higher Rsquared with log on linear. I have to convert.
# even lower with log-log
# terrible with linear on log

# RMSE     Rsquared   
# 71950.54 0.6461951  test for loglog

# log scaling back
pred_loglog_scaled<- exp(predictions_mod_loglog)
pred_loglog_scaled

rmse_log <- postResample(pred_loglog_scaled, test_lm$median_house_value) 
rmse_log
