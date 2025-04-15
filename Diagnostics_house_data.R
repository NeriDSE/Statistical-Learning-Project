# Diagnostics:
final_model <- mod_improved$finalModel


summary(final_model)

# vif: 
vif(final_model)
sqrt(vif(final_model)) > 2


summary(residuals(final_model)) # mean at 0 is good


ggqqplot(residuals(final_model)) # mostly normally distributed

shapiro.test(residuals(final_model)) # residuals are most likely NOT normally distributed, which is a problem

train_lm$fit<-fitted.values(final_model)
train_lm$res<-residuals(final_model)

ggdensity(train_lm, x = "res", fill = "lightgray", title = "Residuals") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")



plot(train_lm$fit, train_lm$res,  xlab= 'Fitted Values', ylab= 'Residuals', main='Residuals vs Fit. Values for Log  Value', cex.main = .8)
# no clear pattern. good.


# Outliers:

# deleted studentized residuals 
ols_plot_resid_stud(final_model)


# studentized residuals 

studentized_residuals <- rstudent(final_model)
plot(final_model$fitted.values, studentized_residuals, 
     xlab = "Fitted Values", ylab = "Studentized Residuals", 
     main = "Studentized Residuals vs Fitted Values")
abline(h = 3, col = "red")  
abline(h = -3, col = "red")  
# does ok with fitted values

ols_plot_resid_stand(final_model) 
# same thing, mostly it's within bounds

ols_plot_resid_stud_fit(final_model)
# Deleted Studentized Residual vs Fitted Values Plot shows many outliers.

# It's plausible there are some inefficiencies, many values have an important impact on the model
# y strange given x

# High Leverage Values
par(cex = .0005)
ols_plot_resid_lev(final_model)

# mostly leverage points it seems. and there's a lot of them 
# x strange given x

par(mfrow=c(1,2))
plot(final_model,4)
plot(final_model,5)
# cook's distance is reassuring. there don't seem to be too many influential data points.
# suggests there's not too much distorsion from the outliers and high leverage points

ols_plot_cooksd_bar(final_model) # 1 outlier
ols_plot_cooksd_chart(final_model)
# mostly ok
# Distribution of all cook's distance for data point (look further into it)


# residuals, fitted values, cook's distance for each point
theme_set(theme_classic())
model.diag.metrics <- augment(final_model)
head(model.diag.metrics)

model.diag.metrics %>%
  top_n(5, wt = .cooksd)


ols_plot_dfbetas(final_model)
# driven by age and distance, not as much income as I thought.
#DFBETA measures the difference in each parameter estimate with and without the influential point. In general, large values of DFBETAS indicate observations that are influential in estimating a given parameter.

ols_plot_dffits(final_model) # difference between fitted value with full data and fitted value by removing each observation. not too many values that have that bi gof a change.

# Robust Regression (since there are extreme and high leverage values)
# Caret isn't working with MASS/rlm. i'll have to try the caretless way

set.seed(42)
house <- house %>% sample_n(4000)


mod_final_rob <- train(median_house_value~.  -population -households -total_rooms,
                    data=train_lm,
                    method = 'rlm',
                    trControl = lm_cv
)
trainControl(summaryFunction = mod_loglog)
predictions_mod_loglog<- predict( mod_loglog, newdata = test_lm )
metrics_improved <- postResample( pred = predictions_mod_loglog , obs =test_lm$log_median_value )
metrics_improved

# Variance of residuals: heteroskedasticity


par(mfrow=c(2,2))
plot(final_model,1)
plot(final_model,5)
plot(train_lm$log_income,train_lm$res,  xlab = 'Median Income', ylab = 'Residuals')
plot(train_lm$median_house_value,train_lm$res, xlab = 'Median House Value', ylab = 'Residuals') 


plot(train_lm$log_rooms,train_lm$res, xlab = 'Total Rooms', ylab = 'Residuals')
plot(train_lm$housing_median_age,train_lm$res, xlab = 'Median Age', ylab = 'Residuals')

plot(train_lm$ocean_proximity ,train_lm$res, xlab = 'Ocean Proximity', ylab = 'Residuals')
# rooms and ocean proximity, could be causing hereoskedasticity, to plot ocean proximity it's lowkey a mess.


# Homoskedasticity:

ncvTest(final_model) # p-value far lower than .05, there is presence of heteroskedasticity in the model


spreadLevelPlot(final_model) # plots studentized residuals vs. fitted values(didn't we already do this?)


# we need to calculate robust standard errors (wrong data set)
set.seed(42)
reg.robust<-lm_robust(median_house_value ~ . -total_rooms -population -households, train_lm,se_type = "stata")
summary(reg.robust)

bptest(final_model) # null rejected...
coeftest(final_model, vcov = vcovHC(final_model, "HC1"))

# Correlation between error and predictors
cor(train_lm[, c('log_income', "log_rooms", 'housing_median_age')], train_lm$res) 
?coeftest

