# Linear Regressions on full dataset


# Linear Regression

mod<-lm(y~m, data=house_data)
plot(y~m, col="lightskyblue", pch=19, cex=.1,data=house_data)
abline(mod, col="red3", lwd=3)
summary(mod) # crazy estimate, definetely associated.

house_data$median_income_normal <- house_data$median_income*10000
m2 <- house_data$median_income_normal # just to make the number nicer
s
plot(y~m2, col="lightskyblue", pch=19, cex=.1,data=house_data)
mod2<-lm(y~m2, data=house_data)
abline(mod2, col="red", lwd=3)
summary(mod2)
# yes there's a correlation, it just seems a bit, squared? perhaps

# With log, 3 models:

plot(logy~m, col="lightskyblue", pch=19, cex=.1, data=house_data)
abline(lm(logy~m, data=house_data), col = 'red2', lwd= 1.5)
mod3<- lm(logy~m, data = house_data)
summary(mod3)
# much worse. not as strong. here there's probably the x^2 squared effect which I want to try

plot(y~logm, col="pink", pch=19, cex=.1, data=house_data)
abline(lm(y~logm, data=house_data), col = 'salmon', lwd= 3)
mod4<- lm(y~logm, data = house_data)
summary(mod4)
# an even better estimate, clearly
plot(logy~logm, col='hotpink1', pch=19, cex=.1, data=house_data)
abline(lm(logy~logm, data=house_data), col = 'salmon', lwd= 3)
mod5<- lm(logy~logm, data = house_data)
summary(mod5)
# 
# the linear model is actually the most successful model with an R^2 of .47. Didn't see that coming.


mod6 <- lm(y~ logm + I(logm^2), data = house_data)
summary(mod6) # the linear with the squared is basically the same (.005 better), but the log with the squared has the highest r squared, although Idk why it's positive.

house <- house_data %>% dplyr:: select(-c(log_median_value, house_tier, median_house_value))

# Full Linear Regression, with all relevant variables

mod_full <- lm(y ~., data = house)
summary(mod_full)
# this is pretty good with an r^2 of .63

mod_squared <-lm(logy ~. + I(m^2), data = house)
summary(mod_squared)
# this has the highest R^2 (.67) but it contradicts my earlier claims



vif(mod_full) # as predicted households, total_rooms and bedrooms are a mess, especially households and bedrooms

sqrt(vif(mod_full)) > 2

# I could try to drop the collinear variables and re run everything. That, I think, would be a good first order of business.

mod_noncollinear <- lm(logy~. -households -total_bedrooms - population - median_income, data = house)
# best model, without median_income, but with log income and logy
summary(mod_noncollinear)
# every variable is significant, the R^2 has decreased by .2, not tragic, i'd say.

vif(mod_noncollinear) 
sqrt(vif(mod_noncollinear)) > 2 
# age and rooms get up there a little bit, but they're comparatively much lower scores without bedrooms and households
# without bedrooms and rooms the R^2 is back at .63 but it's still sort of collinear
# without bedrooms, roomds and households there's no collinearity but we get .59 R^2

# this is the best model I've found by playing around by myself so far. Let's check everything else.

mod_noncollinear2 <- lm(house$median_house_value~. -households -total_bedrooms - population - log_income, data = house)
summary(mod_noncollinear2)
# just to have