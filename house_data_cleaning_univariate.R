# Housing Project - Univariate Analysis and loaded dataset

library(broom)
library(car)
library(caret)
library(cluster)
library(corrplot)
library(estimatr)
library(factoextra)
library(ggpubr)
library(glmnet)
library(gplots)
library(Hmisc)
library(leaps)
library(lmtest)
library(MASS)
library("party")
library("partykit")
library(pls)
library(plyr)
library(plotly)
library(randomForest)
library(rpart)
library(rpart.plot)
library(olsrr)
library(tree)
library(tidyverse)
library(sandwich)
library("PerformanceAnalytics")

# https://www.kaggle.com/datasets/shibumohapatra/house-price

house_data <- read_csv('1553768847-housing.csv')
View(house_data)
options(scipen = 999)  # Reduces scientific notation

# Data cleaning and re-labeling

house_data <- house_data %>% dplyr::select(-c(longitude, latitude))

house_data <- house_data[!is.na(house_data$total_bedrooms),] # took out the null values

house_data$ocean_proximity <- as.factor(house_data$ocean_proximity)

y <- house_data$median_house_value
x <- house_data$ocean_proximity
m <- house_data$median_income

house_data$log_median_value <- log(y)
logy <- house_data$log_median_value
house_data$log_income <- log(m)
logm <- house_data$log_income
house_data <- house_data %>% mutate(house_tier = case_when(median_house_value <= 1e+05 ~ 'Low price', median_house_value>1e+05 & median_house_value <= 3e+05 ~ 'Medium Price', median_house_value > 3e+05 ~ 'High Price'))
z <- house_data$house_tier

house <- house_data %>% dplyr:: select(-c(house_tier))


# Univariate analysis: 

summary(house_data)
# median age is tight, relatively new buildings all around
# total rooms, bedrooms, and households seem skewed, with possible great outliers
# ocean proximity is very unbalanced, only 5 houses in the islands, and most are at a medium distance from the ocean or far away from it. Always keep in mind the possibility of residential neighborhoods.
# Median Income is for skewed, obviously

# Histogram of target variable:

hist(y, main = 'House Value', xlab = 'Dollars', col = 'lightpink')
boxplot(y, main = 'House Value', col = 'lightpink')
t.test(y, conf.level = 0.95)$conf.int
qqnorm(y) # it seems as if 500k is the max median value of a house, possibly a shortcoming of the dataset. but there seems to be an upper bound.
ggdensity(house_data, x = "median_house_value", fill = "lightgray", title = "house value") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

# 200k is the sample average of the median value of a house in a given block in the California region
# A quick trip on google says the median hosue price in california in 1990 is 194k, so I'd say this checks out.
# Few outliers of houses that are clearly of superior value because they come from select neighborhoods where incomes are super high.

#set.seed(123)
#house_data <- house_data %>% sample_n(5000)
#y <- house_data$median_house_value
shapiro.test(y[0:5000])
# actually a decent shapiro p-value



qqnorm(logy)
shapiro.test(logy[0:5000]) # much closer to one
ggqqplot(house_data$log_median_value) # it's much better, almost perfect but there's this weird upperbound

ggdensity(house_data, x = "log(median_house_value)", fill = "lightgray", title = "log(house value)") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

# Rest of univariate variables

# Ocean Proximity

table(house_data$ocean_proximity)
par(mar = c(6, 4, 4, 2))  # Bottom, left, top, right margins
barplot(table(house_data$ocean_proximity), col = c('lightyellow', 'lightblue', 'darkred', 'lightgreen', 'lightpink' ), cex.names = .6, cex.axis = .8) # same conclusions from summary, possibly an initial description.
# Relationship with target variable: does the price of the median house depend on its proximity to the ocean?
# divide house median value in 3 sectors


# House Tier

house_data <- house_data %>% mutate(house_tier = case_when(median_house_value <= 1e+05 ~ 'Low price', median_house_value>1e+05 & median_house_value <= 3e+05 ~ 'Medium Price', median_house_value > 3e+05 ~ 'High Price'))
z <- house_data$house_tier

hist(logy, main = 'Logarithm of Median House Value', xlab = 'Dollars', col = 'lightpink')
house_data <- house_data %>% mutate(house_tier_log = case_when(log_median_value <= 11.75 ~ 'Low price', median_house_value>11.75 & log_median_value <= 12.5 ~ 'Medium Price', log_median_value > 12.55 ~ 'High Price'))
z_2<-house_data$house_tier_log


table(z, x)
plot(table(z, x), col = c('lightyellow', 'lightblue', 'darkred', 'lightgreen', 'lightpink' ), cex.axis =.3)
plot(t(table(z, x)), col = c('lightyellow', 'lightblue', 'azure', 'lightgreen', 'lightpink' ), cex.axis =.4) # clearer,  could split the tiers further to see where everything is. It's an intuitive dataset after all.
margin.table(table(z, x), 1)
margin.table(table(z, x), 2)

round(prop.table(table(z, x)), 4) # percentages for the whole table
round(prop.table(table(z, x), 1), 4) # row percentage
round(prop.table(table(z, x), 2), 4) # column percentage

boxplot(y ~ x, xlab = "Position", ylab="House Value", col="gold", cex.axis=.5)
plotmeans(y ~ x, xlab = "Position", ylab="House Value", cex.axis=.6)

Chi_Test <- chisq.test(table(z, x), correct=FALSE)
Chi_Test 
# 5532 with a very low p-value. Meaning that the values differ a lot from the expected values. that means there is a strong relationship between the position and the placing of the house.

O = data.frame(x, y) # House value and position
ddply(D,~x,summarise,mean=mean(y),sd=sd(y),n=length(y))
summary(aov(y ~ x, data=O)) # additional, more precise information

# same thing but with log value, for conciseness, clarity and coherence

table(z_2)
table(z_2, x)
plot(t(table(z_2, x)), col = c('lightyellow', 'lightblue', 'azure', 'lightgreen', 'lightpink' ), cex.axis =.4)

boxplot(logy ~ x, xlab = "Position", ylab="House Value", col="gold", cex.axis=.5)
plotmeans(logy ~ x, xlab = "Position", ylab="House Value", cex.axis=.6) # sharper than with non-logged value

Chi_Test <- chisq.test(table(z_2, x), correct=FALSE)
Chi_Test  # even more significant than before somehow

# Median Income

hist(m, main = 'Median Income', xlab = 'Thousands of Dollars', col = 'chartreuse')
qqnorm(m) # some people with 0 income might be wrong, whole row might have to be deleted, could cause problems with the logarithm, showing negative values
qqnorm(logm)

ggdensity(house_data, x = "median_income", fill = "lightgray", title = "median income") +
  stat_overlay_normal_density(color = "red", linetype = "dashed") # Of course. Classic income curve.

ggdensity(house_data, x = "log(median_income)", fill = "lightgray", title = "log(income)") +
  stat_overlay_normal_density(color = "red", linetype = "dashed") # It's perfect, just has negative values
 
# Splitting it into tiers:
house_data <- house_data %>% mutate(income_divide = case_when(median_income <= 5 ~ 'Low Income', median_income > 5 ~ 'High Income'))
d <- house_data$income_divide

table(d)
as.factor(d)
boxplot(y ~ d, xlab = "Income", ylab="House Value", col="peachpuff")
plotmeans(y ~ d, xlab = "Income", ylab="House Value")

# Scaled densities for the incomes, they match almost perfectly with only a slight lag
ggplot() +
  geom_density(data = house_data, aes(x = scale(log_median_value), color = "Median House Value", fill = "Median House Value"), alpha = 0.4, linewidth = 1) +
  geom_density(data = house_data, aes(x = scale(log_income), color = "Median Income", fill = "Median Income"), alpha = 0.4, linewidth = 1) +
  theme_minimal() +
  labs(title = "Density Plot: Median House Value vs. Median Income", 
       x = "Value", y = "Density") +
  scale_fill_manual(name = "Variable", values = c("Median House Value" = "#1f77b4", "Median Income" = "#ff7f0e")) +
  scale_color_manual(name = "Variable", values = c("Median House Value" = "#1f77b4", "Median Income" = "#ff7f0e")) +
  theme(legend.position = "top")


# Check the big correlation matrixes and bivariate ananlysis then get into the more specific specific regressions
plot(m, logy)
house_numeric <- house_data %>% dplyr:: select(median_house_value, housing_median_age, households, total_rooms, total_bedrooms, population, log_income)

correlations_house = round(cor(house_numeric),2) 
symnum(correlations_house, abbr.colnames = FALSE)
pairs(house_numeric, data = house_numeric)
scatterplotMatrix(house_numeric, regLine = TRUE)
?heatmap

heatmap(x = correlations_house, symm = TRUE, Colv = NA, Rowv = NA, cexRow = 0.7, cexCol = 0.55)

corrplot(correlations_house, type = 'upper', 
         tl.col = "black", tl.srt = 45) # now it works. 

chart.Correlation(house_numeric, histogram=TRUE, pch=19) # I mean... House Value is strongly correlated with log income... that's kind of that. there's nothing else to it really. and it makes perfect sense...

