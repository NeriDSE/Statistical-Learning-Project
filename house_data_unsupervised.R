# Housing Project
library(plyr)
library(tidyverse)
library(gplots)
library(corrplot)
library(car)



house_data <- read_csv('1553768847-housing.csv')
View(house_data)

# cleaning

house_data <- house_data %>% select(-c(longitude, latitude))

house_data <- house_data[!is.na(house_data$total_bedrooms),] # took out the null values

# Some univariate analysis

summary(house_data)

y <- house_data$median_house_value
hist(y, main = 'House Value', xlab = 'Dollars', col = 'lightpink')
boxplot(y, main = 'House Value', col = 'lightpink')
t.test(y, conf.level = 0.95)$conf.int

x <- house_data$ocean_proximity
table(house_data$ocean_proximity)
par(mar = c(6, 4, 4, 2))  # Bottom, left, top, right margins
barplot(table(house_data$ocean_proximity), col = c('lightyellow', 'lightblue', 'darkred', 'lightgreen', 'lightpink' ), cex.names = .6, cex.axis = .8)

# Question 1: Does price of the house depend on where the house is?
# divide the house prices in 3 sectors

house_data <- house_data %>% mutate(house_tier = case_when(median_house_value <= 1e+05 ~ 'Low price', median_house_value>1e+05 & median_house_value <= 3e+05 ~ 'Medium Price', median_house_value > 3e+05 ~ 'High Price'))
z <- house_data$house_tier


table(z, x)
plot(table(z, x), col = c('lightyellow', 'lightblue', 'darkred', 'lightgreen', 'lightpink' ), cex.axis =.3)
plot(t(table(z, x)), col = c('lightyellow', 'lightblue', 'darkred', 'lightgreen', 'lightpink' ), cex.axis =.5) # clearer
margin.table(table(z, x), 1)
margin.table(table(z, x), 2)

options(scipen = 999)  # Reduces scientific notation

round(prop.table(table(z, x)), 4) # percentages for the whole table
round(prop.table(table(z, x), 1), 4) # row percentage
round(prop.table(table(z, x), 2), 4) # column percentage

# Chi square test to check for the independence between the price and the location. if the chi square is h
Chi_Test <- chisq.test(table(z, x), correct=FALSE)
Chi_Test 
# 5532 with a very low p-value. meaning that the values differ a lot from the expected values. that means there is a strong relationship between the position and the placing of the house.

m <- house_data$median_income
hist(m, main = 'Median Income', xlab = 'Thousands of Dollars', col = 'chartreuse')
house_data <- house_data %>% mutate(income_divide = case_when(median_income <= 5 ~ 'Low Income', median_income > 5 ~ 'High Income'))
d <- house_data$income_divide

table(d)
as.factor(d)
boxplot(y ~ d, xlab = "Income", ylab="House Value", col="peachpuff")
boxplot(y ~ x, xlab = "Position", ylab="House Value", col="gold", cex.axis=.5)

plotmeans(y ~ d, xlab = "Income", ylab="House Value")
plotmeans(y ~ x, xlab = "Position", ylab="House Value", cex.axis=.6)

n <- house_data$population
table(y, x)
D = data.frame(x, y) # House value and position
ddply(D,~x,summarise,mean=mean(y),sd=sd(y),n=length(y))
M <- data.frame(y, d) # House value and income
ddply(M,~d,summarise,mean=mean(y),sd=sd(y),n=length(y))


# It's the tables, the chi squared test, the boxplot and the means. very straightforward and simple. You can do these between a categorical variable with a few options and a continuous variable with many options. to do it for age or population i'd have to split that up. so... maybe later.
summary(aov(y ~ x, data=D))
summary(aov(y~ d, data = M))
# huge values, very different means, groups differ a lot, more in the income than the position, with a very significant p value. the means are absurdly high though...

# Does the median house value depend on bathrooms, age, people, income...
# this is for multiple numeric variables

x1 <- house_data$total_rooms
x2 <- house_data$households
x3 <- house_data$population

X <- data.frame(y, m, x1, x2, x3)
C = cor(X)
C
pairs(~y+m+x1+x2+x3,data=X)
corrplot(C, method = 'number')
corrplot(C, method = 'color', order = 'alphabet') # + other methods, the og just has circles
# not a whole lot of correlation between value and the other variables besides value and income. I'd try with other variables too.

# linear regression 
mod<-lm(y~m+x1+x2+x3, data=X)
summary(mod)
# m is crazy as predicted and so is number of nearby households but in a positive way

# Then we check Collinearity
vif(mod)
# x1 and x2 have medium high to high collinearity, they're probably channeling other variables 

par(mfcol = c(1, 4))
plot(x1, y, xlab = "Total Rooms", 
     main = "House Value", pch = 15)
# add the interpolating line
abline(lm(y~x1), col = "red") 

plot(x2, y, xlab = "Houses in the neighborhood", 
     main = "House Value", pch = 15)
# aggiungi la retta interpolante
abline(lm(y~x2), col = "red") 

plot(x3, y, xlab = "Neighborhood Population", 
     main = "House Value", pch = 15)
# aggiungi la retta interpolante
abline(lm(y~x3), col = "red") 

plot(m, y, xlab = "Income", 
     main = "House Value", pch = 15)
# aggiungi la retta interpolante
abline(lm(y~m), col = "red") 

# I would like to try with some more variables but that may be a job for the supervised/regression part. I want to focus on the unsupervised part now, starting with PCA. let's go.

# Hierarchical Clustering
# First, what would you cluster. I could cluster income groups. I want to try that, although I need to make the buckets first, so for now I could cluster based on the position. 5 buckets... not the best not the worst but cool. what houses are where etc etc. then I can move on to try other stuff. I want to read on this btw. easier when you have specific groups you cluster by 
# why does she do pca after this? 

# Numeric dataset: 
house_data$ocean_proximity <- as.character(house_data$ocean_proximity)


house_for_cluster <- house_data %>% select_if(is.numeric)
house_for_cluster_scaled <- scale(house_for_cluster)
distance <- dist(house_for_cluster_scaled)

set.seed(42)

# she made this function to set up the agglomeration for hierarchical clustering
agglo <- function(hc){
  data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0, 
                               hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE) }
h1<-hclust(distance, method="average")
agglo(h1)

# I have to choose better data. this may not lend itself to this job very easily. ah. shame.
install.packages('rattle')

k.means.fit <- k.means(house_for_cluster_scaled, 3)
str(k.means.fit)

pca<-princomp(house_for_cluster, cor=T)
summary(pca)
screeplot(pca)
abline(a=1, b=0, h= 1, col ='red')
# not even 3