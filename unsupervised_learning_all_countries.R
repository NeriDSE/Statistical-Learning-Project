# Unsupervised Project - Agricultural Outlook for OECD countries - Statistical Learning
# Every Country, no outlier removed

oecd_rough<- read_csv('oecd_agriculture_all_countries.csv')

oecd_data <- oecd_rough %>% dplyr::select(-c(STRUCTURE, STRUCTURE_ID, STRUCTURE_NAME, ACTION, `Reference area`, FREQ,`Frequency of observation`, COMMODITY, Commodity, MEASURE, UNIT_MEASURE, VERSION_ID, `Version ID`, TIME_PERIOD, `Time period`, `Observation value`, OBS_STATUS, `Observation status`, `Unit multiplier`, DECIMALS, Decimals, CONVENTION, `Agricultural convention`, UNIT_MULT, `Unit of measure`))

oecd_data <- oecd_data %>% pivot_wider(names_from = 'Measure', values_from = 'OBS_VALUE')
oecd_data <- oecd_data %>% select(-`Biodiesel production from commodity`)
oecd_data <- as.data.frame(oecd_data)
rownames(oecd_data)<-oecd_data$REF_AREA
oecd_data<-oecd_data %>% select(-REF_AREA)


zeros_per_column <- apply(oecd_data, 2, function(x) sum(x == 0))
zeros_per_column
oecd_data <- oecd_data %>% select(-c(`Ethanol production from commodity`, `Biofuel use`, `Other use`, `Exports`))
oecd_data <- oecd_data %>% filter(!(row.names(oecd_data) %in% c('IDN', 'VNM', 'MYS', 'PHL')))

oecd_no_outliers <- oecd_data %>% filter(!(row.names(oecd_data) %in% c('EU', 'CHN', 'IND')))
# Without the 3 outliers the PCA is far less compressed. more differentiation between the various countries. Makes no difference with hierarchical clustering.

# There's 0s in Japan (Exports), New Zealand (Other Use), Indonesia Philippines Vietnam and Malaysia (GHG Emissions). Plus the South East Asian countries have entries that are basically 0 (0.0001) that may skew results? who knows.
# Either I take out exports, other use and ghg emissions or I take out those countries. what's better? What's worse? Weighing pros and cons. I can take out other use and exports, since they're well paired with other more sizeable variables such as production and consumption.
# I don't want to take out the emissions. But I definetely think it's a bad idea to take out observations I would rather take out variables

View(oecd_data)
View(oecd_scaled)
View(oecd_scale)

# Unit conversion, convert everything to thousands of tonnes:

oecd_scaled <- oecd_data

oecd_scaled <- oecd_no_outliers

# Production, Consumption, Feed, Imports, Exports, Food, Other use are in already in thousands of tonnes

# Human consumption is in kg, in units
oecd_scaled$`Human consumption` <- oecd_data$`Human consumption`/1000000
# Direct GHG emissions are tons of CO2, which is in the millions
oecd_scaled$`Direct GHG emission` <- oecd_data$`Direct GHG emission`*1000
# Yield is in tonnes per square hectare, units
oecd_scaled$Yield <- oecd_data$Yield/1000
# Food fat availability is in Grammes per person per day in units
oecd_scaled$`Food fat availability` <- oecd_data$`Food fat availability`/1000000000
# Producer price is in National currency per tonne in units
oecd_scaled$`Producer price` <- oecd_data$`Producer price`/1000
# Total calorie and protein availability are in kilocalories
oecd_scaled$`Total calorie availability` <- oecd_data$`Total calorie availability`/1000000
oecd_scaled$`Food protein availability` <- oecd_data$`Food protein availability`/1000000
# Area harvested is in hectares (which I guess is in the thousands)
oecd_scaled$`Area harvested` <- oecd_data$`Area harvested`/1000


# Logging:

oecd_scaled[c("Production", "Feed", 'Area harvested', "Ending stocks", "Producer price", 'Direct GHG emission' )] <- lapply(oecd_scaled[c("Production", "Feed", 'Area harvested',  "Ending stocks", "Producer price", 'Direct GHG emission' )], log)

# function(x) log(x+1e-24)


# oecd_scaled[c("Production", "Exports", "Feed", 'Other use', 'Area harvested', 'Direct GHG emission', "Ending stocks", "Producer price" )] <- lapply(oecd_scaled[c("Production", "Exports", "Feed", 'Other use', 'Area harvested', 'Direct GHG emission', "Ending stocks", "Producer price" )], function(x) log(x+1e-24))

# Then z-scaling:
oecd_scaled <-scale(oecd_scaled)
oecd_scaled<-as.data.frame(oecd_scaled)

# Skewness:

# Very skewed: production, exports, other use, feed, area harvested, ghg emissions, ending stocks, producer price
# Skewed but not as much: imports, yield, human consumption (basically not skewed)
# not skewed: the 3 food indicators

hist(oecd_data$`Total calorie availability`) # No skewness
hist(oecd_data$Yield) # Some skewness
hist(oecd_data$`Production`) # Strong Skewness


ggqqplot(oecd_data$Production)
ggqqplot(oecd_scaled$Production) 
shapiro.test(oecd_data$Production) 
shapiro.test(oecd_scaled$Production)

# ggqqplot(oecd_data$Exports) 
# ggqqplot(oecd_scaled$Exports)
# shapiro.test(oecd_data$Exports)
# shapiro.test(oecd_scaled$Exports)

# ggqqplot(oecd_data$`Other use`)
# ggqqplot(oecd_scaled$`Other use`)
# shapiro.test(oecd_data$`Other use`)
# shapiro.test(oecd_scaled$`Other use`)


ggqqplot(oecd_data$Feed)
ggqqplot(oecd_scaled$Feed)
shapiro.test(oecd_data$Feed)
shapiro.test(oecd_scaled$Feed)

ggqqplot(oecd_data$`Area harvested`)
ggqqplot(oecd_scaled$`Area harvested`)
shapiro.test(oecd_data$`Area harvested`) 
shapiro.test(oecd_scaled$`Area harvested`)


ggqqplot(oecd_data$`Direct GHG emission`)
ggqqplot(oecd_scaled$`Direct GHG emission`)
shapiro.test(oecd_data$`Direct GHG emission`) 
shapiro.test(oecd_scaled$`Direct GHG emission`)


ggqqplot(oecd_data$`Ending stocks`)
ggqqplot(oecd_scaled$`Ending stocks`)
shapiro.test(oecd_data$`Ending stocks`)
shapiro.test(oecd_scaled$`Ending stocks`)

ggqqplot(oecd_data$`Producer price`)
ggqqplot(oecd_scaled$`Producer price`)
shapiro.test(oecd_data$`Producer price`) 
shapiro.test(oecd_scaled$`Producer price`)


# Distances

ds_euc <- dist(scale(oecd_scaled))
ds_man <- dist(scale(oecd_scaled), method  = 'manhattan')

# PCA with unscaled data:
pca_oecd<-princomp(oecd_scaled, cor=T)

summary(pca_oecd)
screeplot(pca_oecd) # 3rd and 4th barely relevant
abline(h=1, col = 'red')

# Variable importance
fviz_contrib(pca_oecd, "var", axes = 1, fill = "forestgreen", color = "forestgreen")
# All of the main balance for wheat, as well as the specifics and the emissions indicator.
fviz_contrib(pca_oecd, "var", axes = 2, fill = "forestgreen", color = "forestgreen")
# All of the other human indicators (NOTE: should they be compared and used here??)
fviz_contrib(pca_oecd, "var", axes = 3, fill = "forestgreen", color = "forestgreen")
# Exports, ending stocks, biofuel use, food, feed, imports, not as clearly labeled, less useful


# plots:
plot(pca_oecd$scores, type = "n")
text(pca_oecd$scores, labels = substr(rownames(oecd_data), 1, 3), cex = 0.5)
abline(h=0, v=0)
## biplot 
biplot(pca_oecd, cex = .4)
# decently well balanced and similar to before in terms of results. What's left to understand is if this is the right strategy and logging everything is a good idea.



# Right off the bat, we can already identify a series of groups
# The larger population and larger landmass producers/consumers with huge balances: India, China, EU, Russia (all possible outliers)
# The smaller and smaller populations, lower income importing countries
# The smaller and smaller populations, higher income importing countries
# Of course this could be conflicting wheat and other factors such as industrialization, emissions, population.

# The split on the horizontal axis identifies importing vs exporting, producing countries. That's 

# the split between the vertical axis is about prices, yield and indicators regarding the subsistency of a population (how many calories, proteinsn, and fats can be extracted from a single population). That's the divide northern africa/middle east vs rest of the world, that could be analyzed further. (for the old one)



# Hierarchical Clustering:
agglo <- function(hc){
  data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0, 
                               hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE) 
}
# With Euclidean Distance
opar <- par(mfrow = c(1, 3))        


# 6 clusters is the best I'll get.
# EU, CHN, IND always together, either by itself or bunched with the Central Asian/Anglo-Saxon countries block
# USA AUS CAN together
# IRN and GBR switch a bit, usually IRN is with ISR and SAU
# RUS, KAZ, ARG, PAK, UKR always together
# CHL next to NXAL near CHE and NOR and in the same group as the middle eastern countries.

# The leaves are almost the same in every dendogram, regardless of the distance used. This consistency in similarity leads us to believe the clustering is working well.

h1_e<-hclust(ds_euc, method="average")
agglo(h1_e)
plot(h1_e, main= 'Average Linkage', cex = .65) 

h2_e<-hclust(ds_euc, method="complete")
agglo(h2_e)
plot(h2_e,  main= 'Complete Linkage', cex = .65)

h3_e<-hclust(ds_euc, method="ward")
agglo(h3_e)
plot(h3_e,  main= 'Ward Linkage', cex = .65)

# With Block/Manhattan Distance
h1_m<-hclust(ds_man, method="average")
agglo(h1_m)
plot(h1_m, main= 'Average Linkage', cex = .65) # 6 clusters~ more or less

h2_m<-hclust(ds_man, method="complete")
agglo(h2_m)
plot(h2_m,  main= 'Complete Linkage', cex = .65)

h3_m<-hclust(ds_man, method="ward")
agglo(h3_m)
plot(h3_m,  main= 'Ward Linkage', cex = .65) # Basically, I would choose this one. Strong against outliers, although my data is not exactly normally distributed and beautiful.


plot(h2_m, main="Complete Linkage")
rect.hclust(h2_m, 6)
h2cluster_m <- cutree(h2_m, k=6) 
table(h2cluster_m)

plot(h2_e, main="Complete Linkage", cex=.65)
rect.hclust(h2_e, 5)
h2cluster_e <- cutree(h2_e, k=5) 
h2cluster_e

plot(h3_m, main="Ward Linkage", cex = .65)
rect.hclust(h3_m, 6)
h3cluster_m <- cutree(h3_m, k=6) 
table(h1cluster_m)

h3cluster_e <- cutree(h3_e, k=6) 
h3cluster_e

plot(h1_m, main="Average Linkage", cex=.65)
h1cluster_m <- cutree(h1_m, k=6)
rect.hclust(h1_m, 6)
h1cluster_m
table(h1cluster_m)

h1cluster_e <- cutree(h1_e, k=6) 
h1cluster_e
table(h1cluster_e)

# they are all very similar among themselves. especially the anglo-saxon block, the central Asian block + argentina, the eu india china block, the colombia nigeria, and thailand block, the paraguay Peru and south africa block, mexico and ethiopia


# Exploring the solutions:

average_e <- cutree(h1_e, k=5)
complete_e<- cutree(h2_e, k=5)
ward_e<- cutree(h3_e, k=5)

average_m <- cutree(h1_m, k=5)
complete_m <- cutree(h2_m, k=5)
ward_m <- cutree(h3_m, k=5)

table(average_e,complete_e)
table(average_e,ward_e)
table(complete_e, ward_e)

table(average_m,complete_m)
table(average_m,ward_m)
table(complete_m, ward_m)
# most of them pretty similar, I think Manhattan distance is tighter, more consistent.



plot(oecd_scaled, col=hcluster, main="Complete Linkage") 
# Whatever I'm supposed to get from this, I ain't getting it.


hcluster <- cutree(h3_m, k=6)
table(hcluster)
hcluster

## explore solutions
means<-aggregate(oecd_data, list(hcluster), mean)
means 
means_scale<-aggregate(oecd_scaled, list(hcluster), mean)
means_scale
# Pretty much the PCA breakdown. 
# Average linkage w/ Euclidean distance: 
# 6 chn, ind, eu, are super highin every production and consumption index but low on price, and availability indicators
# 3 Chile, switzerland, norway, israel, new zealand, saudi arabia are low on everything even indicators
# 4 south africa, korea, peru, japan, paraguay, colombia, thailand, nigeria, brazil, ethiopia, and mexico all have even lower values than the previous group, mostly negative
# 5 turkey and egypt have very high indicator values, but everything else is pretty high
# 1 not as high as the anglo-saxon - central asian group: USA, Canada, Great Britain, Argentina, Kazhakistan, Pakistan, Russia, Australia. They have high production/consumption values but very low indicators
# 2 is just Iran which could easily fit with 5.

# Complete linkage w/ Euclidean distance: 
# 6 same as before
# 1 very similar as before, countries it's a bit of a new mix though, with Great Britain 
# 3 is somewhat similar to 4 before with less countries, 4 before got split, pretty negative overall
# 2 is the same as 1 before
# 5 is the same as before
# 4 is similar to 4, pretty negative overall

# Ward linkage w/ Euclidean distance: 
# 1 is the same as 2, the anglo-central-argentina group.high on production and consumption, feed...
# 2 is the chique northern countries except saudi and iran got sucked up into it.
# 3 is the same as 3 before, super low on all
# 4 is Ethiopia, South Africa, mexico, peru... not too high, not too low
# 5 always just egypt and turkey, which I find a bad idea personally.

# Average Linkage w/ Manhattan measures
# Manhattan seems a bit better more equally distributed. without bunching up shit in the same way

# 1 anglosaxon group, mostly positive and pretty high
# 2 iran, turkey and egypt, middle of the road, mostly positive , with decently high positive indicators
# 6 is the same as always, remember it's low on indicators
# 3 the smaller, elitish group, pretty negative on everything
# 4 the poor and mostly small people group, very low
# 5 the big and ok performing group, mostly positive, actually.

# Complete Linkage w/ Manhattan distance:
# it's all the same as before

# Ward Linkage w/ Manhattan distance:

# it lowkey seems the same. whatever. that should be a sign of consistency.


aggregate(oecd_scaled, list(h1cluster_e), mean)

# R^2
mydata<-oecd_scaled
mydata$group<-h3cluster_m
R2 <- rep(NA, (ncol(mydata)-1))
for(i in 1:(ncol(mydata)-1)) 
  R2[i] <- anova(aov(mydata[,i] ~ mydata[,ncol(mydata)]))[1,2]/(anova(aov(mydata[,i] ~ mydata[,ncol(mydata)]))[1,2]+anova(aov(mydata[,i] ~ mydata[,ncol(mydata)]))[2,2])
R2
mydata<-mydata[,-ncol(mydata)]
col<-colnames(mydata)
final<-cbind(col,R2)
final 

# Every euclidean distance measure is heavy on consumption, food and imports, complete also has ending stocks. complete and ward seem to be the ones with the the higher say here. Then emissions, and the indicators.
# Every Manhattan distance is the exact same. with emphasis on the indicators, and then the human consumption, food, and consumption having a bit of an impact. it's similar to PCA and it's well distributed. I would go with this.


## Explore K mean solution 
set.seed(123)
fit <- kmeans(mydata, 6)
aggregate(mydata,by=list(fit$cluster),FUN=mean)
mydata <- data.frame(mydata, fit$cluster)

heatmap(as.matrix(oecd_scaled))

wss <- (nrow(mydata))*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)

wss

wssplot <- function(mydata, nc=15, seed=123){
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:nc){
    set.seed(123)
    wss[i] <- sum(kmeans(mydata, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(mydata, nc=25) 


plot(1:10, wss, type="b", xlab="Number of clusters", ylab="Within Deviance")

plot(4:10, wss[4:10], type="b", xlab="Number of cluster", ylab="Within Deviance")
which.min(wss) # 3 works.


clusplot(oecd_scaled, fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0, cex = .5 )



# Through all of them
for(k in 2:10){
  k.m.fit <- kmeans(oecd_scaled, k) # k = 3
  clusplot(oecd_scaled, k.m.fit$cluster, 
           main=sprintf("2D representation of the Cluster solution\n k = %d",k),
           color=TRUE, shade=TRUE,
           labels=2, lines=0, cex = .4)
}
# The reasoning from before stays and they are pretty similar to the PCA and hierarchical clustering analysis.

# I may need to run different seeds. next time. when we meet. I would like to consider all of this pretty much done.

# 3 kind of remains the best in this scenario. Not bad.
# Missing this analysis: 
summary(oecd_scaled[fit$cluster==1,"Direct GHG emission"])
summary(oecd_scaled[fit$cluster==2,"Direct GHG emission"])
summary(oecd_scaled[fit$cluster==3,"Direct GHG emission"])
summary(oecd_scaled[fit$cluster==4,"Direct GHG emission"])
boxplot(oecd_scaled$`Direct GHG emission` ~ fit$cluster)
boxplot(oecd_scaled$`Production` ~ fit$cluster)
boxplot(oecd_scaled$`Feed` ~ fit$cluster)
boxplot(oecd_scaled$`Food` ~ fit$cluster)
boxplot(oecd_scaled$`Ending stocks` ~ fit$cluster)
boxplot(oecd_scaled$`Imports` ~ fit$cluster) # for 3 
boxplot(oecd_scaled$`Human consumption` ~ fit$cluster) # for 3 
boxplot(oecd_scaled$`Yield` ~ fit$cluster)
boxplot(oecd_scaled$`Producer price` ~ fit$cluster) # all the same height
boxplot(oecd_scaled$`Food protein availability` ~ fit$cluster)
boxplot(oecd_scaled$`Food fat availability` ~ fit$cluster)



# I explored the means. 
# Let's briefly explore the k-means.
