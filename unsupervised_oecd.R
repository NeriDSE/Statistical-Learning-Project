# Unsupervised Project - Agricultural Outlook for OECD countries - Statistical Learning
# we're looking at balance (production), indicators, prices in agriculture in 2019
# No outliers, check what works better

oecd_rough <- read_csv('oecd_agriculture.csv') 
oecd_rough <- read_csv('oecd_agriculture_more_countries.csv')

View(oecd_rough)
# https://data-explorer.oecd.org/vis?lc=en&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_AGR%40DF_OUTLOOK_2021_2030&df[ag]=OECD.TAD.ATM&dq=EU%2BUSA%2BGBR%2BCHE%2BCAN%2BNOR%2BAUS%2BCHL%2BCOL%2BISR%2BJPN%2BKOR%2BMEX%2BNZL%2BTUR.A.CPC_0111...&pd=2019%2C2019&to[TIME_PERIOD]=false&vw=tb

# Data Cleaning

oecd_cleaner <- oecd_rough %>% dplyr::select(-c(STRUCTURE, STRUCTURE_ID, STRUCTURE_NAME, ACTION, REF_AREA, FREQ,`Frequency of observation`, COMMODITY, Commodity, MEASURE, UNIT_MEASURE, VERSION_ID, `Version ID`, TIME_PERIOD, `Time period`, `Observation value`, OBS_STATUS, `Observation status`, DECIMALS, Decimals, CONVENTION, `Agricultural convention`, UNIT_MULT))
View(oecd_cleaner)


oecd_data <- oecd_rough %>% dplyr::select(-c(STRUCTURE, STRUCTURE_ID, STRUCTURE_NAME, ACTION, `Reference area`, FREQ,`Frequency of observation`, COMMODITY, Commodity, MEASURE, UNIT_MEASURE, VERSION_ID, `Version ID`, TIME_PERIOD, `Time period`, `Observation value`, OBS_STATUS, `Observation status`, `Unit multiplier`, DECIMALS, Decimals, CONVENTION, `Agricultural convention`, UNIT_MULT, `Unit of measure`))
View(oecd_data)

oecd_data <- oecd_data %>% pivot_wider(names_from = 'Measure', values_from = 'OBS_VALUE')
oecd_data <- oecd_data %>% select(-`Biodiesel production from commodity`)
oecd_data <- as.data.frame(oecd_data)
rownames(oecd_data)<-oecd_data$REF_AREA
oecd_data<-oecd_data %>% select(-REF_AREA)


zeros_per_column <- apply(oecd_data, 2, function(x) sum(x == 0))
zeros_per_column
oecd_data <- oecd_data %>% select(-`Ethanol production from commodity`, `Biofuel use`)

# Scaling, first unit conversion

View(oecd_data)
# Convert everything to thousands of tonnes
# Production, Consumption, Feed, Imports, Exports, Food, Other use are in tonnes, thousands of tonnes
# Human consumption is in kg, in units
oecd_scaled <- oecd_data

oecd_scaled$`Human consumption` <- oecd_data$`Human consumption`/1000000
# DIrect GHG emissions are tons of co2, which is in millions
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

# Then z-scaling:
oecd_scaled <- scale(oecd_data)
ds <- dist(scale(oecd_data))

ds_man <- dist(scale(oecd_data), method  = 'manhattan')

# No outlier Analysis (NOTE: take out all the countries you don't want here, don't switch between datasets):

oecd_data <- oecd_data %>% filter( row.names(oecd_data) != c('CHN', 'IND', 'EU'))

# Ready for the Hierarchical Clustering Analysis:

agglo <- function(hc){
  data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0, 
                               hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE) 
  }

opar <- par(mfrow = c(1, 3))                
### average linkage
h1<-hclust(ds, method="average")
agglo(h1)
plot(h1, main= 'Average Linkage')

h2<-hclust(ds, method="complete")
agglo(h2)
plot(h2,  main= 'Complete Linkage')

h3<-hclust(ds, method="ward")
agglo(h3)
plot(h3,  main= 'Ward Linkage')

# What we can see is that UK, Australia, and Canada are all grouped together, all the kind of anglosaxon leaning countries.
# Korea and japan are in the same place, makes sense along with Colombia and Mexico, all in the same region. Geographically similar but also very far apart pointing to a shared factor between the,m.
# smaller and richer in their apart strips of land, not super agriculturally prone, Chile is kind of the outlier
# us and turkey it's unclear why they're so similar

# a bit by landmass, a bit by geography, a bit of everything. Check if some variables should be excluded/what matters most. with PCA... bit of a mess. yea fr. could I eliminate them off the bat since they aren't strictly on the measures? how good of an idea is that? email. this is clearly a mess.


average <- cutree(h1, k=5)
complete<- cutree(h2, k=5)
ward<- cutree(h3, k=5)
table(average,complete)
table(average,ward)
table(complete, ward)

plot(h1, main="Complete linkage")
rect.hclust(h2, 5)
h2cluster <- cutree(h2, k=5)
h2cluster

plot(oecd_data, col=h2cluster, main="complete likage")

table(h2cluster)

## explore solution
means<-aggregate(oecd_data, list(h2cluster), mean)
means 
means_scale<-aggregate(oecd_scaled, list(h2cluster), mean)
means_scale
# what can be clearly seen here is that 1 and 5 (all the anglosaxons, developed economies) have very high production, consumption and general values
# 2 and 3 are lower, with the richer smaller ones less than the larger a bit poorer ones, low emissions too. So, in a way it adds up.



# R^2
mydata<-oecd_data
mydata$group<-h2cluster
R2 <- rep(NA, (ncol(mydata)-1))
for(i in 1:(ncol(mydata)-1)) 
  R2[i] <- anova(aov(mydata[,i] ~ mydata[,ncol(mydata)]))[1,2]/(anova(aov(mydata[,i] ~ mydata[,ncol(mydata)]))[1,2]+anova(aov(mydata[,i] ~ mydata[,ncol(mydata)]))[2,2])
R2
# Consumption, Food, Ending Stock, Biofuel Use, Imports are the main players here.
# They're determining the difference in the clusters.
# Of course I could try with the other clusters too. It will be done.

mydata<-mydata[,-ncol(mydata)]
col<-colnames(mydata)
final<-cbind(col,R2)
final


##### K means 
## How many clusters??
wss <- (nrow(mydata))*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)

wss # 8 clusters

plot(1:10, wss, type="b", xlab="Number of clusters", ylab="Within Deviance")

plot(4:10, wss[4:10], type="b", xlab="Number of cluster", ylab="Within Deviance")
which.min(wss) # 10 is 10 times lower than 4 by the way, you can't tell because the first one is so large

summary(oecd_data[fit$cluster==1,"Human consumption"])
summary(oecd_data[fit$cluster==2,"Human consumption"])
summary(oecd_data[fit$cluster==3,"Human consumption"])
summary(oecd_data[fit$cluster==4,"Human consumption"])
boxplot(oecd_data$`Yield` ~ fit$cluster)


## Explore K mean solution 
fit <- kmeans(mydata, 3) 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
mydata <- data.frame(mydata, fit$cluster)

table(h2cluster,fit$cluster) # compare with the hierarchical clusters from before

heatmap(oecd_scaled, scale = "row") 
# it's all good until the 4th/5th that's a fact

clusplot(oecd_scaled, fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
# Through all of them
for(k in 2:6){
  k.m.fit <- kmeans(oecd_scaled, k) # k = 3
  clusplot(oecd_scaled, k.m.fit$cluster, 
           main=sprintf("2D representation of the Cluster solution\n k = %d",k),
           color=TRUE, shade=TRUE,
           labels=2, lines=0)
}

# PCA
pca_oecd<-princomp(oecd_data, cor=T) # can't do it they want more data. that... can be done. I guess. I can pull more countries in here. Like 20 more.
summary(pca_oecd)
screeplot(pca_oecd) # only up until the 4th

# plots:
plot(pca_oecd$scores)
text(pca_oecd$scores, rownames(oecd_data))
abline(h=0, v=0)
## biplot 
biplot(pca_oecd)

fviz_contrib(pca_oecd, "var", axes = 1, fill = "forestgreen", color = "forestgreen")
fviz_contrib(pca_oecd, "var", axes = 2, fill = "forestgreen", color = "forestgreen")
fviz_contrib(pca_oecd, "var", axes = 3, fill = "forestgreen", color = "forestgreen")
fviz_contrib(pca_oecd, "var", axes = 4, fill = "forestgreen", color = "forestgreen")

# India, China, and the EU are outliers in wheat production, on the side of emissions and production, Bunch of poor countries on the left and rich countries in the middle. Then the PCA interpretation and there we go.
# One with and one without the outliers with the agricultural explanations and the possible OECD explanations that I'd have to read up and write about. Oh boy. Some strain.
