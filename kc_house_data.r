#import libraries 
library(tidyverse)
library(boot)
library(stringr)
library(lubridate)
library(DT)
library(leaflet)
library(caret)
library(corrplot)
library(ggplot2)
library(ggmap)
library(randomcoloR)
library(scatterplot3d)
library(RColorBrewer)
library(randomForest)
library(leaps)
library(rpart)
library(rpart.plot)

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

#import the dataset
kc_house<-read.csv("kc_house_data.csv")

#TODO: dove abbiamo preso i dati, spiegare ogni colonna cosa significa

#check the dimension of the dataset
dim(kc_house)


#convert date from string to observation number of the day (starting from 1)
new.date<-as.Date(kc_house$date,'%Y%m%d')
new.date<-new.date-(sort(new.date))[1]

#We add this column to the dataset instead of the original one
kc_house[2]<-as.numeric(new.date)
kc_house

#check the number of duplicated ids
length(kc_house$id) - length(unique(kc_house$id))
#since id is not unique, we retrieve rows with duplicated id
#duplicated(id)
d<-kc_house[duplicated(kc_house$id),]
dim(d)
#we note that there are some duplicates wrt id and date
#we suppose, by observations, that a specific id corresponds to a specific date
d<-kc_house[duplicated(kc_house$id, kc_house$date),]
dim(d)
#we want to check whether they are the same rows
all(duplicated(kc_house$id)==duplicated(kc_house$id,kc_house$date))
#our observation is confirmed by this check
#hence, id not useful to understand data and to predict
kc_house<-kc_house[-1]

# kc_house
dim(kc_house)
kc_house

#we now take into account the column "yr_renovated": this column express the year of the last renovation of the
#house. Since the null values are coded as 0 and the other are coded as a year, this two are not very consistent and
#so we want to transform this column into a feature that express the year of the last renovation: if no renovation
#is done on a house, we use the year of construction.

#we firtsly check that there are no errors in the data, i.e. year of renovation smaller than year of construction.
any(kc_house$yr_built>kc_house$yr_renovated && kc_house$yr_renovated!=0)

#since there are no errors, we now want to substitute the "yr_renovated" feature with the year of the last renovation
#or, if not present, the year of construction.
mask<-kc_house$yr_renovated==0
mask
ylr<-kc_house$yr_renovated
ylr[mask]<-kc_house$yr_built[mask]
ylr

#rename the column
kc_house["yr_renovated"]<-ylr

colnames(kc_house)[15]<-"yr_last_renovation"
length(kc_house$yr_last_renovation)
#move the target feature (price) to the last column
kc_house<-kc_house[,c("date", "bedrooms", "bathrooms", "sqft_living","sqft_lot", "floors", "waterfront","view","condition",
           "grade", "sqft_above", "sqft_basement", "yr_built", "yr_last_renovation", "zipcode",  "lat", "long","sqft_living15", 
           "sqft_lot15", "price")]

kc_house







#EXPLORING DATA

#The grade is a classification by construction quality which refers to the types of materials used and the quality 
#of workmanship. Buildings of better quality (higher grade) cost more to build per unit of measure and command 
#higher value (https://info.kingcounty.gov/assessor/esales/Glossary.aspx?type=r#g). For this reason it is a 
#value (from 1 to 13) established before and independently wrt the price of the house; so, it makes sense to keep it in 
#the dataset to predict the price of the house.
#First of all, we note that the column "bathrooms" has real (and not only integer) values: this is because
#it expresses the #bathroom/#bedrooms.
#Moreover, also the column "floors" has float values: this is because there can be "partial" (half) floors (like
#the mansard) that cannot be considered as a whole floor. 
#Condition is a value from 1 to 5 that expresses the condition state of the house.
#sqft_living is the sum of sqft_above + sqft_basement 

#we start the exploration of the data with an overall numerical summary
summary(kc_house)

#we notice that the maximum number of bedrooms is 33, a very high number. Moreover we note that this house
#must have also 33*1.75 bathrooms, which is 57 total bathrooms, i.e. 90 total rooms (excluding other possible
#spaces.)
#We want to see the other values of that row, that are the values of that house.
kc_house[kc_house$bedrooms==33,]
boxplot(kc_house$bedrooms)
# We note that the total living space is 1620 square feet, equal to about 150 m^2. This would imply that each room has a mean of 1.66 m^2,
#which is clearly impossible. So, there must be some errors in the reporting of the data (the number of 
#bedrooms could be 3 for example).

#So, we decide to delete that row.

kc_house<-kc_house[kc_house$bedrooms<33,]
dim(kc_house)

#In general, we want to check the mean square footage of the rooms per each house, to see if there are 
#other unlikely values as the previous one.

#Define the mean dimension of a room in a house, in squared meters
mean_sqm<-(kc_house$sqft_living/(kc_house$bedrooms+(kc_house$bedrooms*kc_house$bathrooms)))/10.764
length(mean_sqm)
#we note that there are some infinite values due to the fact that there are no bedrooms and bathrooms in 
#that building. So, we want to boxplot the mean_sqft excluding those houses.

boxplot(mean_sqm[mean_sqm<Inf])
min(mean_sqm)

#We see that now the minimum value of the mean square meters of the rooms in a house is 3 times the 
#previous, so we have no evidence that this is an unlikely value.

#using the lm function, we notice an issue in the outcome of the summary
model<-lm(kc_house$price ~ ., data=kc_house)
summary(model)

#From the summary of the model we have just applied we can see that the coefficient of the column 
#"sqft_basement" are labeled as NA: this is because there is a collinearity between that feature and 
#the two column "sqft_living" and "sqft_above". In fact, "sqft_basement" = "sqft_living" - "sqft_above".
#We give the proof of that:

sqft_diff<-kc_house$sqft_living-(kc_house$sqft_basement + kc_house$sqft_above)
any(sqft_diff!=0)

#As we notice, all the values in "sqft_diff" are zero.
#So, for the prediction we get rid of the feature "sqft_basement" because it doesn't add any valuable 
#information wrt to what we know from the other two columns.

#we delete the column sqft_basement because it add no infos and it can be add in future if we want (lossless delete)
kc_house<-kc_house[,-12]

#we plot the distribution of sqft_lot
hist(kc_house$sqft_lot, breaks=30,main= "sqft_lot distribution", xlab = "sqft_lot")
#we can see that this feature has a really bad distribution, so we can try applying the log10 to it.
kc_house[5]<-log10(kc_house$sqft_lot)
kc_house[18]<-log10(kc_house$sqft_lot15)

hist(kc_house$sqft_lot, breaks=40, main = "Histogram of dimensions of lots")


#We want to see the type of houses we're dealing with; thus, we check the distribution of the grade of the
#houses and we compare it to a normal distribution.
hist(kc_house$price, breaks = 1000, main = "Price frequency", xlab="Price")

out_threshold<-min(boxplot(kc_house$price)$out)

# We are cutting out 5% of the data (outliers) that are not representative wrt to the type 
# of houses our predictor has to deal with
dim(kc_house[kc_house$price>out_threshold,])[1]/dim(kc_house)[1]*100
kc_house<-kc_house[kc_house$price<out_threshold,]
dim(kc_house)

hist(kc_house$price, breaks=100)
qqnorm(kc_house$price)

#Instead, if we would consider the log10 transformation of the target value, the distribution
#becomes really close to a Normal one. We are going to use this later.
qqnorm(log10(kc_house$price))
#we also log the price
kc_house[19]<-log10(kc_house[19])
hist(kc_house$price, breaks = 100, main= "Frequency of log(price)", xlab = "log(price)")
dim(kc_house)


attach(kc_house)

# MAP
# Set location bounds (King County)
location <- c(-140, 35, -90, 57)
# Fetch the map (osm = OpenStreetMap)
points.kc <- get_map(location=location, source="osm")
# Draw the map
map.kc <- ggmap(points.kc)
# Add the points layer
map.kc <- map.kc + geom_point(data = kc_house, aes(x = kc_house$long, y = kc_house$lat), size = .0001)
# Plot map
map.kc

# More zoom
# Set location bounds (King County)
location <- c(-123.25, 47.15, -121.25, 47.9)
# Fetch the map (osm = OpenStreetMap)
points.kc <- get_map(location=location, source="osm")
# Draw the map
map.kc <- ggmap(points.kc)
# Add the points layer
map.kc <- map.kc + geom_point(data = kc_house, aes(x = kc_house$long, y = kc_house$lat), size = .0001)
# Plot map
map.kc

# Print a table containing correlation between covariables
cor(kc_house)
# Plot correlation between covariates
corrplot(cor(kc_house))

# DATE (cor=-0.00570)
# We notice we have a continuous detection for the dates: almost every day (starting from the initial one)
# there are some house sold and so we don't have time intervals in which values of price are not present, 
# as we can see from this plot.
plot(sort(date))
# Plot of price as a function of the date
plot(price~date, main="Price by date")
# Plot using boxplot to gain more information about the correlation between price and date
boxplot(price~sort(date), main="Price by date")
# To see in a better way the data, we can average for each date the prices of the houses sold in that
# day. Obviously we get an approximation, but this is not a big deal having a lot of examples (about 60 
# per day) and this allows us to see really data in a nicer way.
average_price_bydate <-aggregate(price~date, FUN=mean, data=kc_house)
plot(average_price_bydate, col=1,main="Average price by date")
lines(loess.smooth(date, price), col=2, lty=5)
# Hence, the date does not influence much the price

# BEDROOMS (cor=0.35100546)
boxplot(price~bedrooms, xlab="bedrooms", ylab="price",main="Price by bedrooms")
# We see that the greater the number of bedrooms, the greater the price. Also we can notice that
# there is a big number of outliers in the upper part for values of bedrooms in the range 2-6.

kc_house %>%
  group_by(bedrooms) %>%
  summarise(PriceMedian = median(10**(price), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bedrooms = reorder(bedrooms,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%

  ggplot(aes(x = bedrooms,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = "yellow") +
  geom_text(aes(x = bedrooms, y = 1, label = paste0(" ",PriceMedian, sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'bedrooms', 
       y = 'Median Price', 
       title = 'Median price per number of bedrooms') +
  coord_flip() + 
  theme_bw()


#BATHROOMS (cor=0.55082290)
boxplot(price~bathrooms, xlab="bathrooms", ylab="price",main="Price by bathrooms")
# Also in this case, and more than for the previous one, it is clear that if we increase the number of 
# bathrooms, also the price increases.

kc_house %>%
  group_by(bathrooms) %>%
  summarise(PriceMedian = round(median(10**(price)),0), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(bathrooms = reorder(bathrooms,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  head(10) %>%
  
  ggplot(aes(x = bathrooms,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = bathrooms, y = 1, label = paste0(" ", PriceMedian, sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'bathrooms', y = 'Median Price', title = 'Median Price per number of bathrooms') +
  coord_flip() + 
  theme_bw()

# SQFT_LIVING (cor=0.69536476)
plot(price~sqft_living,main="Price by sqft_living")
lines(loess.smooth(sqft_living, price), col=3)
# Check averages instead of actual data
average_price_bysqftliving <-aggregate(price~sqft_living, FUN=mean, data=kc_house)
plot(average_price_bysqftliving, col=1,main="Average price by living area")
lines(loess.smooth(sqft_living, price), col=3)
# Also in this case there is an evident association between the dimensions of the living area and the price.

# SQFT_LOT (cor=0.09962940)
plot(price~sqft_lot)
# Apply the aggregate function
average_price_bysqftlot <-aggregate(price~sqft_lot, FUN=mean, data=kc_house)
plot(average_price_bysqftlot, col=1,main="Average price by lot dimension")
lines(loess.smooth(sqft_lot, price), col=3)
# The plot shows that This feature does not influence so much the targe

# FLOORS (cor= 0.31059266)
boxplot(price~floors, xlab="floors", main="Price by floor")
# There is a little increase in the median price as floors increase, but not so much

# WATERFRONT (cor=0.17459026)
boxplot(price~waterfront, xlab="waterfront", main="Price by waterfront")

# VIEW (cor= 0.34653430)
boxplot(price~view, xlab="view", main="Price by view")
# The plot shows that there is a correlation between the covariate view and price

# CONDITION (cor= 0.03949428)
boxplot(price~condition, xlab="condition", main="Price by condition")

# GRADE (cor= 0.70366105)
boxplot(price~grade, xlab="grade", main="Price by grade")
# The plot shows that the grade has a strong correlation with the target

# We suspect correlation between GRADE and almost every other covariate
# since GRADE is the mark given by an expert after evaluating many aspects of an house.
# Hence, we try to fit a linear model in order to predict the grade using every covariate, except from price
lm_grade <- lm(grade ~ . -price -sqft_lot15 -date, data=kc_house)
summary(lm_grade)

kc_house %>%
  group_by(grade) %>%
  # summarise(PriceMedian = median(10**(price)), na.rm = TRUE) %>%
  summarise(PriceMedian = round(median(10**(price),2)), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(grade = reorder(grade,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  
  ggplot(aes(x = grade,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = grade, y = 1, label = paste0(" ",PriceMedian, sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Grade', 
       y = 'Median price', 
       title = 'Grade and median price') +
  coord_flip() + 
  theme_bw()

# SQFT_ABOVE(cor=0.60184347)
plot(price~sqft_above)
# Check average values instead of median
average_price_bysqftabove <-aggregate(price~sqft_above, FUN=mean, data=kc_house)
plot(average_price_bysqftabove, col=1,main="Average price by above dimension")
lines(loess.smooth(sqft_above, price), col=5)
# The plot shows that price is linearly dependent from the feature sqft_above

# YR_BUILT (cor=0.08067957)
plot(price~yr_built)
lines(loess.smooth(yr_built, price), col=5)
# Plot using the means
average_price_yb <-aggregate(price~yr_built, FUN=mean, data=kc_house)
plot(average_price_yb, col=1,main="Average price by construction year", ylim=c(5,6.5))
lines(loess.smooth(yr_built, price), col=5)
# Previous plots shows that there is no linear relationship between when an house has been built and its price

# YR_LAST_RENOVATION (cor=0.13032651)
plot(price~yr_last_renovation)
lines(loess.smooth(yr_last_renovation, price), col=5)
# Plot using the means
average_price_ylr <-aggregate(price~yr_last_renovation, FUN=mean, data=kc_house)
plot(average_price_ylr, col=1,main="Average price by above dimension", ylim=c(4.9,6.7))
lines(loess.smooth(yr_last_renovation, price), col=5)

# LAT (cor=0.44916082)
plot(price~lat)
lines(loess.smooth(lat, price), col=5)
# Plot using the means
average_price_lat <-aggregate(price~lat, FUN=mean, data=kc_house)
plot(average_price_lat, col=1,main="Average price by lat")
lines(loess.smooth(lat, price), col=5)
# The plot shows that latitude and price are quite correlated

# LONG (cor=0.04996692)
plot(price ~ long)
lines(loess.smooth(long, price), col=5)
# Plot using the means
average_price_long <-aggregate(price~long, FUN=mean, data=kc_house)
plot(average_price_long, col=1,main="Average price by long")
lines(loess.smooth(long, price), col=5)
# The logitude seems to be less correlated with price

# ZIPCODE (cor=0.03831967)
plot(price~zipcode)
lines(loess.smooth(zipcode, price), col=5)
# Plot using the means
average_price_zc <-aggregate(price~zipcode, FUN=mean, data=kc_house)
plot(average_price_zc, col=1,main="Average price by zipcode", ylim=c(5,6.8))
lines(loess.smooth(zipcode, price), col=5)
# Plots using boxplots
boxplot(price~zipcode)

# Since we suspect a strong correlation between LONG, LAT and ZIPCODE, we plot them on the map
# There are 70 zipcodes
n_zipcodes <- length(unique(zipcode))
# Define colors scheme (randomly)
set.seed(3)
# Select one different color per zipcode
palette <- randomColor(n_zipcodes)
# Set location bounds (King County)
location <- c(-123.25, 47.15, -121.25, 47.9)
# Fetch the map (osm = OpenStreetMap)
map <- get_map(location=location, source="osm")
# Plot the points on the map using lat and long, while the color is given by the zipcode
plt <- ggmap(map)
plt <- plt + geom_point(data=kc_house, aes(x=long, y=lat, col=as.factor(zipcode)))
plt <- plt + theme(legend.position='none')
plt <- plt + scale_colour_manual(values=palette)
plt

# We hence try to fit a linear model to predict correlation between LONG, LAT and ZIPCODE
lm_zipcode <- lm(zipcode ~ long + lat, data=kc_house)
summary(lm_zipcode)
# The linear model does not provide a significantly high R squared value, hence we can keep all the variables

# PRICE with respect to LAT and LONG
# Get colors for labeling the points
plotvar <- kc_house$price # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr, "PuBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
plot.angle <- 290
# 3D Scatter plot
scatterplot3d(kc_house$long, kc_house$lat, plotvar, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=2, 
              col.axis="gray", col.grid="gray", xlab="Longitude", ylab="Latitude", zlab="Price", 
              main="Price as a function of latitude and longitude")



#We plot the houses in the map
bins<-cut(kc_house$price, c(0,log10(250e3),log10(500e3),log10(750e3),log10(1e6),log10(2e6)))

center_lon = median(kc_house$long,na.rm = TRUE)
center_lat = median(kc_house$lat,na.rm = TRUE)

factpal <- colorFactor(c("black","blue","yellow","orange","#0B5345"), 
                       bins)


leaflet(kc_house) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~long, lat = ~lat, 
             color = ~factpal(bins))  %>%
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 12) %>%
  
  addLegend("bottomright", pal = factpal, values =~bins,
            title = "House Price Distribution",
            opacity = 1)

PriceBinGrouping = function(limit1, limit2)
{
  return(
    
    kc_house%>%
      filter(price > limit1) %>%
      filter(price <= limit2)
  )
}

PriceGroup1 = PriceBinGrouping(0,log10(250e3))
PriceGroup2 = PriceBinGrouping(log10(250e3),log10(500e3))
PriceGroup3 = PriceBinGrouping(log10(500e3),log10(750e3))
PriceGroup4 = PriceBinGrouping(log10(750e3),log10(1e6))
PriceGroup5 = PriceBinGrouping(log10(1e6),log10(2e6))

MapPriceGroups = function(PriceGroupName,color)
{
  center_lon = median(PriceGroupName$long,na.rm = TRUE)
  center_lat = median(PriceGroupName$lat,na.rm = TRUE)
  
  leaflet(PriceGroup2) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
    addCircles(lng = ~long, lat = ~lat, 
               color = ~c(color))  %>%
    # controls
    setView(lng=center_lon, lat=center_lat,zoom = 12)
}
#to show the houses of that group, do for example:
MapPriceGroups(PriceGroup1,"black")
MapPriceGroups(PriceGroup2,"blue")
MapPriceGroups(PriceGroup3,"yellow")
MapPriceGroups(PriceGroup4,"orange")
MapPriceGroups(PriceGroup5,"#0B5345")


# SQFT_LIVING_15 (cor=0.61935746)
plot(price~sqft_living15)
lines(loess.smooth(sqft_living15, price), col=3)
# Plot using means
average_price_l15 <-aggregate(price~sqft_living15, FUN=mean, data=kc_house)
plot(average_price_l15, col=1,main="Average price by sqft_living15")
lines(loess.smooth(sqft_living15, price), col=5)
# The plot shows that price and sqft_living are strictly correlated

# SQFT_LOT15 (cor=0.09160121)
plot(price~log10(sqft_lot15))
lines(loess.smooth(log10(sqft_lot15), price), col=3)
# Plot using means
average_price_lot15 <-aggregate(price~sqft_lot15, FUN=mean, data=kc_house)
plot(average_price_lot15, col=1,main="Average price by sqft_lot15")
lines(loess.smooth(sqft_lot15, price), col=5)

# WHAT ARE MOST IMPORTANT FEATURE TO PREDICT THE PRICE?
# We try to answer the question looking at the features that causes the greatest decrease in
# the R2 value, repeating the process iteratively for each feature.
# Linear model of price against every feature
model <- lm(price~ ., data=kc_house)
sm <- summary(model)

# Definition of an empty matrix which contains model, R2, adjusted R2
models <- matrix(data=NA, nrow=18, ncol=5, dimnames=list(c(), c('deleted', 'r2', 'adj.r2', 'diff.r2', 'diff.adj.r2')))
# Define the features names 
features <- colnames(kc_house)[-19]
# Loop through every model combination without a single feature
for(i in 1:length(features)) {
  # Define a linear model without the current feature
  curr_model <- lm(as.formula(paste('price ~ . -', features[i], sep='')), data=kc_house)
  # Save the summary
  curr_sm <- summary(curr_model)
  # Save deleted feature, r2 and adjusted r2
  models[i,1] <- features[i]
  models[i,2] <- curr_sm$r.squared
  models[i,3] <- curr_sm$adj.r.squared
  # Add difference between previous and current r2 and adjusted r2
  models[i,4] <- sm$r.squared - curr_sm$r.squared
  models[i,5] <- sm$adj.r.squared - curr_sm$adj.r.squared
}

# table for visualisation of results
# View(models)
# Find the feature which causes the greatest decrease in R2 value when deleted
models[which.max(models[,4]),1]
# Order the matrix by r2 difference
models <- models[order(as.numeric(models[,4]), decreasing=TRUE),]
# Show models
fix(models)
# Plot the features "importance"
plot(models[,4], xlab="", ylab="R2 difference", main="Feature importance by R2 reduction", xaxt='n', type='p')
# Add labels  
axis(1, at=1:length(models[,1]), labels=models[,1], las=2)


#Calculation of variable importance for regression in another way
fitControl <- trainControl(method="cv",number = 10)
model_1 = train(price~ ., data = kc_house, method = "lm",trControl = fitControl,metric="RMSE")
importance_model_1 = varImp(model_1)

# we then plot 

PlotImportance = function(importance)
{
  varImportance <- data.frame(Variables = row.names(importance[[1]]), 
                              Importance = round(importance[[1]]$Overall,2))
  
  # Create a rank variable based on importance
  rankImportance <- varImportance %>% 
    mutate(Rank = paste0('#',dense_rank(desc(Importance))))
  
  rankImportancefull = rankImportance
  
  ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                             y = Importance)) +
    geom_bar(stat='identity',colour="white", fill=fillColor) +
    geom_text(aes(x = Variables, y = 1, label = Rank, color="blue"),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Variables', title = 'Relative Variable Importance') +
    coord_flip() + 
    theme_bw()
}

PlotImportance(importance_model_1)

# we can notice, in fact, that the last two features (sqft_above, long) are the first to be
# deleted when doing backward variable selection!

# Plot of price as function of the covariates, taken one by one
# Note that heare only the best 7 covariates, by importance given by r2 difference, are taken
pairs(price ~ lat + grade + yr_built + sqft_living + sqft_living15 + condition + view, data=kc_house)


##########
# BACKWARD FEATURE SELECTION

#Define the random seed (otherwise we cannot repeat exactly the same experiment)
set.seed(29)
#try to evaluate performance of different models splitting the whole set into training-validation-test set
id.train<-createDataPartition(price, p=.90, list=FALSE)
train_set<-kc_house[id.train,] #90
test_set<-kc_house[-id.train,] #10


par(mfrow=c(1,2))
boxplot(train_set$price, main="Train", ylim=c(4.5,7))
boxplot(test_set$price, main="Test", ylim=c(4.5,7))
par(mfrow=c(1,1))

###########
# automatic backward feature selection
# regfit.back <- regsubsets(price~.,data=train_set, nvmax=18,method="backward")
# summary(regfit.back)
# reg.summary <- summary(regfit.back)
# 
# plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
# plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
# which.max(reg.summary$adjr2)
# points(18,reg.summary$adjr2[18], col="red",cex=2,pch=20)
# plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
# plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')


# We want to find the best model of grade 1: first of all we try considering the 18 best models
# (one for each number of variables) and then with the step function, using Schwarz Information Criterion as metrics.
# We then also find the best models of grade 2-5 with the step function using BIC.



# grade 1

#we want to find the best model to predict, so first of all we find the best model for each
#number of covariates, then we apply a 10-fold Cross Validation to see which of those
#has the smaller error in the prediction

#maximum number of predictors
max_var=18

#find the best model per number of predictors
regfit.best <- regsubsets(price~ ., data=train_set, nvmax=max_var, method="backward")
reg.summary <- summary(regfit.best)
# model matrix construction 
# test.mat <- model.matrix(price~ .,data=kc_house[test,])
# test.mat
# colnames(test.mat)

# RMSE on the test set
# val.errors=rep(NA,max_var)
# for(i in 1:max_var){
#   coefi <- coef(regfit.best,id=i)
#   pred <- test.mat[,names(coefi)]%*%coefi
#   val.errors[i] <- sqrt(mean((kc_house$price[test]-pred)^2))
# }
# 
# val.errors
# best<-which.min(val.errors)
# 
# coef(regfit.best, best)


# K-fold cross-validation
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

k=10
set.seed(1)
folds <- sample(1:k, dim(train_set)[1], replace=TRUE)
cv.errors <- matrix(NA, k, max_var, dimnames=list(NULL, paste(1:max_var)))

for(j in 1:k){
  best.fit <- regsubsets(price~ ., data=train_set[folds!=j,], nvmax=max_var)
  
  for(i in 1:max_var){
    pred <- predict(best.fit, train_set[folds==j,], id=i)
    cv.errors[j,i] <- sqrt(mean((train_set$price[folds==j]-pred)^2))
  }
}

plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
points(which.min(reg.summary$rss),reg.summary$rss[which.min(reg.summary$rss)], col="red",cex=2,pch=20)

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')

mean.cv.errors <- apply(cv.errors, 2, mean)


#vector of mean errors
mean.cv.errors

#number of variables in the best model
which.min(mean.cv.errors)

#error with the best model (17 variables)
mean.cv.errors[which.min(mean.cv.errors)]

plot(mean.cv.errors, type="l", main="RMSE for each model", xlab="Number of variables in the model",
     ylab = "RMSE")
points(which.min(mean.cv.errors), mean.cv.errors[which.min(mean.cv.errors)], col=2, pch=16)
#######


# We try to find the best model for each grade of polynomial

# Set n as training set dimension
n <- dim(train_set)[1]


# grade 1

# We try with BIC 
full.mod <- lm(price~ ., data=train_set)
step.mod <- step(full.mod, steps=1000, k=log(n), trace=1, direction="backward")
formula1 <- step.mod$call$formula

# Try using the AIC as metric for the selection 

full.mod <- lm(price~ ., data=train_set)
step.mod <- step(full.mod, steps=1000, trace=1, direction="backward")
formula1 <- step.mod$call$formula


# NOTE: the BIC penalize more in the number of varialbles (the optimal number according to BIC is 
# 16), while with AIC we get the same model obtained in the previous procedure, the one
# considering the 18 best model (one model for each number of variables).

# cv.glm function: The data is divided randomly into K groups. For each group the
# linear model is fit to data omitting that group, then the function cost (by default the 
# average squared error function) is applied to the observed responses in the group 
# that was omitted from the fit and the prediction
# made by the fitted models for those observations.
set.seed(17)
glm.fit <- glm(formula1,data=train_set)
cv.error1 <- cv.glm(train_set, glm.fit, K=10, cost=RMSE)$delta[1]
cv.error1

# We then proceed trying to find the best model for each degree of the polynomial
#grade 2
full.mod <- lm(price~date+I(date^2)+
              +bedrooms+I(bedrooms^2)+
              +bathrooms+I(bathrooms^2)
              +sqft_living+I(sqft_living^2)
              +sqft_lot+I(sqft_lot^2)
              +floors+I(floors^2)
              +waterfront
              +view+I(view^2)
              +condition+I(condition^2)
              +grade+I(grade^2)
              +sqft_above+I(sqft_above^2)
              +yr_built+I(yr_built^2)
              +yr_last_renovation+I(yr_last_renovation^2)
              +zipcode+I(zipcode^2)
              +lat+I(lat^2)
              +long+I(long^2)
              +sqft_living15+I(sqft_living15^2)
              +sqft_lot15+I(sqft_lot15^2),
              data=train_set)
step.mod <- step(full.mod, steps=100, trace=1, k=log(n),direction="backward")
formula2<-step.mod$call$formula

set.seed(17)
glm.fit <- glm(formula2,data=train_set)
cv.error2 <- cv.glm(train_set,glm.fit,K=10, cost=RMSE)$delta[1]
cv.error2

#grade 3
full.mod <- lm(price~date+I(date^2)+I(date^3)+
            +bedrooms+I(bedrooms^2)+I(bedrooms^3)+
            +bathrooms+I(bathrooms^2)+I(bathrooms^3)
            +sqft_living+I(sqft_living^2)+I(sqft_living^3)
            +sqft_lot+I(sqft_lot^2)+I(sqft_lot^3)
            +floors+I(floors^2)+I(floors^3)
            +waterfront
            +view+I(view^2)+I(view^3)
            +condition+I(condition^2)+I(condition^3)
            +grade+I(grade^2)+I(grade^3)
            +sqft_above+I(sqft_above^2)+I(sqft_above^3)
            +yr_built+I(yr_built^2)+I(yr_built^3)
            +yr_last_renovation+I(yr_last_renovation^2)+I(yr_last_renovation^3)
            +zipcode+I(zipcode^2)+I(zipcode^3)
            +lat+I(lat^2)+I(lat^3)
            +long+I(long^2)+I(long^3)
            +sqft_living15+I(sqft_living15^2)+I(sqft_living15^3)
            +sqft_lot15+I(sqft_lot15^2)+I(sqft_lot15^3),
            data=train_set)
step.mod <- step(full.mod, steps=100, trace=1, k=log(n),direction="backward")
formula3<-step.mod$call$formula

set.seed(17)
glm.fit <- glm(formula3,data=train_set)
cv.error3 <- cv.glm(train_set, glm.fit, K=10, cost=RMSE)$delta[1]
cv.error3

#grade 4
full.mod <- lm(price~date+I(date^2)+I(date^3)+I(date^4)
            +bedrooms+I(bedrooms^2)+I(bedrooms^3)+I(bedrooms^4)
            +bathrooms+I(bathrooms^2)+I(bathrooms^3)+I(bathrooms^4)
            +sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)
            +sqft_lot+I(sqft_lot^2)+I(sqft_lot^3)+I(sqft_lot^4)
            +floors+I(floors^2)+I(floors^3)+I(floors^4)
            +waterfront
            +view+I(view^2)+I(view^3)+I(view^4)
            +condition+I(condition^2)+I(condition^3)+I(condition^4)
            +grade+I(grade^2)+I(grade^3)+I(grade^4)
            +sqft_above+I(sqft_above^2)+I(sqft_above^3)+I(sqft_above^4)
            +yr_built+I(yr_built^2)+I(yr_built^3)+I(yr_built^4)
            +yr_last_renovation+I(yr_last_renovation^2)+I(yr_last_renovation^3)+I(yr_last_renovation^4)
            +zipcode+I(zipcode^2)+I(zipcode^3)+I(zipcode^4)
            +lat+I(lat^2)+I(lat^3)+I(lat^4)
            +long+I(long^2)+I(long^3)+I(long^4)
            +sqft_living15+I(sqft_living15^2)+I(sqft_living15^3)+I(sqft_living15^4)
            +sqft_lot15+I(sqft_lot15^2)+I(sqft_lot15^3)+I(sqft_lot^4),
            data=train_set)
step.mod <- step(full.mod, steps=100, trace=1, k=log(n),direction="backward")
formula4 <- step.mod$call$formula

set.seed(17)
glm.fit <- glm(formula4, data=train_set)
cv.error4 <- cv.glm(train_set, glm.fit, K=10, cost=RMSE)$delta[1]
cv.error4

#grade 5
full.mod <- lm(price~date+I(date^2)+I(date^3)+I(date^4)+I(date^5)
               +bedrooms+I(bedrooms^2)+I(bedrooms^3)+I(bedrooms^4)+I(bedrooms^5)
               +bathrooms+I(bathrooms^2)+I(bathrooms^3)+I(bathrooms^4)+I(bathrooms^5)
               +sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)
               +sqft_lot+I(sqft_lot^2)+I(sqft_lot^3)+I(sqft_lot^4)+I(sqft_lot^5)
               +floors+I(floors^2)+I(floors^3)+I(floors^4)+I(floors^5)
               +waterfront
               +view+I(view^2)+I(view^3)+I(view^4)+I(view^5)
               +condition+I(condition^2)+I(condition^3)+I(condition^4)+I(condition^5)
               +grade+I(grade^2)+I(grade^3)+I(grade^4)+I(grade^5)
               +sqft_above+I(sqft_above^2)+I(sqft_above^3)+I(sqft_above^4)+I(sqft_above^5)
               +yr_built+I(yr_built^2)+I(yr_built^3)+I(yr_built^4)+I(yr_built^5)
               +yr_last_renovation+I(yr_last_renovation^2)+I(yr_last_renovation^3)+I(yr_last_renovation^4)+I(yr_last_renovation^5)
               +zipcode+I(zipcode^2)+I(zipcode^3)+I(zipcode^4)+I(zipcode^5)
               +lat+I(lat^2)+I(lat^3)+I(lat^4)+I(lat^5)
               +long+I(long^2)+I(long^3)+I(long^4)+I(long^5)
               +sqft_living15+I(sqft_living15^2)+I(sqft_living15^3)+I(sqft_living15^4)+I(sqft_living^5)
               +sqft_lot15+I(sqft_lot15^2)+I(sqft_lot15^3)+I(sqft_lot^4)+I(sqft_lot^5),
               data=train_set)
step.mod <- step(full.mod, steps=150, trace=1,k=log(n), direction="backward")
formula5 <- step.mod$call$formula

set.seed(17)
glm.fit <- glm(formula5, data=train_set)
cv.error5 <- cv.glm(train_set, glm.fit, K=10, cost=RMSE)$delta[1]
cv.error5


# Define the vector of errors
cv.errors <- c(cv.error1, cv.error2, cv.error3, cv.error4, cv.error5)

# Plot the errors (RMSE) according to the degree of the model
plot(cv.errors, type="l", main="RMSE of the different models", xlab="Degree of the model")

# Define the final model
final_model <- glm(formula5, data = train_set)

# Prediction over the test set with the final model
prediction <- predict(final_model, test_set[,-19])

#Errors with prediction
postResample(10**(prediction),10**(test_set[,19]))



#VERY IMPORTANT! THIS COULD BE THE SOLUTION
# par(mfrow=c(1,1))
# library(glmnet)
# fit = glmnet(as.matrix(train_set[,-19]), as.vector(train_set[,19]))
# plot(fit)
# 
# cvfit = cv.glmnet(as.matrix(train_set[,-19]), as.vector(train_set[,19]))
# plot(cvfit)
# 
# fit$lambda
# fit$call
# 
# predict(fit, as.matrix(test_set[,-19]))
# 
# colnames(kc_house)
# 
# step.mod$call


###########
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = cor(kc_house), col = palette, symm = TRUE)
#############



# we can design a CV with regularization also:
# trained <- list()
# formulas <- list(formula1, formula2, formula3, formula4, formula5)
# best_rmse <- c() # Best RMSE score per formula
# best_r2 <- c() # Best R2 score per formula
# # Train every formula using CV
# for (i in 1:length(formulas)) {
#   trained[[i]] <- train(formulas[[i]], data=train_set, method="glmnet", trControl=fitControl, metric="RMSE")
#   # Save best RMSE
#   best_rmse <- c(best_rmse, min(trained[[i]]$results$RMSE))
#   # Save best R2
#   best_r2 <- c(best_r2, max(trained[[i]]$results$Rsquared))
# }
# 
# par(mfrow=c(1,2))
# # Plot of RMSE
# plot(best_rmse, main="RMSE per formula", xlab="Formula", ylab="RMSE")
# # Plot of R2
# plot(best_r2, main="R2 per formula", xlab="Formula", ylab="R2")
# par(mfrow=c(1,1))

# Training of lm on the whole training set
# best_lm <- lm(trained[[3]], data=train_set)
# Predict values using test set
# pred <- predict(trained[[3]], newdata=test_set)
# obs <- test_set[,19]
# # Compute RMSE
# RMSE(pred, obs)
# # Compute R2
# R2(pred, obs)

# # ensure the results are repeatable
# set.seed(7)
# # define the control using a random forest selection function
# control6 <- rfeControl(functions=rfFuncs, method="cv", number=10)
# # run the RFE algorithm
# results <- rfe(train_set[,-19], train_set[,19], sizes=5, rfeControl=control6)
# # summarize the results
# print(results)
# # list the chosen features
# predictors(results)
# # plot the results
# plot(results, type=c("g", "o"))
# 
# # 
# row.names(importance6[[1]])
# importance6[[1]]
# typeof(importance6[[1]])

# TODO: We want to plot the most important features of our best model


PlotImportance(importance2)
PlotImportance(importance4)
# PlotImportance(importance6)

set.seed(10)


lmProfile$variables
# KCHouseData2 = kc_house %>%
#   select(-date)
# set.seed(13)


PlotImportance(importance)

formula<-price~ .

xgbGrid <- expand.grid(nrounds = 500,
                       max_depth = 4,
                       eta = .05,
                       gamma = 0,
                       colsample_bytree = .5,
                       min_child_weight = 1,
                       subsample = 1)

fitControl <- trainControl(method="cv",number = 10)
t = train(formula, data = train_set,
                            method = "xgbTree",trControl = fitControl,
                            tuneGrid = xgbGrid,na.action = na.pass,metric="RMSE")

importance = varImp(t)

PlotImportance(importance)


