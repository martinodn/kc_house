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
library(leaps)
library(tree)

afillColor = "#FFA07A"
fillColor2 = "#F1C40F"

kc_house<-read.csv("kc_house_data.csv")

#TODO: dove abbiamo preso i dati, spiegare ogni colonna cosa significa

#check the dimension of the dataset
dim(kc_house)

#convert date from string to observation number of the day (starting from 1)
new.date<-as.Date(kc_house$date,'%Y%m%d')
new.date<-new.date-(sort(new.date))[1]
#new.date
#sort(unique(new.date))

#We add this column to the dataset instead of the original one
kc_house[2]<-as.numeric(new.date)
kc_house
# dim(kc_house)

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

kc_house["yr_renovated"]<-ylr
kc_house
colnames(kc_house)[15]<-"yr_last_renovation"
length(kc_house$yr_last_renovation)
#move the target feature (price) to the last column
kc_house<-kc_house[,c("date", "bedrooms", "bathrooms", "sqft_living","sqft_lot", "floors", "waterfront","view","condition",
           "grade", "sqft_above", "sqft_basement", "yr_built", "yr_last_renovation", "zipcode",  "lat", "long","sqft_living15", 
           "sqft_lot15", "price")]

kc_house
################################EXPLORING DATA

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

#We want to see the type of houses we're dealing with; thus, we check the distribution of the grade of the
#houses and we compare it to a normal distribution.
hist(kc_house$grade, breaks =12)
qqnorm(kc_house$grade)
#We can see that the distribution of grade is close to a normal one, with the mean between 7 and 8 (7.65).
#Other feature, like sqft_living, are not distributed as a normal and we can see it by plotting its values.
qqnorm(kc_house$sqft_living)

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

#Plotting the pair plot should give some idea about correlation between our variables
#However, pair plot of all the columns is large and difficult to manage.
#Hence, we proceed by plotting few variables against each other.

# Plotting a simple map: bigger circles mean bigger price
# For more info, check out http://geog.uoregon.edu/bartlein/courses/geog495/lec05.html


# Get colors for labeling the points
plotvar <- house$price # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr, "PuBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
plot.angle <- 340
# 3D Scatter plot
scatterplot3d(kc_house$long, kc_house$lat, plotvar, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=2, 
              col.axis="gray", col.grid="gray", xlab="Longitude", ylab="Latitude", zlab="Price")

##########################REGRESSION MODEL
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

hist(kc_house$sqft_lot)
#we can see that this feature has a really bad distribution, so we can try applying the log10 to it.
kc_house[5]<-log10(kc_house$sqft_lot)
kc_house[18]<-log10(kc_house$sqft_lot15)

hist(kc_house$sqft_lot, breaks=50, main = "Histogram of dimensions of lots")

kc_house[19]<-log10(kc_house[19])
dim(kc_house)
length(kc_house$yr_last_renovation)
# kc_house<-kc_house[kc_house$price < log10(1000000),]
dim(kc_house)


max(kc_house$price)
attach(kc_house)

# length(yr_last_renovation)
# dim(kc_house)



#DATE (cor=-0.00570)
#we notice we have a continuous detection for the dates: almost every day (starting from the initial one)
#there are some house sold and so we don't have time intervals in which values of price are not present, 
#as we can see from this plot.
plot(sort(date))

#we plot the price as a function of the date
plot(price~date, main="Price by date")

#it is not a very good plot, so we can try with the boxplot
boxplot(price~sort(date), main="Price by date")

#to see in a better way the data, we can average for each date the prices of the houses sold in that
#day. Obviously we get an approximation, but this is not a big deal having a lot of examples (about 60 
#per day) and this allows us to see really data in a nicer way.
average_price_bydate <-aggregate(price~date, FUN=mean, data=kc_house)
plot(average_price_bydate, col=1,main="Average price by date")
lines(loess.smooth(date, price), col=2, lty=5)
#we see that the date does not influence so much the price of the house


#BEDROOMS (cor=0.35100546)
boxplot(price~bedrooms, xlab="bedrooms", ylab="price",main="Price by bedrooms")
#we see that the greater the number of bedrooms, the greater the price. Also we can notice that
#there is a big number of outliers in the upper part for values of bedrooms in the range 2-6.

kc_house %>%
  group_by(bedrooms) %>%
  summarise(PriceMedian = median(10**(price), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bedrooms = reorder(bedrooms,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%

  ggplot(aes(x = bedrooms,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = "yellow") +
  geom_text(aes(x = bedrooms, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'bedrooms', 
       y = 'Median Price', 
       title = 'Median price per number of bedrooms') +
  coord_flip() + 
  theme_bw()
#BATHROOMS (cor=0.55082290)
boxplot(price~bathrooms, xlab="bathrooms", ylab="price",main="Price by bathrooms")
#Also in this case, and more than for the previous one, it is clear that if we increase the number of 
#bathrooms, also the price increases.

kc_house %>%
  group_by(bathrooms) %>%
  summarise(PriceMedian = median(10**(price), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bathrooms = reorder(bathrooms,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = bathrooms,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = bathrooms, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'bathrooms', 
       y = 'Median Price', 
       title = 'bathrooms and Median Price') +
  coord_flip() + 
  theme_bw()

#SQFT_LIVING (cor=0.69536476)
plot(price~sqft_living,main="Price by sqft_living")
lines(loess.smooth(sqft_living, price), col=3)


#we can see data better:
average_price_bysqftliving <-aggregate(price~sqft_living, FUN=mean, data=kc_house)
plot(average_price_bysqftliving, col=1,main="Average price by living area")
lines(loess.smooth(sqft_living, price), col=3)
#also in this case there is an evident association between the dimensions of the living area and the price.


#SQFT_LOT (cor=0.09962940)
plot(price~sqft_lot)


#we apply the aggregate function
average_price_bysqftlot <-aggregate(price~sqft_lot, FUN=mean, data=kc_house)
plot(average_price_bysqftlot, col=1,main="Average price by lot dimension")
lines(loess.smooth(sqft_lot, price), col=3)

#sqft_lot feature does not influence so much the target, as we can see from the plot


#FLOORS (cor= 0.31059266)
boxplot(price~floors, xlab="floors", main="Price by floor")
#there is a little increase in the price as floors increase, but not so much


#WATERFRONT (cor=0.17459026)
boxplot(price~waterfront, xlab="waterfront", main="Price by waterfront")
#low correlation


#VIEW (cor= 0.34653430)
boxplot(price~view, xlab="view", main="Price by view")
#We notice that there is a soft correlation between the covariate view and price


#CONDITION (cor= 0.03949428)
boxplot(price~condition, xlab="condition", main="Price by condition")


#GRADE (cor= 0.70366105)
boxplot(price~grade, xlab="grade", main="Price by grade")
#grade has a strong correlation with the target!!

kc_house %>%
  group_by(grade) %>%
  # summarise(PriceMedian = median(10**(price)), na.rm = TRUE) %>%
  summarise(PriceMedian = round(median(10**(price),2)), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(grade = reorder(grade,PriceMedian)) %>%
  arrange(desc(PriceMedian)) %>%
  
  ggplot(aes(x = grade,y = PriceMedian)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = grade, y = 1, label = paste0("(",PriceMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'grade', 
       y = 'Median Price', 
       title = 'grade and Median Price') +
  coord_flip() + 
  theme_bw()

#SQFT_ABOVE(cor=0.60184347)
plot(price~sqft_above)
#not so nice... we can see it better
average_price_bysqftabove <-aggregate(price~sqft_above, FUN=mean, data=kc_house)
plot(average_price_bysqftabove, col=1,main="Average price by above dimension")
lines(loess.smooth(sqft_above, price), col=5)
#price is really increasing as sqft_above increases!

#YR_BUILT (cor=0.08067957)
plot(price~yr_built)
lines(loess.smooth(yr_built, price), col=5)

average_price_yb <-aggregate(price~yr_built, FUN=mean, data=kc_house)
plot(average_price_yb, col=1,main="Average price by construction year")
lines(loess.smooth(yr_built, price), col=5)

#YR_LAST_RENOVATION (cor=0.13032651)
plot(price~yr_last_renovation)
kc_house
length(yr_last_renovation)

average_price_ylr <-aggregate(price~yr_last_renovation, FUN=mean, data=kc_house)
plot(average_price_ylr, col=1,main="Average price by above dimension")
lines(loess.smooth(yr_last_renovation, price), col=5)

#ZIPCODE (cor=0.03831967)
plot(price~zipcode)
lines(loess.smooth(zipcode, price), col=5)

average_price_zc <-aggregate(price~zipcode, FUN=mean, data=kc_house)
plot(average_price_zc, col=1,main="Average price by zipcode")
lines(loess.smooth(zipcode, price), col=5)

#LAT (cor=0.44916082)
plot(price~lat)
lines(loess.smooth(lat, price), col=5)

average_price_lat <-aggregate(price~lat, FUN=mean, data=kc_house)
plot(average_price_lat, col=1,main="Average price by lat")
lines(loess.smooth(lat, price), col=5)

#can be seen a quite strong correlation

#LONG (cor=0.04996692)
plot(price~long)

average_price_long <-aggregate(price~long, FUN=mean, data=kc_house)
plot(average_price_long, col=1,main="Average price by long")
lines(loess.smooth(long, price), col=5)
#not so significant the long

#We plot the houses in the map
bins<-cut(kc_house$price, c(0,log10(250e3),log10(500e3),log10(750e3),log10(1e6),log10(2e6),log10(999e6)))

center_lon = median(kc_house$long,na.rm = TRUE)
center_lat = median(kc_house$lat,na.rm = TRUE)

factpal <- colorFactor(c("black","blue","yellow","orange","#0B5345","red"), 
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
PriceGroup6 = PriceBinGrouping(log10(2e6),log10(999e6))

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
MapPriceGroups(PriceGroup6,"red")


#ZIPCODE in function of latitude and longitude
#Not that zipcode is an integer value, but is referred to a discrete variable
#First we are interested to know how many zipcodes are there
length(unique(zipcode)) # There are 70 zipcodes
# Define a plot coloured with respect to zipcode
library(randomcoloR)
set.seed(3)
palette <- randomColor(length(unique(zipcode)))
plt <- ggplot(kc_house, aes(x=long, y=lat, col=as.factor(zipcode)) ) 
plt <- plt + geom_point()
plt <- plt + theme(legend.position='none')
plt <- plt + scale_colour_manual(values=palette)
plt

#SQFT_LIVING_15 (cor=0.61935746)
plot(price~sqft_living15)
average_price_l15 <-aggregate(price~sqft_living15, FUN=mean, data=kc_house)
plot(average_price_l15, col=1,main="Average price by sqft_living15")
lines(loess.smooth(sqft_living15, price), col=5)
#really significant!

#SQFT_LOT15 (cor=0.09160121)
plot(price~log10(sqft_lot15))

average_price_lot15 <-aggregate(price~sqft_lot15, FUN=mean, data=kc_house)
plot(average_price_lot15, col=1,main="Average price by sqft_lot15")
lines(loess.smooth(sqft_lot15, price), col=5)
cor(kc_house)



#BACKWARD VARIABLE SELECTION


#Define the random seed (otherwise we cannot repeat exactly the same experiment)
set.seed(29)
#try to evaluate performance of different models splitting the whole set into training-validation-test set
id.train<-createDataPartition(price, p=.80, list=FALSE)
train_set<-kc_house[id.train,] #80
test_set<-kc_house[-id.train,] #20

set.seed(29)
id.val<-createDataPartition(train_set$price, p=.15, list=FALSE)

# id.test<-createDataPartition(price, p=.20, list=FALSE)

val_set<-train_set[id.val,] 
train_set<-train_set[-id.val,] 

val_set_X<-val_set[,-19]
val_set_y<-val_set[,19]
test_set_X<-test_set[,-19]
test_set_y<-test_set[,19]

RMSE_values=c()



#model1 (linear model with grade 1)
model1<-lm(price ~ . -sqft_above -sqft_lot, data=train_set)
summary(model1)
# car::vif(model1)
pred1<-predict(model1, newdata=val_set_X)
postResample(10**(pred1), 10**(val_set_y))
r1<-RMSE(10**(pred1), 10**(val_set_y))
RMSE_values=c(RMSE_values,r1)
dim(kc_house)
# plot(model1)

#we don't want to cut off the intercept, so we keep it!


#RMSE can also be calculated as:
sqrt(mean((10**(pred1)-10**(val_set_y))**2))

# Linear model of grade 2
model2<-lm(price ~  date + I(date^2)
          + I(bedrooms^2)
          + bathrooms
          + sqft_living + I(sqft_living^2)
          + sqft_lot + I(sqft_lot^2)
          + floors
          + waterfront 
          + view
          + condition
          + grade
          + sqft_above
          + yr_built + I(yr_built^2)
          + yr_last_renovation + I(yr_last_renovation^2)
          + zipcode
          + lat + I(lat^2)
          + long + I(long^2)
          + sqft_living15 + I(sqft_living15^2)
          + sqft_lot15,
          data=train_set)

summary(model2)
pred2<-predict(model2, val_set_X)
postResample(10**(pred2), 10**(val_set_y))
r2<-RMSE(10**(pred2),10**(val_set_y))
r2
RMSE_values=c(RMSE_values,r2)
#with this graph we have the evidence that we can use a linear model to fit the data

#plot(model2)
hist(model2$residuals, breaks = 100)
# anova(model1,model2, test="Chisq")

#polynomial model grade 3
# model3<-lm(price~ date+I(date^2)+ I(date^3)+bedrooms+I(bedrooms^2) +I(bedrooms^3) + bathrooms + I(bathrooms^2)+
#              I(bathrooms^3)+sqft_living+I(sqft_living^2)+I(sqft_living^3)+sqft_lot+I(sqft_lot^2)+I(sqft_lot^3)+
#              floors+I(floors^2)+I(floors^3)+waterfront+I(waterfront^2)+I(waterfront^3)+view+I(view^2)+I(view^3)+condition+I(condition^2)+
#              I(condition^3)+grade+I(grade^2)+I(grade^3)+sqft_above+I(sqft_above^2)+I(sqft_above^3)+yr_built+I(yr_built^2)+I(yr_built^3)+
#              yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+lat+I(lat^2)+I(lat^3)+long+I(long^2)+I(long^3)+sqft_living15+I(sqft_living15^2)+I(sqft_living15^3)+sqft_lot15+
#              I(sqft_lot15^2)+I(sqft_lot15^3),data=train_set)
# summary(model3)

#remove 1 by 1 the covariates (we report only the final model)
# formula.3 <- "price ~ I(date^3)  + I(bedrooms^2) + I(bathrooms^2) + sqft_living + I(sqft_living^2) + sqft_lot +
#              + waterfront + view + I(view^2) + I(view^3) + I(condition^2) +grade + sqft_above + I(sqft_above^2) + I(sqft_above^3) + yr_built + I(yr_built^2) + I(yr_built^3) +
#              yr_last_renovation + I(yr_last_renovation^2) + zipcode + lat + I(lat^2) + long + I(long^2) + I(sqft_living15^2) + I(sqft_living15^3) + 
#              I(sqft_lot15^2) + I(sqft_lot15^3)"
# model3<-lm(as.formula(formula.3),data=train_set)
# summary(model3)
# anova(model2,model3)
# pred3<-predict(model3, newdata=val_set_X)
# postResample(pred3, val_set_y)
# 
# RMSE(10**(pred3),10**(val_set_y))
# plot(val_set_y,pred3,  xlim=c(0,3000),ylim=c(0,3000))

# dim(cor(kc_house)[,19])

model3<-lm(price ~ I(date^3)
          + I(bedrooms^2)
          + bathrooms
          + sqft_living + I(sqft_living^3)
          + sqft_lot + I(sqft_lot^2) + I(sqft_lot^3)
          + I(floors^2) + I(floors^3)
          + waterfront 
          + view + I(view^2) + I(view^3)
          + condition
          + I(grade^2) + I(grade^3)
          + sqft_above + I(sqft_above^2) + I(sqft_above^3)
          + yr_built + I(yr_built^2) + I(yr_built^3)
          + I(yr_last_renovation^2) + I(yr_last_renovation^3)
          + I(zipcode^2)
          + lat + I(lat^2)
          + long + I(long^2)
          + sqft_living15 + I(sqft_living15^2) + I(sqft_living15^3),
          data=train_set)

summary(model3)
pred3<-predict(model3, newdata=val_set_X)
postResample(10**(pred3), 10**(val_set_y))

r3<-RMSE(10**(pred3),10**(val_set_y))
# anova(model2,model3, test="Chisq")
RMSE_values=c(RMSE_values,r3)

# TO BE ADDED!!!
# pairs(kc_house, columns=c("date","bedrooms","bathrooms","sqft_living","sqft_above","price"))

model4<-lm(price ~ I(date^3)
          + I(bedrooms^3)
          + bathrooms + I(bathrooms^2) + I(bathrooms^3) + I(bathrooms^4)
          + sqft_living + I(sqft_living^2)
          + sqft_lot + I(sqft_lot^2) + I(sqft_lot^3)
          + floors
          + waterfront 
          + view + I(view^2) + I(view^3)
          + I(condition^2) + I(condition^3)
          + grade + I(grade^2) + I(grade^3) + I(grade^4)
          + sqft_above + I(sqft_above^2) + I(sqft_above^3)
          + yr_built + I(yr_built^2) + I(yr_built^3)
          + I(yr_last_renovation^2) + I(yr_last_renovation^3)
          + zipcode
          + lat + I(lat^2) + I(lat^4)
          + long + I(long^2)
          + sqft_living15 + I(sqft_living15^2)
          + sqft_lot15 + I(sqft_lot15^2) + I(sqft_lot15^3) + I(sqft_lot15^4),
          data=train_set)
summary(model4)
pred4<-predict(model4, newdata=val_set_X)
postResample(10**(pred4),10** (val_set_y))
r4<-RMSE(10**(pred4),10** (val_set_y))
RMSE_values=c(RMSE_values,r4)
# anova(model3,model4, test="Chisq")

#diagnostic plots
par(mfrow=c(2,2))
plot(model4)





model5<-lm(price~ I(date^4)
           + I(bedrooms^3) 
           + bathrooms  
           + sqft_living + I(sqft_living^2) + I(sqft_living^3) + I(sqft_living^4) + I(sqft_living^5)
           + I(sqft_lot^2) + I(sqft_lot^3) + I(sqft_lot^4) 
           +  I(floors^2) + I(floors^3) 
           + waterfront 
           + view + I(view^2) + I(view^3) 
           + I(condition^2) + I(condition^3)
           + I(grade^2) + I(grade^3) 
           + sqft_above + I(sqft_above^2)  + I(sqft_above^5)
           + yr_built + I(yr_built^2) + I(yr_built^3)
           + I(yr_last_renovation^2) + I(yr_last_renovation^3) 
           + I(zipcode^2) 
           + lat + I(lat^2) + I(lat^4) 
           + long + I(long^2) 
           + sqft_living15 + I(sqft_living15^2)
           + sqft_lot15 + I(sqft_lot15^2) + I(sqft_lot15^3) + I(sqft_lot15^4), 
           data=train_set)

summary(model5)
pred5<-predict(model5, newdata=val_set_X)
postResample(10**(pred5),10**(val_set_y))
r5<-RMSE(10**(pred5),10**(val_set_y))
RMSE_values=c(RMSE_values,r5)


model6<-lm(price~poly(date,6)+
            poly(bedrooms,6)+
            poly(bathrooms,6)+
            poly(sqft_living,6)+
            poly(sqft_lot,6)+
            poly(floors,5)+
            waterfront+
            poly(view,4)+
            poly(condition,4)+
            poly(grade,6)+
            poly(sqft_above,6)+
            poly(yr_built,6)+
            poly(yr_last_renovation,6)+
            poly(zipcode,6)+
            poly(lat,6)+
            poly(long,6)+
            poly(sqft_living15,6)+
            poly(sqft_lot15,6),
            data=train_set)

summary(model6)
pred6<-predict(model6, newdata=val_set_X)
postResample(10**(pred6),10**(val_set_y))
# anova(model5,model6)
r6<-RMSE(10**(pred6),10**(val_set_y))
RMSE_values=c(RMSE_values,r6)
RMSE_values
plot(RMSE_values)
#we stop with the polynomial 5 because we see that the we have a lower adjusted R-squared and also the RMSE on the
#previously unseen data is worse, so this can be due to overfitting of the training set.




corrplot(cor(kc_house))
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = cor(kc_house), col = palette, symm = TRUE)

formula = price ~ .-sqft_above -sqft_lot
fitControl <- trainControl(method="cv",number = 15)
KCHouseDataModel = train(formula, data = train_set, method = "lm",trControl = fitControl,metric="RMSE")
importance = varImp(KCHouseDataModel)

# we can design a CV with regularization also:
formulax = price ~ .
fitControlx <- trainControl(method="cv",number = 10)
kc_houseX = train(formulax, data = train_set, method = "glmnet",trControl = fitControlx,metric="RMSE")

#####

formula2<-price~ date + I(date^2)+ I(bedrooms^2)+ bathrooms+
                  + sqft_living + I(sqft_living^2)+
                  + sqft_lot + I(sqft_lot^2)+
                  + floors+
                  + waterfront +
                  + view+
                  + condition+
                  + grade+
                  + sqft_above+
                  + yr_built + I(yr_built^2)+
                  + yr_last_renovation + I(yr_last_renovation^2)+
                  + zipcode+
                  + lat + I(lat^2)+
                  + long + I(long^2)+
                  + sqft_living15 + I(sqft_living15^2)+
                  + sqft_lot15

fitControl2 <- trainControl(method="cv",number = 10)
KCHouseDataModel2 = train(formula2, data = train_set, method = "lm",trControl = fitControl2,metric="RMSE")
KCHouseDataModel2
importance2<-varImp(KCHouseDataModel2)


formula4<-price ~ I(date^3)+ I(bedrooms^3)+ bathrooms + I(bathrooms^2) + I(bathrooms^3) + I(bathrooms^4)+ 
                  sqft_living + I(sqft_living^2)+ sqft_lot + I(sqft_lot^2) + I(sqft_lot^3)+ floors+ waterfront + view + I(view^2) +
                  + I(view^3)+ I(condition^2) + I(condition^3)+
                  + grade + I(grade^2) + I(grade^3) + I(grade^4)+
                  + sqft_above + I(sqft_above^2) + I(sqft_above^3)+
                  + yr_built + I(yr_built^2) + I(yr_built^3)+
                  + I(yr_last_renovation^2) + I(yr_last_renovation^3)+
                  + zipcode+
                  + lat + I(lat^2) + I(lat^4)+
                  + long + I(long^2)+
                  + sqft_living15 + I(sqft_living15^2)+
                  + sqft_lot15 + I(sqft_lot15^2) + I(sqft_lot15^3) + I(sqft_lot15^4)
                  

fitControl4 <- trainControl(method="cv",number = 10)

KCHouseDataModel4 = train(formula4, data = train_set,
                         method = "lm",trControl = fitControl4,metric="RMSE")

KCHouseDataModel4

importance4 = varImp(KCHouseDataModel4)

formula6<-price~poly(date,6)+
  poly(bedrooms,6)+
  poly(bathrooms,6)+
  poly(sqft_living,6)+
  poly(sqft_lot,6)+
  poly(floors,5)+
  waterfront+
  poly(view,4)+
  poly(condition,4)+
  poly(grade,6)+
  poly(sqft_above,6)+
  poly(yr_built,6)+
  poly(yr_last_renovation,6)+
  poly(zipcode,6)+
  poly(lat,6)+
  poly(long,6)+
  poly(sqft_living15,6)+
  poly(sqft_lot15,6)
fitControl6 <- trainControl(method="cv",number = 10)
KCHouseDataModel6 = train(formula6, data = train_set, method = "lm",trControl = fitControl6,metric="RMSE")
importance6<- varImp(KCHouseDataModel6)

# 
# row.names(importance6[[1]])
# importance6[[1]]
# typeof(importance6[[1]])
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

PlotImportance(importance)
PlotImportance(importance2)
PlotImportance(importance4)
# PlotImportance(importance6)


KCHouseData2 = kc_house %>%
  select(-date)
set.seed(13)


PlotImportance(importance)
# 
# PCAData = kc_house %>%
#   select(lat,long)
# 
# pca = prcomp(PCAData, scale. = T)
# 
# KCHouseData_pca <- predict(pca, newdata = PCAData)
# 
# KCHouseData_pca = as.data.frame(KCHouseData_pca)
# 
# KCHouseData2 = cbind(KCHouseData2,KCHouseData_pca)
# 
# dim(KCHouseData2)
# kc_house %>% 
#   filter(!is.na(lat)) %>% 
#   filter(!is.na(long)) %>% 
#   
#   ggplot(aes(x=lat,y=long))+
#   geom_point(color = "blue")+
#   
#   theme_bw()+
#   theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
#   xlab("Latitude")+
#   ylab("Longitude")
# 
# pca = prcomp(PCAData, scale. = T)
# 
# biplot (pca , scale =0)

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

importance = varImp(KCHouseDataModelXGB)



##############ANALYSIS WITHOUT OUTLIERS maderfuckerzzz







##############################################THE UNTOUCHABLE ZONE!!!! ALERT!!!!! DANGER!!!!

# Plot of explanatory values wrt price in form of a Tree in order to extract most important interactions
model <- tree(price~., data=kc_house)
plot(model) 
text(model)
# Most explanatory variable is grade, followed by sqft_living and latitude
# Hence we create a model conidering only first grade variables plus the interactions
model7 <- lm(price ~ . + (grade + lat + sqft_living)^2, data=train_set)
summary(model7)
# We execute a proper backward selection, taking into account the p-vale
model7 <- update(model7, . ~ . -sqft_lot -grade:lat -sqft_living:lat -sqft_above)
summary(model7)
# Validation
pred7<-predict(model7, newdata=val_set_X)
RMSE(10^pred7, 10^val_set_y) # 180381.8
R2(10^pred7, 10^val_set_y) # 0.7668159

# # Model 7: takes into account every interaction between variables
# rhs <- paste(colnames(train)[-19], collapse=' + ')
# rhs <- paste('(', rhs, ')^2', sep='')
# lhs <- 'price ~'
# formula7 <- as.formula(paste(lhs, rhs))
# model7 <- lm(formula7, data=train)
# summary(model7)
# pred7<-predict(model7, newdata=val_set_X)
# RMSE(10^pred7, 10^val_set_y)
# R2(10^pred7, 10^val_set_y)
# # Execute automatic feature selection
# regfit.full <- regsubsets(formula7, method="backward", data=train, really.big=T)
# summary(regfit.full)
# 
# reg.summary <- summary(regfit.full)
# 
# # elements of reg.summary
# names(reg.summary)
# 
# # R^2 statistic for the best model of every subset group
# reg.summary$rsq
# reg.summary$bic
# 
# #
# # second group of plots 
# #
# 
# plot(regfit.full,scale="r2")
# plot(regfit.full,scale="adjr2")
# plot(regfit.full,scale="Cp")
# plot(regfit.full,scale="bic")
# 
# # Cp best
# coef(regfit.full,8)
# 
# # BIC best
# coef(regfit.full,4)
# 
# #
# # first group of plots 
# #
# par(mfrow=c(2,2))
# 
# # panel 1
# plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
# 
# # panel 2
# plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
# which.max(reg.summary$adjr2)
# points(7,reg.summary$adjr2[7], col="red",cex=2,pch=20)
# 
# # panel 3
# plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
# which.min(reg.summary$cp)
# points(6,reg.summary$cp[6],col="red",cex=2,pch=20)
# 
# # panel 4
# plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
# which.min(reg.summary$bic)
# points(4,reg.summary$bic[4],col="red",cex=2,pch=20)
# 
# par(mfrow=c(1,1))

#Splitting the whole dataset into training and test set
#Define training indexes
set.seed(29)
idx.train<-createDataPartition(kc_house$price, p=.80, list=F)
#Define train and test subsets
train<-kc_house[idx.train,]
test<-kc_house[-idx.train,]
#Check length of train and test set (percentage)
dim(train)[1]/dim(kc_house)[1]
dim(test)[1]/dim(kc_house)[1]
#Check price values in train set
summary(train$price)
#Check price values in test set
summary(test$price)

#Cross-Validation (using previously splitted database)
#Define k for k-fold cross-validation
k<-10
#Define an array of formula, to be used in model training
m<-list(model1, model2, model3, model4, model5)
# Define a matrix with k rows and p columns for RMSE
cv.rmse<-matrix(nrow=k, ncol=length(m))
# Define a matrix with k rows and p columns for R^2
cv.rsquared<-matrix(nrow=k, ncol=length(m))
#Split train data in K-fold split
set.seed(30)
folds<-createFolds(train$price, k=k, list=FALSE, returnTrain=FALSE)
#Loop through every fold
for (i in 1:k) {
  #Get validation set for i-th iteration
  idx.valid<-which(folds==i, arr.ind=TRUE)
  #Loops through every grade of the polynomial specified
  for (j in 1:length(m)) {
    #Get validation set
    cv.valid<-train[idx.valid,]
    #Get training set, without validation set
    cv.train<-train[-idx.valid,]
    #Train the model using training set
    model<-update(m[[j]], data=cv.train)
    #Predict values using validation set (without price column)
    cv.predicted<-predict(model, newdata=cv.valid)
    #Add prediction scores to the matrices
    cv.rmse[i,j] <- RMSE(10^cv.predicted, 10^cv.valid[,19])
    cv.rsquared[i,j] <- R2(10^cv.predicted, 10^cv.valid[,19])
  }
}

par(mfrow=c(1,1))
# Plot the RMSE at every iteration
plot(cv.rmse[,1], type="l", col="red", ylim=c(0, 800000)) # 1st model
lines(cv.rmse[,2], col="blue") #2nd model
lines(cv.rmse[,3], col="yellow") # 3rd mode
lines(cv.rmse[,4], col="orange") # 4th model
lines(cv.rmse[,5], col="purple") # 5th model
lines(cv.rmse[,6], col="green") # 6th model
lines(cv.rmse[,7], col="cyan") # 7th model

# Plot the R2 at every iteration
plot(cv.rsquared[,1], type="l", col="red", ylim=c(0.3,1)) # 1st model
lines(cv.rsquared[,2], col="blue") #2nd model
lines(cv.rsquared[,3], col="yellow") # 3rd mode
lines(cv.rsquared[,4], col="orange") # 4th model
lines(cv.rsquared[,5], col="purple") # 5th model
lines(cv.rsquared[,6], col="green") # 6th model
lines(cv.rsquared[,7], col="cyan") # 7th model

#Initialize mean values for prediction scores
cv.mean.rmse<-c()
cv.mean.rsquared<-c()
#Compute the average scores on every fold, for every model
for (j in 1:length(m)) {
  cv.mean.rmse<-c(cv.mean.rmse, mean(cv.rmse[,j]))
  cv.mean.rsquared<-c(cv.mean.rsquared, mean(cv.rsquared[,j]))
}
# Show averaged results
cv.mean.rmse
cv.mean.rsquared

