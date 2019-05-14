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

# import required libraries
library(ggplot2)
library(ggmap)

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
# First import libraries
library(scatterplot3d)
library(RColorBrewer)
# Get colors for labeling the points
plotvar <- kc_house$price # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr, "PuBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
# 3D Scatter plot
plot.angle <- 340
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

hist(sqft_lot)
#we can see that this feature has a really bad distribution, so we can try applying the log10 to it.
kc_house[5]<-log10(kc_house$sqft_lot)
kc_house[18]<-log10(kc_house$sqft_lot15)

hist(kc_house$sqft_lot, breaks=50, main = "Histogram of dimensions of lots")

kc_house[19]<-log10(kc_house[19])
dim(kc_house)
length(kc_house$yr_last_renovation)

attach(kc_house)
# length(yr_last_renovation)
# dim(kc_house)

#Import caret library
library(caret)
#Define the random seed (otherwise we cannot repeat exactly the same experiment)
set.seed(42)
#try to evaluate performance of different models splitting the whole set into training-validation-test set


id.train<-createDataPartition(price, p=.80, list=FALSE)
train_set<-kc_house[id.train,] #80
test_set<-kc_house[-id.train,] #20
# 
id.val<-createDataPartition(train_set$price, p=.15, list=FALSE)
# id.test<-createDataPartition(price, p=.20, list=FALSE)
val_set<-train_set[id.val,] 
train_set<-train_set[-id.val,] 

val_set_X<-val_set[,-19]
val_set_y<-val_set[,19]
test_set_X<-test_set[,-19]
test_set_y<-test_set[,19]

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

#BATHROOMS (cor=0.55082290)
boxplot(price~bathrooms, xlab="bathrooms", ylab="price",main="Price by bathrooms")
#Also in this case, and more than for the previous one, it is clear that if we increase the number of 
#bathrooms, also the price increases.

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
#model1 (linear model with grade 1)
model1<-lm(price ~ .-floors -sqft_lot -sqft_lot15, data=train_set)
summary(model1)
pred1<-predict(model1, newdata=val_set_X)

plot(model1)
#we don't want to cut off the intercept, so we keep it!
postResample(pred1, val_set_y)

#RMSE can also be calculated as:
sqrt(mean((10**(pred1)-10**(val_set_y))**2))


#model2 (linear model with grade 2)
formula.2 <- "price ~ date + I(date^2)  + I(bedrooms^2) + I(bathrooms^2) +
             sqft_living + I(sqft_living^2) + I(sqft_lot^2) +
             waterfront + view + condition + grade + I(grade^2)  + I(yr_built^2) +
             yr_last_renovation + I(yr_last_renovation^2)  + I(zipcode^2) +
             lat + I(lat^2) + long + I(long^2) + sqft_living15"
model2<-lm(as.formula(formula.2), data=train_set)
summary(model2)
anova(model1,model2)
#with this graph we have the evidence that we can use a linear model to fit the data


hist(model2$residuals, breaks = 100)
pred2<-predict(model2, val_set_X)
postResample(pred2, val_set_y)
RMSE(10**(pred2),10**(val_set_y))
#the residual standard error of the model2 can be calculated also as:
sqrt(473738789/14020)
#polynomial model grade 3
# model3<-lm(price~ date+I(date^2)+ I(date^3)+bedrooms+I(bedrooms^2) +I(bedrooms^3) + bathrooms + I(bathrooms^2)+
#              I(bathrooms^3)+sqft_living+I(sqft_living^2)+I(sqft_living^3)+sqft_lot+I(sqft_lot^2)+I(sqft_lot^3)+
#              floors+I(floors^2)+I(floors^3)+waterfront+I(waterfront^2)+I(waterfront^3)+view+I(view^2)+I(view^3)+condition+I(condition^2)+
#              I(condition^3)+grade+I(grade^2)+I(grade^3)+sqft_above+I(sqft_above^2)+I(sqft_above^3)+yr_built+I(yr_built^2)+I(yr_built^3)+
#              yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+lat+I(lat^2)+I(lat^3)+long+I(long^2)+I(long^3)+sqft_living15+I(sqft_living15^2)+I(sqft_living15^3)+sqft_lot15+
#              I(sqft_lot15^2)+I(sqft_lot15^3),data=train_set)
# summary(model3)

#remove 1 by 1 the covariates (we report only the final model)
formula.3 <- "price ~ I(date^3)  + I(bedrooms^2) + I(bathrooms^2) + sqft_living + I(sqft_living^2) + sqft_lot +
             + waterfront + view + I(view^2) + I(view^3) + I(condition^2) +grade + sqft_above + I(sqft_above^2) + I(sqft_above^3) + yr_built + I(yr_built^2) + I(yr_built^3) +
             yr_last_renovation + I(yr_last_renovation^2) + zipcode + lat + I(lat^2) + long + I(long^2) + I(sqft_living15^2) + I(sqft_living15^3) + 
             I(sqft_lot15^2) + I(sqft_lot15^3)"
model3<-lm(as.formula(formula.3),data=train_set)
summary(model3)
anova(model2,model3)
pred3<-predict(model3, newdata=val_set_X)
postResample(pred3, val_set_y)

RMSE(10**(pred3),10**(val_set_y))
# plot(val_set_y,pred3,  xlim=c(0,3000),ylim=c(0,3000))

dim(cor(kc_house)[,19])
pairs(kc_house, columns=c("date","bedrooms","bathrooms","sqft_living","sqft_above","price"))


#work in progress... using the function "poly" to simplify sintax
# model4<-lm(price~ poly(date,4)+poly(bedrooms,4) + poly(bathrooms,4) + poly(sqft_living,4) +
#              poly(sqft_lot,4) + poly(floors,4) +
#              waterfront +poly(view,4) +poly(condition,4) +poly(grade,4) +poly(sqft_above,4) +poly(yr_built,4) +
#              poly(yr_last_renovation,4) +poly(zipcode,4) +poly(lat,4) +poly(long,4) +poly(sqft_living15 ,4) +poly(sqft_lot15,4),data=train_set)
# summary(model4)
formula.4 <- "price~ I(date^4)+ I(bedrooms^3)+  bathrooms+ sqft_living+I(sqft_living^4) +
             poly(sqft_lot,1) + floors + waterfront +view +I(view^3)+I(view^4) + poly(condition,1) +
             +grade + poly(sqft_above,3) + I(yr_built^2) +
             +poly(yr_last_renovation,3) + poly(zipcode,1) + poly(lat,4) + poly(long,4) + sqft_living15 + I(sqft_living15^2) +I(sqft_living15^4)"
model4<-lm(as.formula(formula.4),data=train_set)
summary(model4)
anova(model3,model4)
pred4<-predict(model4, newdata=val_set_X)
postResample(pred4, val_set_y)



library(corrplot)


corrplot(cor(kc_house))
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = cor(kc_house), col = palette, symm = TRUE)



##############ANALYSIS WITHOUT OUTLIERS maderfuckerzzz







##############################################THE UNTOUCHABLE ZONE!!!! ALERT!!!!! DANGER!!!!
# models=c(model1,model2,model3,model4)
# results=c()
# i=0
# for(model in models){
#   i=i+1
#   pred_i<-predict(model, newdata=val_set_X)
#   results[i]=postResample(pred4, val_set_y)
# }
# train_set<-as.data.frame(train_set)
# model5<-lm(price ~ poly(train_set,4),data=train_set)

#plot every feature vs price (target)


#Splitting the whole dataset into training and test set
#Define training indexes
idx.train<-createDataPartition(kc_house$price, p=.80, list=FALSE)
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
f<-c(formula.1, formula.2, formula.3, formula.4)
# Define a matrix with k rows and p columns for RMSE
cv.rmse<-matrix(nrow=k, ncol=length(models))
# Define a matrix with k rows and p columns for R^2
cv.rsquared<-matrix(nrow=k, ncol=length(models))
#Split train data in K-fold split
folds<-createFolds(train$price, k=k, list=FALSE, returnTrain=FALSE)
#Loop through every fold
for (i in 1:k) {
  #Get validation set for i-th iteration
  idx.valid<-which(folds==i, arr.ind=TRUE)
  #Loops through every grade of the polynomial specified
  for (j in 1:length(f)) {
    #Get validation set
    cv.valid<-train[idx.valid,]
    #Get training set, without validation set
    cv.train<-train[-idx.valid,]
    #Train the model using training set
    model<-lm(data=cv.train, formula=as.formula(f[j]))
    #Predict values using validation set (without price column)
    cv.predicted<-predict(model, newdata=cv.valid[-19])
    #Add prediction scores to the matrices
    cv.rmse[i,j]<-RMSE(cv.predicted, cv.valid[19])
    cv.rsquared[i,j]<-R2(cv.predicted, cv.valid[19])
  }
}
#Initialize mean values for prediction scores
cv.mean.rmse<-c()
cv.mean.rsquared<-c()
#Compute the average scores on every fold, for every model
for (j in 1:length(f)) {
  cv.mean.rmse<-c(cv.mean.rmse, mean(cv.rmse[,j]))
  cv.mean.rsquared<-c(cv.mean.rsquared, mean(cv.rsquared[,j]))
}
# Show averaged results
cv.mean.rmse
cv.mean.rsquared
# The best model is the 4-th, by looking at rmse and r-squared
# Hence, we retrain the best model on the whole dataset
model <- lm(data=train, formula=as.formula(formula.4))
summary(model)
# Test the best model on the test set
predicted <- predict(model, newdata=test[-19])
# Compute scores on test set
RMSE(predicted, test[19])
R2(predicted, test[19])

