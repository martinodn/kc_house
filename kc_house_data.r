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
yr_last_renovation<-kc_house$yr_renovated
yr_last_renovation[mask]<-kc_house$yr_built[mask]
yr_last_renovation

kc_house["yr_renovated"]<-yr_last_renovation

colnames(kc_house)[15]<-"yr_last_renovation"

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
# 
# model<-glm(price ~ .-sqft_basement, data=kc_house)
# summary(model)
# 
# residuals(model)
# hist(residuals(model), breaks = 200)
# qqnorm(residuals(model))
# 
# model<-lm(price ~ .-sqft_basement-floors, data=kc_house)
# summary(model)


#try to standardize the price to see the difference (CUT OFF THIS PART)
#new.price<-((price-mean(price))/sd(price))
# new.price
# plot(new.price)
# kc_house[20]<-new.price
# kc_house
# model<-lm(price ~ .-sqft_basement, data=kc_house)
# summary(model)

#we delete the column sqft_basement because it add no infos and it can be add in future if we want (lossless delete)
kc_house<-kc_house[,-12]

attach(kc_house)


# kc_house[,19]
# 
# test_id = sample(1:dim(kc_house)[1], size = 0.25*dim(kc_house)[1])
# Test =kc_house[test_id ,] #Test dataset 25% of total
# Training = kc_house[-test_id ,] #Train dataset 75% of total
# 
# library(GGally)
# library(ggplot2)
# p1<-ggpairs(data=Training, columns = c(1:5,19), axisLabels = "show")
# p1
# 
# kc_house
# 
# p2<-ggpairs(data=Training, columns = c(6:10,19), axisLabels = "show")
# p2
# 
# p2<-ggpairs(data=Training, columns = c(6:10,19), axisLabels = "show")
# p2
# 
# p3<-ggpairs(data=Training, columns = c(11:15,19), axisLabels = "show")
# p3
# 
# p4<-ggpairs(data=Training, columns = c(16:18,19), axisLabels = "show")
# p4
# 
# boxplot1=boxplot(price~sqft_living, data=Training, 
#                  col=(c("gold","darkgreen")),
#                  main="Price vs. Sqft_living", xlab="Sqft_living", ylab="Price")
# 
# boxplot2=boxplot(price~bathrooms, data=Training, 
#                  col=(c("gold","darkgreen")),
#                  main="Price vs. Bathrooms", xlab="Bathrooms", ylab="Price")
# 
# boxplot3=boxplot(price~grade, data=Training, 
#                  col=(c("gold","darkgreen")),
#                  main="Price vs. Grade", xlab="Grade", ylab="Price")
# 
# boxplot4=boxplot(price~view, data=Training, 
#                  col=(c("gold","darkgreen")),
#                 main="Price vs. View", xlab="View", ylab="Price")
# 
# boxplot5=boxplot(price~lat, data=Training, 
#                  col=(c("gold","darkgreen")),
#                  main="Price vs. Lat", xlab="Lat", ylab="Price")
# 
# boxplot6=boxplot(price~sqft_above, data=Training, 
#                  col=(c("gold","darkgreen")),
#                  main="Price vs. sqft_above", xlab="Lat", ylab="Price")
# 
# #those variables has quite strong correlation with the output
# cor(kc_house[,c(3,4,8,10,15,19)])
# 
# cor(kc_house)[,19]
# 
# plot(sqft_living,price, main="Sqft_Living vs. Price of House", xlab="Sqft_Living", ylab="Price of House", pch=19)
# 
# 
# 
# linear_model<-lm(price~sqft_living, data=Training)
# 



#################
# kc_house<-kc_house[price<1000000,]
kc_house[19]<-(kc_house[19]/1000)


attach(kc_house)


#Import caret library
library(caret)
#Define the random seed (otherwise we cannot repeat exactly the same experiment)
set.seed(42)
#try to evaluate performance of different models splitting the whole set into training-validation-test set


id.train<-createDataPartition(price, p=.65, list=FALSE)
id.val<-createDataPartition(price, p=.15, list=FALSE)
id.test<-createDataPartition(price, p=.20, list=FALSE)

train_set<-kc_house[id.train,]
val_set_X<-kc_house[id.val,-19]
val_set_y<-kc_house[id.val,19]
test_set_X<-kc_house[id.test,-19]
test_set_y<-kc_house[id.test,19]

#we train model1 (linear model with grade 1)
#BACKWARD variable selection

# model1<-lm(price~ ., data=train_set)
# summary(model1)
# model1<-lm(price~ .-floors, data=train_set)
# summary(model1)
formula.1 <- "price ~ .-floors -sqft_lot -sqft_lot15"
model1<-lm(as.formula(formula.1), data=train_set)
summary(model1)
pred1<-predict(model1, newdata=val_set_X)
#we don't want to cut off the intercept, so we keep it!
pred1
pr<-postResample(pred1, val_set_y)
pr
#RMSE can also be calculated as:
sqrt(mean((pred1-val_set_y)**2))


#train model 2, polynomial model with grade 2
# model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) + bathrooms + I(bathrooms^2)+
#              sqft_living+I(sqft_living^2)+sqft_lot+I(sqft_lot^2)+floors+I(floors^2)+
#              waterfront+I(waterfront^2)+view+I(view^2)+condition+I(condition^2)+
#              grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
#              yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
#              lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15+
#              I(sqft_lot15^2), data=train_set)
# summary(model2)
# 
# model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) + bathrooms + I(bathrooms^2)+
#              sqft_living+I(sqft_living^2)+sqft_lot+I(sqft_lot^2)+floors+I(floors^2)+
#              waterfront+view+I(view^2)+condition+I(condition^2)+
#              grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
#              yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
#              lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15+
#              I(sqft_lot15^2), data=train_set)
# summary(model2)
# 
# model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) + bathrooms + I(bathrooms^2)+
#              sqft_living+I(sqft_living^2)+sqft_lot+I(sqft_lot^2)+floors+I(floors^2)+
#              waterfront+view+I(view^2)+condition+I(condition^2)+
#              grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
#              yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
#              lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15, data=train_set)
# summary(model2)
# 
# model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) + bathrooms + I(bathrooms^2)+
#              sqft_living+I(sqft_living^2)+sqft_lot+I(sqft_lot^2)+floors+I(floors^2)+
#              waterfront+view+condition+I(condition^2)+
#              grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
#              yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
#              lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15, data=train_set)
# summary(model2)
# 
# model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) + bathrooms + I(bathrooms^2)+
#              sqft_living+I(sqft_living^2)+I(sqft_lot^2)+floors+I(floors^2)+
#              waterfront+view+condition+I(condition^2)+
#              grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
#              yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
#              lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15, data=train_set)
# summary(model2)
# 
# model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) + bathrooms + I(bathrooms^2)+
#              sqft_living+I(sqft_living^2)+I(sqft_lot^2)+floors+I(floors^2)+
#              waterfront+view+condition+grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
#              yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
#              lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15, data=train_set)
# summary(model2)
# 
# model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) +I(bathrooms^2)+
#              sqft_living+I(sqft_living^2)+I(sqft_lot^2)+floors+I(floors^2)+
#              waterfront+view+condition+grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
#              yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
#              lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15, data=train_set)
# summary(model2)
# 
# model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) +I(bathrooms^2)+
#              sqft_living+I(sqft_living^2)+I(sqft_lot^2)+floors+I(floors^2)+
#              waterfront+view+condition+grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
#              yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
#              lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2), data=train_set)
# summary(model2)

formula.2 <- "price ~ date + I(date^2) + bedrooms + I(bedrooms^2) + I(bathrooms^2) +
             sqft_living + I(sqft_living^2) + I(sqft_lot^2) + floors + I(floors^2) +
             waterfront + view + condition + grade + I(grade^2) + sqft_above + I(sqft_above^2) + yr_built + I(yr_built^2) +
             yr_last_renovation + I(yr_last_renovation^2) + zipcode + I(zipcode^2) +
             lat + I(lat^2) + long + I(long^2) + sqft_living15"
model2<-lm(as.formula(formula.2), data=train_set)
summary(model2)

#with this graph we have the evidence that we can use a linear model to fit the data


hist(model2$residuals, breaks = 100)
pred2<-predict(model2, val_set_X)
postResample(pred2, val_set_y)
#the residual standard error of the model2 can be calculated also as:
anova(model2)
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
formula.3 <- "price ~ I(date^3) + bedrooms + I(bedrooms^2) + I(bathrooms^2) + sqft_living + I(sqft_living^2) + I(sqft_living^3) + sqft_lot + waterfront + view + I(view^2) + I(view^3) + I(condition^2) +
             grade + I(grade^2) + I(grade^3) + sqft_above + I(sqft_above^2) + I(sqft_above^3) + yr_built + I(yr_built^2) + I(yr_built^3) +
             yr_last_renovation + I(yr_last_renovation^2) + zipcode + I(zipcode^2) + lat + I(lat^2) + long + I(long^2) + I(sqft_living15^2) + I(sqft_living15^3) + sqft_lot15 +
             I(sqft_lot15^2) + I(sqft_lot15^3)"
model3<-lm(as.formula(formula.3),data=train_set)
summary(model3)

pred3<-predict(model3, newdata=val_set_X)
postResample(pred3, val_set_y)
# plot(val_set_y,pred3,  xlim=c(0,3000),ylim=c(0,3000))

dim(cor(kc_house)[,19])
ggpairs(kc_house, columns=c("date","bedrooms","bathrooms","sqft_living","sqft_above","price"))


#work in progress... using the function "poly" to simply sintax
# model4<-lm(price~ poly(date,4)+poly(bedrooms,4) + poly(bathrooms,4) + poly(sqft_living,4) +
#              poly(sqft_lot,4) + poly(floors,4) +
#              waterfront +poly(view,4) +poly(condition,4) +poly(grade,4) +poly(sqft_above,4) +poly(yr_built,4) +
#              poly(yr_last_renovation,4) +poly(zipcode,4) +poly(lat,4) +poly(long,4) +poly(sqft_living15 ,4) +poly(sqft_lot15,4),data=train_set)
# summary(model4)
formula.4 <- "price~ poly(date,3)+poly(bedrooms,2) + bathrooms + I(bathrooms^3) + I(bathrooms^4) + poly(sqft_living,4) +
             poly(sqft_lot,1) + floors + waterfront +poly(view,4) + poly(condition,1) + poly(grade,4) + poly(sqft_above,3) + I(yr_built^2)+I(yr_built^3) +
             poly(yr_last_renovation,3) + poly(zipcode,3) + poly(lat,4) + poly(long,4) + poly(sqft_living15 ,4) + I(sqft_lot15^4)"
model4<-lm(as.formula(formula.4),data=train_set)
summary(model4)

pred4<-predict(model4, newdata=val_set_X)
postResample(pred4, val_set_y)

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


hist(log10(sort(kc_house$price)))
ggplot(kc_house, aes(x = price)) +
  geom_histogram(color = "white") +
  labs(x = "price (USD)", title = "House price")

plot(kc_house$price~kc_house$sqft_living)

ggplot(kc_house, aes(x = sqft_living)) +
  geom_histogram(color = "white") +
  labs(x = "living space (square feet)", title = "House size")


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

