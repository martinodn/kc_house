kc_house<-read.csv("kc_house_data.csv")
attach(kc_house)


#TODO: dove abbiamo preso i dati, spiegare ogni colonna cosa significa

#check the dimension of the dataset
dim(kc_house)

#convert date from string to observation number of the day (starting from 1)
new.date<-as.Date(date,'%Y%m%d')
new.date<-new.date-(sort(new.date))[1]
#new.date
#sort(unique(new.date))

#We add this column to the dataset instead of the original one
kc_house[2]<-as.numeric(new.date)
kc_house
dim(kc_house)

#check the number of duplicated ids
length(id)- length(unique(id))

kc_house[c(80:100),]
#since id is not unique, we retrieve rows with duplicated id
#duplicated(id)
d<-kc_house[duplicated(id),]
dim(d)
#we note that there are some duplicates wrt id and date
#we suppose, by observations, that a specific id corresponds to a specific date
d<-kc_house[duplicated(id, date),]
dim(d)

#we want to check whether they are the same rows
all(duplicated(id)==duplicated(id,date))

#our observation is confirmed by this check
#hence, id not useful to understand data and to predict
kc_house<-kc_house[-1]

kc_house
dim(kc_house)

#we now take into account the column "yr_renovated": this column express the year of the last renovation of the
#house. Since the null values are coded as 0 and the other are coded as a year, this two are not very consistent and
#so we want to transform this column into a feature that express the year of the last renovation: if no renovation
#is done on a house, we use the year of construction.

#we firtsly check that there are no errors in the data, i.e. year of renovation smaller than year of construction.
any(yr_built>yr_renovated && yr_renovated!=0)

#since there are no errors, we now want to substitute the "yr_renovated" feature with the year of the last renovation
#or, if not present, the year of construction.
mask<-yr_renovated==0
mask
yr_last_renovation<-yr_renovated
yr_last_renovation[mask]<-yr_built[mask]
yr_last_renovation

kc_house["yr_renovated"]<-yr_last_renovation

colnames(kc_house)[15]<-"yr_last_renovation"

#move the target feature (price) to the last column
kc_house<-kc_house[,c("date", "bedrooms", "bathrooms", "sqft_living","sqft_lot", "floors", "waterfront","view","condition",
           "grade", "sqft_above", "sqft_basement", "yr_built", "yr_last_renovation", "zipcode",  "lat", "long","sqft_living15", 
           "sqft_lot15", "price")]


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
kc_house[bedrooms==33,]
boxplot(bedrooms)
# We note that the total living space is 1620 square feet, equal to about 150 m^2. This would imply that each room has a mean of 1.66 m^2,
#which is clearly impossible. So, there must be some errors in the reporting of the data (the number of 
#bedrooms could be 3 for example).

#So, we decide to delete that row.

kc_house<-kc_house[bedrooms<33,]
dim(kc_house)

#In general, we want to check the mean square footage of the rooms per each house, to see if there are 
#other unlikely values as the previous one.
detach(kc_house)
attach(kc_house)
mean_sqm<-(sqft_living/(bedrooms+(bedrooms*bathrooms)))/10.764
length(mean_sqm)
#we note that there are some infinite values due to the fact that there are no bedrooms and bathrooms in 
#that building. So, we want to boxplot the mean_sqft excluding those houses.

boxplot(mean_sqm[mean_sqm<Inf])
min(mean_sqm)

#We see that now the minimum value of the mean square meters of the rooms in a house is 3 times the 
#previous, so we have no evidence that this is an unlikely value.

#We want to see the type of houses we're dealing with; thus, we check the distribution of the grade of the
#houses and we compare it to a normal distribution.
hist(grade, breaks =12)
qqnorm(grade)
#We can see that the distribution of grade is close to a normal one, with the mean between 7 and 8 (7.65).
#Other feature, like sqft_living, are not distributed as a normal and we can see it by plotting its values.
qqnorm(sqft_living)

# MAP

# import required libraries
library(ggplot2)
library(ggmap)

# Set location bounds (King County)
location <- c(-140, 35, -90, 57)
# Fetch the map (osm = OpenStreetMap)
kc <- get_map(location=location, source="osm")
# Draw the map
map.kc <- ggmap(kc)
# Add the points layer
map.kc <- map.kc + geom_point(data = kc_house, aes(x = long, y = lat), size = .0001)
# Plot map
map.kc

#more zoommed graph
# Set location bounds (King County)
location <- c(-123.25, 47.15, -121.25, 47.9)
# Fetch the map (osm = OpenStreetMap)
kc <- get_map(location=location, source="osm")
# Draw the map
map.kc <- ggmap(kc)
# Add the points layer
map.kc <- map.kc + geom_point(data = kc_house, aes(x = long, y = lat), size = .0001)
# Plot map
map.kc

##########################REGRESSION MODEL
model<-lm(price ~ ., data=kc_house)
summary(model)

#From the summary of the model we have just applied we can see that the coefficient of the column 
#"sqft_basement" are labeled as NA: this is because there is a collinearity between that feature and 
#the two column "sqft_living" and "sqft_above". In fact, "sqft_basement" = "sqft_living" - "sqft_above".
#We give the proof of that:

sqft_diff<-sqft_living-(sqft_basement + sqft_above)
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
kc_house[19]<-(kc_house[19]/1000)
dim(kc_house)
detach(kc_house)
attach(kc_house)
min(kc_house$price)

price
#Import caret library
library(caret)
#Define the random seed (otherwise we cannot repeat exactly the same experiment)
set.seed(42)
#try to evaluate performance of different models splitting the whole set into training-validation-test set

id.train<-createDataPartition(kc_house$price, p=.75, list=FALSE)
id.val<-createDataPartition(kc_house$price, p=.25, list=FALSE)
# id.test<-createDataPartition(kc_house$price, p=.20, list=FALSE)

train_set<-kc_house[id.train,]
val_set_X<-kc_house[id.val,-19]
val_set_y<-kc_house[id.val,19]
# test_set_X<-kc_house[id.test,-19]
# test_set_y<-kc_house[id.test,19]
#train_set<-as.data.frame(train_set)

#we train model1 (linear model with grade 1)
model1<-lm(price~ ., data=train_set)
summary(model1)
model1<-lm(price~ .-floors, data=train_set)
summary(model1)
model1<-lm(price~ .-floors-sqft_lot, data=train_set)
summary(model1)
pred1<-predict(model1, newdata=val_set_X)

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE(pred1, val_set_y)

# plot(model1)

min(price)
#train model 2, polynomial model with grade 2
model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) + bathrooms + I(bathrooms^2)+
             sqft_living+I(sqft_living^2)+sqft_lot+I(sqft_lot^2)+floors+I(floors^2)+
             waterfront+I(waterfront^2)+view+I(view^2)+condition+I(condition^2)+
             grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
             yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
             lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15+
             I(sqft_lot15^2), data=train_set)
summary(model2)

model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) + bathrooms + I(bathrooms^2)+
             sqft_living+I(sqft_living^2)+sqft_lot+I(sqft_lot^2)+floors+I(floors^2)+
             waterfront+view+I(view^2)+condition+I(condition^2)+
             grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
             yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
             lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15+
             I(sqft_lot15^2), data=train_set)
summary(model2)

model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) + bathrooms + I(bathrooms^2)+
             sqft_living+I(sqft_living^2)+sqft_lot+I(sqft_lot^2)+floors+I(floors^2)+
             waterfront+view+I(view^2)+condition+I(condition^2)+
             grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
             yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
             lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15, data=train_set)
summary(model2)

model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) + bathrooms + I(bathrooms^2)+
             sqft_living+I(sqft_living^2)+sqft_lot+I(sqft_lot^2)+floors+I(floors^2)+
             waterfront+view+condition+I(condition^2)+
             grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
             yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
             lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15, data=train_set)
summary(model2)

model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) + bathrooms + I(bathrooms^2)+
             sqft_living+I(sqft_living^2)+I(sqft_lot^2)+floors+I(floors^2)+
             waterfront+view+condition+I(condition^2)+
             grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
             yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
             lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15, data=train_set)
summary(model2)

model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) + bathrooms + I(bathrooms^2)+
             sqft_living+I(sqft_living^2)+I(sqft_lot^2)+floors+I(floors^2)+
             waterfront+view+condition+grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
             yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
             lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15, data=train_set)
summary(model2)

model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) +I(bathrooms^2)+
             sqft_living+I(sqft_living^2)+I(sqft_lot^2)+floors+I(floors^2)+
             waterfront+view+condition+grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
             yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
             lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2)+sqft_lot15, data=train_set)
summary(model2)

model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) +I(bathrooms^2)+
             sqft_living+I(sqft_living^2)+I(sqft_lot^2)+floors+I(floors^2)+
             waterfront+view+condition+grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
             yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
             lat+I(lat^2)+long+I(long^2)+sqft_living15+I(sqft_living15^2), data=train_set)
summary(model2)

model2<-lm(price~ date+I(date^2)+ bedrooms+I(bedrooms^2) +I(bathrooms^2)+
             sqft_living+I(sqft_living^2)+I(sqft_lot^2)+floors+I(floors^2)+
             waterfront+view+condition+grade+I(grade^2)+sqft_above+I(sqft_above^2)+yr_built+I(yr_built^2)+
             yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+
             lat+I(lat^2)+long+I(long^2)+sqft_living15, data=train_set)
summary(model2)

pred2<-predict.lm(model2, val_set_X)

RMSE(pred2, val_set_y)


(pred2-val_set_y)[1:10]

lm.score(model2)

#polynomial model grade 3
model3<-lm(price~ date+I(date^2)+ I(date^3)+bedrooms+I(bedrooms^2) +I(bedrooms^3) + bathrooms + I(bathrooms^2)+
             I(bathrooms^3)+sqft_living+I(sqft_living^2)+I(sqft_living^3)+sqft_lot+I(sqft_lot^2)+I(sqft_lot^3)+
             floors+I(floors^2)+I(floors^3)+waterfront+I(waterfront^2)+I(waterfront^3)+view+I(view^2)+I(view^3)+condition+I(condition^2)+
             I(condition^3)+grade+I(grade^2)+I(grade^3)+sqft_above+I(sqft_above^2)+I(sqft_above^3)+yr_built+I(yr_built^2)+I(yr_built^3)+
             yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+lat+I(lat^2)+I(lat^3)+long+I(long^2)+I(long^3)+sqft_living15+I(sqft_living15^2)+I(sqft_living15^3)+sqft_lot15+
             I(sqft_lot15^2)+I(sqft_lot15^3),data=train_set)
summary(model3)
#remove 1 by 1 the covariates (we report only the final model)
model3<-lm(price~ I(date^3)+bedrooms+I(bedrooms^2) +bathrooms + I(bathrooms^2)+
             I(bathrooms^3)+sqft_living+I(sqft_living^2)+I(sqft_living^3)+sqft_lot+
             floors+I(floors^2)+waterfront+view+I(view^2)+I(view^3)+I(condition^2)+
             grade+I(grade^2)+I(grade^3)+sqft_above+I(sqft_above^2)+I(sqft_above^3)+yr_built+I(yr_built^2)+I(yr_built^3)+
             yr_last_renovation+I(yr_last_renovation^2)+zipcode+I(zipcode^2)+lat+I(lat^2)+long+I(long^2)+I(sqft_living15^2)+I(sqft_living15^3)+sqft_lot15+
             I(sqft_lot15^2)+I(sqft_lot15^3),data=train_set)
summary(model3)

pred3<-predict(model3, newdata=val_set_X)
pred3
RMSE(pred3, val_set_y)

sqrt(mean(residuals(model3)**2))
plot(residuals(model3))
# anova(model1)


postResample(pred3, val_set_y)
plot(val_set_y,pred3,  xlim=c(0,3000),ylim=c(0,3000))



###CROSS VALIDATION
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

#Define k for k-fold cross-validation
k<-10
#Split train data in K-fold split
folds<-createFolds(train$price, k=k, list=FALSE, returnTrain=FALSE)
#Loops through every fold
for (i in 1:k) {
  #Get validation set for i-th iteration
  idx.valid<-which(folds==i, arr.ind=TRUE)
  #Get validation set
  train[idx.valid,]
  #Get training set, without validation set
  train[-idx.valid,]
}


