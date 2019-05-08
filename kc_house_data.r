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
kc_house[2]<-new.date
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

##########################REGRESSION MODEL


