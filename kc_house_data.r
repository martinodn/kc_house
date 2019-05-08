kc_house<-read.csv("kc_house_data.csv")
attach(kc_house)

summary(kc_house)

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




