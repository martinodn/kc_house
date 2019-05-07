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
kc_house[1]<-new.date
kc_house
dim(kc_house)

#check the number of duplicated ids
length(id) - length(unique(id))
#since id is not unique, we retrieve rows with duplicated id
d<-kc_house[duplicated(id),]
dim(d)
#we note that there are some duplicates wrt id and date
#we suppose, by observations, that a specific id corresponds to a specific date
d<-kc_house[duplicated(id, date),]
dim(d)
#our observation is confirmed by the check on the count of the duplicates
#hence, id not useful to understand data and to predict
kc_house<-kc_house[-1]
