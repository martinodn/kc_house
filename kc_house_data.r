kc_house<-read.csv("kc_house_data.csv")
attach(kc_house)

summary(kc_house)
#id not useful to understand data and to predict
kc_house<-kc_house[-1]

#check the dimension of the dataset
dim(kc_house)

#convert date from string to observation number of the day (starting from 1)
new.date<-as.Date(date,'%Y%m%d')
new.date<-new.date-(sort(new.date))[1]
#new.date
#sort(unique(new.date))

#We add this column to the dataset instead of the original one


