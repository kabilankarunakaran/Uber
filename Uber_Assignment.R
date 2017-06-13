
uber <- read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)
View(uber)
str(uber)
summary(uber)
library(ggplot2)
library(lattice)
#Assumptions
#1. Considering all time in IST time zone

#Data Cleaning
#checking for NA's in request time
which(is.na(uber$Request.timestamp))

#checking for NA's in Drop time
# we get 3914 NA's 
length(which(is.na(uber$Drop.timestamp)))

# Conerting The Request/Drop timestamp to Standrad Time format
uber_date <- function(data, formats){
  uber_list<-list()
  for(i in 1:length(formats)){
    uber_list[[i]]<- as.POSIXlt(data,format=formats[i])
    uber_list[[1]][!is.na(uber_list[[i]])] <- uber_list[[i]][!is.na(uber_list[[i]])]
  }
  uber_list[[1]]
}

#Passing Value to function
uber$Drop.timestamp  <- multidate(uber$Drop.timestamp,c("%d/%m/%Y %H:%M","%d-%m-%Y %H:%M:%S"))
uber$Request.timestamp  <- multidate(uber$Request.timestamp,c("%d/%m/%Y %H:%M","%d-%m-%Y %H:%M:%S"))

#Seperating the Hour  
uber$Request_hour <- format(uber$Request.timestamp,format = "%H:%M")

#Creating a new column for time_slot
# Assumption 2 
# 04:01 to 8:59 considered to be as "Early Morning"
# 09:00 to 12:00 considered to be as "Late Morning"
# 12:01 to 14:59 considered to be as "Early Afternoon"
# 15:00 to 16:59 considered to be as "Late Morning"
# 17:00 to 18:59 considered to be as "Early Evening"
# 19:00 to 20:59 considered to be as "Late Evening"
# 21:00 to 04:00 considered to abe as "Night"

uber$time_slots <- ifelse(uber$Request_hour >= "04:01" & uber$Request_hour <= "08:59","Early Morning",
                          ifelse(uber$Request_hour >= "09:00" & uber$Request_hour <= "12:00","Late Morning",
                                 ifelse(uber$Request_hour >= "12:01" & uber$Request_hour <= "14:59","Early Afternoon",
                                        ifelse(uber$Request_hour >= "15" & uber$Request_hour <= "16:59","Late Afternoon",
                                               ifelse(uber$Request_hour >= "17:00" & uber$Request_hour <= "18:59","Early Evening",
                                                      ifelse(uber$Request_hour >= "19:00" & uber$Request_hour <= "20:59","Late Evening","Night")))))) 

# Jus to know time difference between drop and pickup 
# Average time taken to Drop a customer 53 mins
uber["Diff"] <-  uber$Drop.timestamp - uber$Request.timestamp
histogram(as.numeric(uber$Diff))

# To count the number of request in different time slots
ggplot(uber,aes(uber$time_slots)) + geom_histogram(stat = "count",position = "Dodge")

# To count the number of request for diiferent pickup points
ggplot(uber,aes(uber$Pickup.point)) + geom_histogram(stat = "count")

# To check which kind of trip could lead to more problems
ggplot(uber,aes(uber$time_slots))+geom_bar(aes(fill = uber$Pickup.point)) + coord_flip()

# To show the request classified under different status and time slots
ggplot(uber,aes(x=uber$time_slots, fill = uber$Status))+geom_bar(position = "stack") + coord_flip()

#To show the the request from different location
ggplot(uber,aes(x=uber$Status, fill = uber$Pickup.point)) + geom_bar(aes(fill = uber$time_slots),position = "dodge")
