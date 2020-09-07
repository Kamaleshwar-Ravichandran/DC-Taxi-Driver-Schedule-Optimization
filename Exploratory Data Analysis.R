library(curl)
library(plyr)
require(xlsx)
library(purrr)

#Exploratory Data Analysis
dcData<-read.csv("DC_Demand_New.csv")

dcData<-na.omit(dcData)
dcData_temp<-data.frame(dcData$ORIGIN_BLOCK_LATITUDE,dcData$ORIGIN_BLOCK_LONGITUDE)
pickupCluster<-kmeans(dcData_temp,centers = 5,nstart = 25)

#choosing number of clusters
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(dcData_temp, k, nstart = 25 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

dcData$cluster<-pickupCluster$cluster
write.csv(dcData,"dctemp.csv")

dcData$timeSlot<-ifelse(dcData$Hour%%2==0,dcData$Hour+1,dcData$Hour)
dcData$timeSlot<-dcData$timeSlot-1
dcData$timeSlot<-dcData$timeSlot/2
grouped_data <- aggregate(dcData$cluster, by=list(dcData$cluster, dcData$Weekday, dcData$timeSlot), FUN=length)
grouped_data2<- aggregate(dcData$ï..DURATION, by=list(dcData$cluster, dcData$Weekday, dcData$timeSlot), FUN=sum)
grouped_data3<- aggregate(dcData$TOTALAMOUNT, by=list(dcData$cluster, dcData$Weekday, dcData$timeSlot), FUN=sum)

Clustered_Data<-grouped_data
Clustered_Data$Mean_Duration<-round(grouped_data2$x/grouped_data$x,2)
Clustered_Data$Mean_Amount<-round(grouped_data3$x/grouped_data$x,2)

Clustered_Data$Group.1<-Clustered_Data$Group.1-1

write.csv(Clustered_Data,"Clustered_Data.csv")


#Confidence Interval
mc<-read.xlsx("D:/UC/MSBA Class Materials/MS CAPSTONE BANA8083/output.xlsx",sheetName="Sheet1")
mc$Total.Revenue<-mc$Total.Revenue
s<-sd(mc$Total.Revenue)
m<-mean(mc$Total.Revenue)
n=200

CI<-qnorm(0.975)*s/sqrt(n)
CI_Lower<-m-CI
CI_Upper<-m+CI

