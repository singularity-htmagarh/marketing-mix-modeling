library(tidyverse)
library(sqldf)
library(reshape2)
library(lubridate)
library(openxlsx)
library(xlsx)
library(XLConnect)
library(zoo)
library(readxl)
library(dummies)
library(ggplot2)
library(forecast)
library(caret)
library(rpart)

setwd("C:/Users/hthapa/OneDrive - Epsilon/Projects/Wingstop MMM/Modeling")

storeDay<-read.csv("store by day dataset UPDATED.csv", stringsAsFactors = FALSE)

## Adding Dataset 
storeDay <- `store week dataset LATEST`

## Market Status is already classified

#storeDay$marketStatus<-NULL
#storeDay$marketStatus[storeDay$DMA_NAME %in% c("DALLAS-FT. WORTH","HOUSTON","LOS ANGELES","SAN ANTONIO")]<-"Core"
#storeDay$marketStatus[storeDay$DMA_NAME %in% c("PHOENIX (PRESCOTT)","SAN DIEGO","SAN FRANCISCO-OAK-SAN JOSE","MIAMI-FT. LAUDERDALE",
#"CHICAGO","SACRAMNTO-STKTON-MODESTO", "DENVER", "LAS VEGAS", "AUSTIN")]<-"Developing"
#storeDay$marketStatus[is.na(storeDay$marketStatus)]<-"Emerging"

storeDay$date <- as.Date(storeDay$date)
storeDay$week <- trunc(((storeDay$date) - min(storeDay$date))/dweeks(1))

storeDay$DeliveryTransactions[is.na(storeDay$DeliveryTransactions)]<-0

# setwd("~/Documents/Wingstop/Modeling/sales")
# transactions<-read.csv("all transaction data.csv", stringsAsFactors = FALSE)
# transactions$date<-as.Date(transactions$DateOfBusiness)
# transactions<-transactions[ , names(transactions) %in% c('StoreNo','TransactionCount','date')]
# transactions2<-read.csv("fixed sales jan feb with transactions.csv", stringsAsFactors = FALSE)
# transactions2$date<-as.Date(transactions2$DateOfBusiness)
# transactions2<-transactions2[ , names(transactions2) %in% c('StoreNo','TransactionCount','date')]
# 
# allTrans<-rbind(transactions, transactions2)
# allTrans<-allTrans[!is.na(allTrans$TransactionCount), ]
# unduplicatedTrans<-allTrans[!duplicated(allTrans[c("StoreNo","date")]), ]
# 
# storeDay<-storeDay[ , !names(storeDay) %in% c("TransactionCount", "TransactionCount.x", "TransactionCount.y")]
# unduplicatedStoreDay<-storeDay[!duplicated(storeDay[c("StoreNo","date")]), ]
# storeDay<-left_join(x = unduplicatedStoreDay, y = unduplicatedTrans, by = c("StoreNo" = "StoreNo", "date" = "date"))
# storeDay<-storeDay[!is.na(storeDay$TransactionCount) , ]
####media variables

setwd("~/Documents/Wingstop/Modeling/media")
tv<-read.csv("tv spend.csv", stringsAsFactors = FALSE)
names(tv)<-c("x", "yearDates", "tvspend")
tv$month<- str_split_fixed(tv$yearDates, "/", n = 3)[,1]
tv$day  <- str_split_fixed(tv$yearDates, "/", n = 3)[,2]
tv$year <- as.numeric(str_split_fixed(tv$yearDates, "/", n = 3)[,3]) + 2000
tv$date <- paste(as.character(tv$year), tv$month, tv$day, sep="-") %>% ymd() %>% as.Date()

dmaCounts<-storeDay %>%
  group_by(date, DMA_NAME) %>%
  summarise(DMAstorecount = n())

nationalCounts<-storeDay %>%
  group_by(date) %>%
  summarise(Nationalstorecount = n())

tvByStore<-left_join(x = tv[ , c("date", "tvspend")], y = nationalCounts, by = c("date" = "date"))
tvByStore$tvspend<-tvByStore$tvspend/tvByStore$Nationalstorecount

storeSales <- left_join(x = storeDay, y = dmaCounts, by = c("date" = "date", "DMA_NAME"= "DMA_NAME"))
storeSales <- left_join(x = storeSales, y = nationalCounts, by = c("date" = "date"))
media<-storeSales[ , c("StoreNo", "date", "DMA_NAME", "DMAstorecount", "Nationalstorecount")]

mediafiles<- c("OLO Display 2017 National",
               "OLO Search 2017 National",
               "OLO Social National 2017",
               "Engagement Facebook Natl 2017",
               "Engagement Natl Twitter 2017",
               "National OLV 2017",
               "OLV Social 2017 National",
               "OLO Display National 2018",
               "OLO Search National 2018",
               "OLO Facebook National 2018",
               "OLO Twitter National 2018",
               "OLV National 2018",
               "OLV Facebook National 2018",
               "OLV Twitter National 2018",
               "OLO Social National 2019",
               "OLO Display National 2019",
               "OLO Search National 2019",
               "OLV National 2019",
               "OLV Social National 2019") 

for (file in mediafiles){
  hold<-read.csv(paste0(file, ".csv"), stringsAsFactors = FALSE)
  if (grepl("2017", file)){
    names(hold)<-c("x", "yearDates", file)
    hold<-hold[ , -c(1)]
    hold$yearDates<-as.Date(hold$yearDates)
    media<-left_join(x = media, y = hold, by = c("date" = "yearDates"))
  } else{
    hold<-hold[ , c("DMA", "date", "storeSpend")]
    names(hold)<-c("DMA", "date", file)
    hold$date<-as.Date(hold$date)
    media<-left_join(x = media, y = hold, by = c("date" = "date", "DMA_NAME" = "DMA"))
  }
  
}

media[is.na(media)]<-0
media$OLOSocialSpend<-rowSums(media[ , c("OLO Social National 2017", "OLO Facebook National 2018", "OLO Twitter National 2018","OLO Social National 2019")])
media$OLOSearchSpend<-rowSums(media[ , c("OLO Search 2017 National", "OLO Search National 2018", "OLO Search National 2019")])
media$OLODisplaySpend<-rowSums(media[ , c("OLO Display 2017 National", "OLO Display National 2018", "OLO Display National 2019")])
media$OLVNationalSpend<-rowSums(media[ , c("National OLV 2017", "OLV National 2018", "OLV National 2019")])
media$OLVSocialSpend<-rowSums(media[ , c("OLV Social 2017 National", "Engagement Facebook Natl 2017", "Engagement Natl Twitter 2017", "OLV Facebook National 2018", "OLV Twitter National 2018", "OLV Social National 2019")])
media$DigitalSpend<-rowSums(media[ , c("OLOSocialSpend", "OLOSearchSpend", "OLODisplaySpend", "OLVNationalSpend", "OLVSocialSpend")])

media<-media[ , c("StoreNo", "date", "DMA_NAME", "OLOSocialSpend", "OLOSearchSpend" , "OLODisplaySpend", "OLVSocialSpend", "OLVNationalSpend", "DigitalSpend", "Nationalstorecount", "DMAstorecount")]

mediaVars<-c("OLOSocialSpend", "OLOSearchSpend" , "OLODisplaySpend", "OLVSocialSpend", "OLVNationalSpend", "DigitalSpend")

for (med in mediaVars){
  text1<-paste0("media$",med, "[year(media$date) == 2017]<-media$", med, "[year(media$date) == 2017] / media$Nationalstorecount[year(media$date) == 2017]")
  text2<-paste0("media$",med, "[year(media$date) > 2017]<-media$", med, "[year(media$date) > 2017] / media$DMAstorecount[year(media$date) > 2017]")
  eval(parse(text = text1))
  eval(parse(text = text2))
}

media<-left_join(x = media, y = tv[ , c("date", "tvspend")], by = c("date" = "date"))
media$tvspend<-media$tvspend/media$Nationalstorecount
media$AdSpend<-rowSums(media[ , c("DigitalSpend", "tvspend")])

media<-media[ , c("StoreNo", "date", "OLOSocialSpend", "OLOSearchSpend" , "OLODisplaySpend", "OLVSocialSpend", "OLVNationalSpend", "DigitalSpend", "tvspend")]

storeDay<-storeDay[ , !names(storeDay) %in% c("OLOSocialSpend", "OLOSearchSpend" , "OLODisplaySpend", "OLVSocialSpend", "OLVNationalSpend", "DigitalSpend", "tvspend")]

storeDay<-left_join(x = storeDay, y = media, by = c("StoreNo" = "StoreNo", "date" = "date"))

names(storeDay)[names(storeDay) %in% "TransactionCount.y"]<-"TransactionCount"
storeDay$adSpend<-storeDay$tvspend + storeDay$DigitalSpend

nationalWeek<-storeDay%>%
  group_by(week)%>%
  summarise(TransactionCount = sum(TransactionCount, na.rm = TRUE), storeWeeks = n_distinct(StoreNo), adSpend = sum(adSpend), totalSales = sum(TotalItemSales, na.rm = TRUE))

######## AVG COUNTS ##############

nationalWeek$avgSales<-nationalWeek$totalSales/nationalWeek$storeWeeks
nationalWeek$avgTransactionCount<-nationalWeek$TransactionCount/nationalWeek$storeWeeks
nationalWeek$avgTransactionSize<-nationalWeek$totalSales/nationalWeek$TransactionCount
nationalDay<-storeDay%>%
  group_by(date)%>%
  summarise(TransactionCount = sum(TransactionCount, na.rm = TRUE), storeWeeks = n_distinct(StoreNo), adSpend = sum(adSpend), totalSales = sum(TotalItemSales, na.rm = TRUE))


##decomposition transactions
nationalWeekTS <- ts(nationalWeek$totalSales, frequency = 52)
stl_national = stl(nationalWeekTS, "periodic")
plot(stl_national)
plotData<-data.frame(stl_national$time.series)
plotData$date<-NULL
plotData$date[1]<-as.Date("2017-01-01")
for (d in 2:nrow(plotData)){
  plotData$date[d]<-as.Date(plotData$date[d - 1]) + 7
}
trendAndRemainder<-stl_national$time.series[ , 2] + stl_national$time.series[ , 3] 
trendAndRemainderData<-data.frame(cbind(week = c(0:(length(trendAndRemainder)-1)), trendAndRemainder))
trendOnly<-stl_national$time.series[ , 2] 
trendOnlyData<-data.frame(cbind(week = c(0:(length(trendOnly)-1)), trendOnly))
plotData$date<-as.Date(plotData$date)
ggplot(plotData, aes(x = date, y = trend)) + geom_line() + 
   geom_line(plotData, mapping = aes(x = date, y = seasonal), color = "blue") + geom_line(plotData, mapping =  aes(x = date, y = remainder),color = "red" ) + ylim(c(-3000000, 30000000)) + ylab("Total Sales")
write.csv(stl_national$time.series, "national total sales trend data.csv")
nationalWeekTS <- ts(nationalWeek$avgSales, frequency = 52)
stl_national = stl(nationalWeekTS, "periodic")
plot(stl_national)
avgSales<-stl_national$time.series[ , 2]
plotData<-data.frame(stl_national$time.series)
plotData$date<-NULL
plotData$date[1]<-as.Date("2017-01-01")
for (d in 2:nrow(plotData)){
  plotData$date[d]<-as.Date(plotData$date[d - 1]) + 7
}

plotData$date<-as.Date(plotData$date)
ggplot(plotData, aes(x = date, y = trend)) + geom_line() + 
  geom_line(plotData, mapping = aes(x = date, y = seasonal), color = "blue") + geom_line(plotData, mapping =  aes(x = date, y = remainder),color = "red" ) + ylim(c(-3000, 30000)) + ylab("Average Store Sales")
write.csv(stl_national$time.series, "national average store sales trend data.csv")

nationalWeekTS <- ts(nationalWeek$TransactionCount, frequency = 52)
stl_national = stl(nationalWeekTS, "periodic")
plot(stl_national)
plotData<-data.frame(stl_national$time.series)
plotData$date<-NULL
plotData$date[1]<-as.Date("2017-01-01")
for (d in 2:nrow(plotData)){
  plotData$date[d]<-as.Date(plotData$date[d - 1]) + 7
}

plotData$date<-as.Date(plotData$date)
ggplot(plotData, aes(x = date, y = trend)) + geom_line() + 
  geom_line(plotData, mapping = aes(x = date, y = seasonal), color = "blue") + geom_line(plotData, mapping =  aes(x = date, y = remainder),color = "red" ) + ylim(c(-200000, 2000000)) + ylab("Total Transactions")

write.csv(stl_national$time.series, "national total transaction trend data.csv")
nationalWeekTS <- ts(nationalWeek$avgTransactionCount, frequency = 52)
stl_national = stl(nationalWeekTS, "periodic")
plot(stl_national)
avgTransactions<-stl_national$time.series[ , 2]
plotData<-data.frame(stl_national$time.series)
plotData$date<-NULL
plotData$date[1]<-as.Date("2017-01-01")
for (d in 2:nrow(plotData)){
  plotData$date[d]<-as.Date(plotData$date[d - 1]) + 7
}

plotData$date<-as.Date(plotData$date)
ggplot(plotData, aes(x = date, y = trend)) + geom_line() + 
  geom_line(plotData, mapping = aes(x = date, y = seasonal), color = "blue") + geom_line(plotData, mapping =  aes(x = date, y = remainder),color = "red" ) + ylim(c(-200, 1500)) + ylab("Average Store Transactions")

write.csv(stl_national$time.series, "national average transaction trend data.csv")

nationalWeekTS <- ts(nationalWeek$adSpend, frequency = 52)
stl_national = stl(nationalWeekTS, "periodic")
plot(stl_national)
adSpend<-stl_national$time.series[ , 2]
write.csv(stl_national$time.series, "national ad spend trend data.csv")

##ad spend vs transaction trend
plotData<-data.frame(cbind(adSpend, avgSales))
p<-ggplot(plotData, aes(x = adSpend, y = avgSales)) + geom_point()
p

plotData<-data.frame(cbind(adSpend, avgTransactions))
p<-ggplot(plotData, aes(x = adSpend, y = avgTransactions)) + geom_point()
p

#delivery basket over time -- any delivery

transactions<-c("internalDeliveryTransactions","externalDeliveryTransactions", "webMobileTransactions", 
  "appTransactions",  "CallToGoTransactions", "CallDineInTransactions", "WalkToGoTransactions", "WalkDineInTransactions")
for (var in transactions){
  eval(parse(text = paste0("storeDay$",var,"[is.na(storeDay$",var,")]<-0")))
}

storeDay$totalDeliveryTransactions<-storeDay$internalDeliveryTransactions + storeDay$externalDeliveryTransactions
storeDay<-left_join(x = storeDay, y = deliveryDates, by = c("StoreNo"))
deliveryList<-unique(storeDay$StoreNo[storeDay$AnyDeliveryAvailable == 1])
deliverystores<-storeDay[storeDay$StoreNo %in% deliveryList, ]
deliverystores$fromDeliveryTime<-as.Date(deliverystores$date) - as.Date(deliverystores$FirstDeliveryDate)
deliverystores$WeeksFromDelivery<-as.numeric(floor(deliverystores$fromDeliveryTime/7))
deliverystores50<-deliverystores$StoreNo[deliverystores$WeeksFromDelivery == 40 ]
deliverystoresneg50<-deliverystores$StoreNo[deliverystores$WeeksFromDelivery == -40 ]
deliverystores<-deliverystores[deliverystores$StoreNo %in% deliverystores50 & deliverystores$StoreNo %in% deliverystoresneg50, ]

DeliverySales<-sqldf('
                     SELECT
                       WeeksFromDelivery
                     , DeliveryWave
                     
                     , sum(TotalItemSales)
                     , sum(TotalComps)
                     , sum(TotalPromos)
                     , sum(TotalNetSales)
                     , sum(totalDelivery)
                     , sum(webMobileSales)
                     , sum(appSales)
                     , sum(webMobileSales)/sum(TotalItemSales) as webMobilepct
                     , sum(appSales)/sum(TotalItemSales) as apppct
                     , sum(WalkToGoSales)
                     , sum(CallToGoSales)
                     , sum(WalkDineInSales)
                     , sum(CallDineInSales)
                     , sum(DeliverySales)
                     , sum(DDDeliverySales)
                     , sum(DoorDashDeliverySales)
                     , (sum(DeliverySales) + sum(DDDeliverySales) + sum(DoorDashDeliverySales))/sum(TotalItemSales) as totalDeliverypct
                     , (sum(DeliverySales))/sum(TotalItemSales) as Deliverypct
                     , (sum(DDDeliverySales))/sum(TotalItemSales) as DDDeliverypct
                     , (sum(DoorDashDeliverySales))/sum(TotalItemSales) as DoorDashDeliverypct
                     , sum(CallDineInSales)/sum(TotalItemSales) as CallDineInpct
                     , sum(WalkDineInSales)/sum(TotalItemSales) as WalkDineInpct
                     , sum(CallToGoSales)/sum(TotalItemSales) as CallToGopct
                     , sum(WalkToGoSales)/sum(TotalItemSales) as WalkToGopct
                     
                      , (sum(internalDeliveryTransactions) + sum(externalDeliveryTransactions))/sum(TransactionCount) as totalDeliveryTransactionspct
 ,cast((sum(DeliveryTransactions))/sum(TransactionCount) as float)  as DeliveryTransactionspct
 ,cast((sum(DDDeliveryTransactions))/sum(TransactionCount) as float)  as DDDeliveryTransactionspct
, cast(cast(sum(DoorDashDeliveryTransactions) as float)/sum(TransactionCount) as float)  as DoorDashDeliveryTransactionspct
, cast(sum(internalDeliveryTransactions)/sum(TransactionCount) as float)  as internalDeliveryTransactionspct
, cast(sum(externalDeliveryTransactions)/sum(TransactionCount) as float)  as externalDeliveryTransactionspct
                     , cast(sum(webMobileTransactions)/sum(TransactionCount) as float) as webMobileTransactionspct
                     , cast(sum(appTransactions)/sum(TransactionCount) as float)  as appTransactionspct
                     , cast(sum(CallDineInTransactions)/sum(TransactionCount) as float)  as CallDineInTransactionspct
                     , cast(sum(WalkDineInTransactions)/sum(TransactionCount) as float)  as WalkDineInTransactionspct
                     , cast(sum(CallToGoTransactions)/sum(TransactionCount) as float)  as CallToGoTransactionspct
                     , cast(sum(WalkToGoTransactions)/sum(TransactionCount) as float)  as WalkToGoTransactionspct
                     
                     ,avg(OverallSatisfaction)
                     ,avg(DeliveryOSAT)
                     ,avg(DigitalOSAT)
                     ,avg(TasteOfFood)
                     ,avg(AccuracyOfOrder)
                     ,avg(SpeedOfService)
                     ,avg(unemploymentRate)
                     , count(*) as numberOfStores
                     
                     
                     FROM
                     deliverystores as s
                     
                     GROUP BY
                     WeeksFromDelivery,
DeliveryWave
                     ;')

write.csv(DeliverySales, "delivery sales and transactions -- any delivery.csv")
DeliverySales$other<-1 - DeliverySales$webMobilepct - DeliverySales$apppct - DeliverySales$DoorDashDeliverypct - DeliverySales$DDDeliverypct -DeliverySales$Deliverypct -
  DeliverySales$WalkDineInpct -
  DeliverySales$WalkToGopct - DeliverySales$CallDineInpct - DeliverySales$CallToGopct

DeliverySales$otherTransactions<-1 - DeliverySales$webMobileTransactionspct - DeliverySales$appTransactionspct - DeliverySales$DeliveryTransactionspct - DeliverySales$DDDeliveryTransactionspct - DeliverySales$DoorDashDeliveryTransactionspct - DeliverySales$WalkDineInTransactionspct -
  DeliverySales$WalkToGoTransactionspct - DeliverySales$CallDineInTransactionspct - DeliverySales$CallToGoTransactionspct

mdata <- melt(DeliverySales[ , c("WeeksFromDelivery", "Deliverypct","DDDeliverypct","DoorDashDeliverypct","other", "webMobilepct", 
                                 "apppct",  "CallToGopct", "CallDineInpct", "WalkToGopct", "WalkDineInpct")], id=c("WeeksFromDelivery"))

ggplot(mdata, aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Sales at introduction of any Delivery")

mdata <- melt(DeliverySales[ , c("WeeksFromDelivery", "DeliveryTransactionspct","DDDeliveryTransactionspct","DoorDashDeliveryTransactionspct", "otherTransactions", "webMobileTransactionspct", 
                                 "appTransactionspct",  "CallToGoTransactionspct", "CallDineInTransactionspct", "WalkToGoTransactionspct", "WalkDineInTransactionspct")], id=c("WeeksFromDelivery"))

ggplot(mdata, aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Transactions at Introduction of Any Delivery")


#non-delivery composition
DeliverySales2<-DeliverySales
DeliverySales2$nonDeliveryPct<-1 - DeliverySales2$Deliverypct - DeliverySales2$DDDeliverypct - DeliverySales2$DoorDashDeliverypct
DeliverySales2$other<-DeliverySales2$other/DeliverySales2$nonDeliveryPct
DeliverySales2$webMobilepct<-DeliverySales2$webMobilepct/DeliverySales2$nonDeliveryPct
DeliverySales2$apppct<-DeliverySales2$apppct/DeliverySales2$nonDeliveryPct
DeliverySales2$CallToGopct<-DeliverySales2$CallToGopct/DeliverySales2$nonDeliveryPct
DeliverySales2$CallDineInpct<-DeliverySales2$CallDineInpct/DeliverySales2$nonDeliveryPct
DeliverySales2$WalkToGopct<-DeliverySales2$WalkToGopct/DeliverySales2$nonDeliveryPct
DeliverySales2$WalkDineInpct<-DeliverySales2$WalkDineInpct/DeliverySales2$nonDeliveryPct

DeliverySales2$nonDeliveryTransactionsPct<-1 - DeliverySales2$DeliveryTransactionspct - DeliverySales2$DDDeliveryTransactionspct - DeliverySales2$DoorDashDeliveryTransactionspct
DeliverySales2$otherTransactions<-DeliverySales2$otherTransactions/DeliverySales2$nonDeliveryTransactionsPct
DeliverySales2$webMobileTransactionspct<-DeliverySales2$webMobileTransactionspct/DeliverySales2$nonDeliveryTransactionsPct
DeliverySales2$appTransactionspct<-DeliverySales2$appTransactionspct/DeliverySales2$nonDeliveryTransactionsPct
DeliverySales2$CallToGoTransactionspct<-DeliverySales2$CallToGoTransactionspct/DeliverySales2$nonDeliveryTransactionsPct
DeliverySales2$CallDineInTransactionspct<-DeliverySales2$CallDineInTransactionspct/DeliverySales2$nonDeliveryTransactionsPct
DeliverySales2$WalkToGoTransactionspct<-DeliverySales2$WalkToGoTransactionspct/DeliverySales2$nonDeliveryTransactionsPct
DeliverySales2$WalkDineInTransactionspct<-DeliverySales2$WalkDineInTransactionspct/DeliverySales2$nonDeliveryTransactionsPct
mdata <- melt(DeliverySales2[ , c("WeeksFromDelivery","other", "webMobilepct", 
                                 "apppct",  "CallToGopct", "CallDineInpct", "WalkToGopct", "WalkDineInpct")], id=c("WeeksFromDelivery"))

ggplot(mdata, aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Sales at introduction of any Delivery")

mdata <- melt(DeliverySales2[ , c("WeeksFromDelivery","otherTransactions", "webMobileTransactionspct", 
                                 "appTransactionspct",  "CallToGoTransactionspct", "CallDineInTransactionspct", "WalkToGoTransactionspct", "WalkDineInTransactionspct")], id=c("WeeksFromDelivery"))

ggplot(mdata, aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Transactions at Introduction of Any Delivery")

#pre-post, and then without app
deliverystores$prepost<-ifelse(deliverystores$WeeksFromDelivery < 0, "before", "after")
DeliverySales<-sqldf('
                     SELECT
                     prepost
                     ,sum(TotalItemSales)
                     ,sum(TotalComps)
                     ,sum(TotalPromos)
                     ,sum(TotalNetSales)
                     ,sum(totalDelivery)
                     ,sum(webMobileSales)
                     ,sum(appSales)
                     , sum(webMobileSales)/sum(TotalItemSales) as webMobilepct
                     , sum(appSales)/sum(TotalItemSales) as apppct
                     ,sum(WalkToGoSales)
                     ,sum(CallToGoSales)
                     ,sum(WalkDineInSales)
                     ,sum(CallDineInSales)
                     ,sum(DeliverySales)
                     ,sum(DDDeliverySales)
                     ,sum(DoorDashDeliverySales)
                     , (sum(DeliverySales) + sum(DDDeliverySales) + sum(DoorDashDeliverySales))/sum(TotalItemSales) as totalDeliverypct
                     ,(sum(DeliverySales))/sum(TotalItemSales) as Deliverypct
                     ,(sum(DDDeliverySales))/sum(TotalItemSales) as DDDeliverypct
                     ,(sum(DoorDashDeliverySales))/sum(TotalItemSales) as DoorDashDeliverypct
                     , sum(CallDineInSales)/sum(TotalItemSales) as CallDineInpct
                     , sum(WalkDineInSales)/sum(TotalItemSales) as WalkDineInpct
                     , sum(CallToGoSales)/sum(TotalItemSales) as CallToGopct
                     , sum(WalkToGoSales)/sum(TotalItemSales) as WalkToGopct
                     
                     , (sum(internalDeliveryTransactions) + sum(externalDeliveryTransactions))/sum(TransactionCount) as totalDeliveryTransactionspct
                     ,cast((sum(DeliveryTransactions))/sum(TransactionCount) as float)  as DeliveryTransactionspct
                     ,cast((sum(DDDeliveryTransactions))/sum(TransactionCount) as float)  as DDDeliveryTransactionspct
                     
                     
                     , cast(cast(sum(DoorDashDeliveryTransactions) as float)/sum(TransactionCount) as float)  as DoorDashDeliveryTransactionspct
                     
                     , cast(sum(internalDeliveryTransactions)/sum(TransactionCount) as float)  as internalDeliveryTransactionspct
                     , cast(sum(externalDeliveryTransactions)/sum(TransactionCount) as float)  as externalDeliveryTransactionspct
                     , cast(sum(webMobileTransactions)/sum(TransactionCount) as float) as webMobileTransactionspct
                     , cast(sum(appTransactions)/sum(TransactionCount) as float)  as appTransactionspct
                     , cast(sum(CallDineInTransactions)/sum(TransactionCount) as float)  as CallDineInTransactionspct
                     , cast(sum(WalkDineInTransactions)/sum(TransactionCount) as float)  as WalkDineInTransactionspct
                     , cast(sum(CallToGoTransactions)/sum(TransactionCount) as float)  as CallToGoTransactionspct
                     , cast(sum(WalkToGoTransactions)/sum(TransactionCount) as float)  as WalkToGoTransactionspct


, sum(TransactionCount) as TransactionCount
                     ,cast((sum(DeliveryTransactions)) as float)  as DeliveryTransactions
                     ,cast((sum(DDDeliveryTransactions))/sum(TransactionCount) as float)  as DDDeliveryTransactions
                     
                     
                     , cast(cast(sum(DoorDashDeliveryTransactions) as float) as float)  as DoorDashDeliveryTransactions
      
                     , cast(sum(webMobileTransactions) as float) as webMobileTransactions
                     , cast(sum(appTransactions) as float)  as appTransactions
                     , cast(sum(CallDineInTransactions) as float)  as CallDineInTransactions
                     , cast(sum(WalkDineInTransactions) as float)  as WalkDineInTransactions
                     , cast(sum(CallToGoTransactions) as float)  as CallToGoTransactions
                     , cast(sum(WalkToGoTransactions) as float)  as WalkToGoTransactions
                     
                     ,avg(OverallSatisfaction)
                     ,avg(DeliveryOSAT)
                     ,avg(DigitalOSAT)
                     ,avg(TasteOfFood)
                     ,avg(AccuracyOfOrder)
                     ,avg(SpeedOfService)
                     ,avg(unemploymentRate)
                     , count(*) as numberOfStores
                     
                     
                     FROM
                     deliverystores as s
                     
                     GROUP BY
                    prepost
                     ;')

DeliverySales$other<-1 - DeliverySales$webMobilepct - DeliverySales$apppct - DeliverySales$DoorDashDeliverypct - DeliverySales$DDDeliverypct -DeliverySales$Deliverypct -
  DeliverySales$WalkDineInpct -
  DeliverySales$WalkToGopct - DeliverySales$CallDineInpct - DeliverySales$CallToGopct


DeliverySales$otherTransactions<-1 - DeliverySales$webMobileTransactionspct - DeliverySales$appTransactionspct - DeliverySales$DeliveryTransactionspct - DeliverySales$DDDeliveryTransactionspct - DeliverySales$DoorDashDeliveryTransactionspct - DeliverySales$WalkDineInTransactionspct -
  DeliverySales$WalkToGoTransactionspct - DeliverySales$CallDineInTransactionspct - DeliverySales$CallToGoTransactionspct
DeliverySales2<-DeliverySales
DeliverySales2$nonDeliveryPct<-1 - DeliverySales2$Deliverypct - DeliverySales2$DDDeliverypct - DeliverySales2$DoorDashDeliverypct
DeliverySales2$other<-DeliverySales2$other/DeliverySales2$nonDeliveryPct
DeliverySales2$webMobilepct<-DeliverySales2$webMobilepct/DeliverySales2$nonDeliveryPct
DeliverySales2$apppct<-DeliverySales2$apppct/DeliverySales2$nonDeliveryPct
DeliverySales2$CallToGopct<-DeliverySales2$CallToGopct/DeliverySales2$nonDeliveryPct
DeliverySales2$CallDineInpct<-DeliverySales2$CallDineInpct/DeliverySales2$nonDeliveryPct
DeliverySales2$WalkToGopct<-DeliverySales2$WalkToGopct/DeliverySales2$nonDeliveryPct
DeliverySales2$WalkDineInpct<-DeliverySales2$WalkDineInpct/DeliverySales2$nonDeliveryPct

DeliverySales2$nonDeliveryTransactionsPct<-1 - DeliverySales2$DeliveryTransactionspct - DeliverySales2$DDDeliveryTransactionspct - DeliverySales2$DoorDashDeliveryTransactionspct
DeliverySales2$otherTransactions<-DeliverySales2$otherTransactions/DeliverySales2$nonDeliveryTransactionsPct
DeliverySales2$webMobileTransactionspct<-DeliverySales2$webMobileTransactionspct/DeliverySales2$nonDeliveryTransactionsPct
DeliverySales2$appTransactionspct<-DeliverySales2$appTransactionspct/DeliverySales2$nonDeliveryTransactionsPct
DeliverySales2$CallToGoTransactionspct<-DeliverySales2$CallToGoTransactionspct/DeliverySales2$nonDeliveryTransactionsPct
DeliverySales2$CallDineInTransactionspct<-DeliverySales2$CallDineInTransactionspct/DeliverySales2$nonDeliveryTransactionsPct
DeliverySales2$WalkToGoTransactionspct<-DeliverySales2$WalkToGoTransactionspct/DeliverySales2$nonDeliveryTransactionsPct
DeliverySales2$WalkDineInTransactionspct<-DeliverySales2$WalkDineInTransactionspct/DeliverySales2$nonDeliveryTransactionsPct
write.csv(t(DeliverySales2), "prepost v2.csv")


#delivery basket over time -- wingstop's own channel

deliveryList<-unique(storeDay$StoreNo[storeDay$InternalDeliveryAvailable == 1])
deliverystores<-storeDay[storeDay$StoreNo %in% deliveryList, ]
deliverystores$fromDeliveryTime<-as.Date(deliverystores$date) - as.Date(deliverystores$FirstDeliveryDate)
deliverystores$WeeksFromDelivery<-as.numeric(floor(deliverystores$fromDeliveryTime/7))
deliverystores50<-deliverystores$StoreNo[deliverystores$WeeksFromDelivery == 40 ]
deliverystoresneg50<-deliverystores$StoreNo[deliverystores$WeeksFromDelivery == -40 ]
deliverystores<-deliverystores[deliverystores$StoreNo %in% deliverystores50 & deliverystores$StoreNo %in% deliverystoresneg50, ]

DeliverySales<-sqldf('
                     SELECT
                     WeeksFromDelivery
                     
                     ,sum(TotalItemSales)
                     ,sum(TotalComps)
                     ,sum(TotalPromos)
                     ,sum(TotalNetSales)
                     ,sum(totalDelivery)
                     ,sum(webMobileSales)
                     ,sum(appSales)
                     , sum(webMobileSales)/sum(TotalItemSales) as webMobilepct
                     , sum(appSales)/sum(TotalItemSales) as apppct
                     ,sum(WalkToGoSales)
                     ,sum(CallToGoSales)
                     ,sum(WalkDineInSales)
                     ,sum(CallDineInSales)
                     ,sum(DeliverySales)
                     ,sum(DDDeliverySales)
                     ,sum(DoorDashDeliverySales)
                     , (sum(DeliverySales) + sum(DDDeliverySales) + sum(DoorDashDeliverySales))/sum(TotalItemSales) as totalDeliverypct
                     ,(sum(DeliverySales))/sum(TotalItemSales) as Deliverypct
                     ,(sum(DDDeliverySales))/sum(TotalItemSales) as DDDeliverypct
                     ,(sum(DoorDashDeliverySales))/sum(TotalItemSales) as DoorDashDeliverypct
                     , sum(CallDineInSales)/sum(TotalItemSales) as CallDineInpct
                     , sum(WalkDineInSales)/sum(TotalItemSales) as WalkDineInpct
                     , sum(CallToGoSales)/sum(TotalItemSales) as CallToGopct
                     , sum(WalkToGoSales)/sum(TotalItemSales) as WalkToGopct
                     
                     , (sum(internalDeliveryTransactions) + sum(externalDeliveryTransactions))/sum(TransactionCount) as totalDeliveryTransactionspct
                     ,(sum(DeliveryTransactions))/sum(TransactionCount) as DeliveryTransactionspct
                     ,(sum(DDDeliveryTransactions))/sum(TransactionCount) as DDDeliveryTransactionspct
                     ,(sum(DoorDashDeliveryTransactions))/sum(TransactionCount) as DoorDashDeliveryTransactionspct
                     , sum(internalDeliveryTransactions)/sum(TransactionCount) as internalDeliveryTransactionspct
                     , sum(externalDeliveryTransactions)/sum(TransactionCount) as externalDeliveryTransactionspct
                     , sum(webMobileTransactions)/sum(TransactionCount) as webMobileTransactionspct
                     , sum(appTransactions)/sum(TransactionCount) as appTransactionspct
                     , sum(CallDineInTransactions)/sum(TransactionCount) as CallDineInTransactionspct
                     , sum(WalkDineInTransactions)/sum(TransactionCount) as WalkDineInTransactionspct
                     , sum(CallToGoTransactions)/sum(TransactionCount) as CallToGoTransactionspct
                     , sum(WalkToGoTransactions)/sum(TransactionCount) as WalkToGoTransactionspct
                     
                     ,avg(OverallSatisfaction)
                     ,avg(DeliveryOSAT)
                     ,avg(DigitalOSAT)
                     ,avg(TasteOfFood)
                     ,avg(AccuracyOfOrder)
                     ,avg(SpeedOfService)
                     ,avg(unemploymentRate)
                     , count(*) as numberOfStores
                     
                     
                     FROM
                     deliverystores as s
                     
                     GROUP BY
                     WeeksFromDelivery
                     ;')

#write.csv(DeliverySales, "delivery sales and transactions -- any delivery.csv")
DeliverySales$other<-1 - DeliverySales$webMobilepct - DeliverySales$apppct - DeliverySales$DoorDashDeliverypct - DeliverySales$DDDeliverypct -DeliverySales$Deliverypct - DeliverySales$WalkDineInpct -
  DeliverySales$WalkToGopct - DeliverySales$CallDineInpct - DeliverySales$CallToGopct

DeliverySales$otherTransactions<-1 - DeliverySales$webMobileTransactionspct - DeliverySales$appTransactionspct - DeliverySales$DeliveryTransactionspct - DeliverySales$DDDeliveryTransactionspct - DeliverySales$DoorDashDeliveryTransactionspct - DeliverySales$WalkDineInTransactionspct -
  DeliverySales$WalkToGoTransactionspct - DeliverySales$CallDineInTransactionspct - DeliverySales$CallToGoTransactionspct

mdata <- melt(DeliverySales[ , c("WeeksFromDelivery", "Deliverypct","DDDeliverypct","DoorDashDeliverypct","other", "webMobilepct", 
                                 "apppct",  "CallToGopct", "CallDineInpct", "WalkToGopct", "WalkDineInpct")], id=c("WeeksFromDelivery"))

ggplot(mdata[mdata$WeeksFromDelivery >= -40 & mdata$WeeksFromDelivery <= 40, ], aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Sales at Introduction of Local Wingstop's Own Delivery")

mdata <- melt(DeliverySales[ , c("WeeksFromDelivery", "internalDeliveryTransactionspct","DDDeliveryTransactionspct","DoorDashDeliveryTransactionspct", "otherTransactions", "webMobileTransactionspct", 
                                 "appTransactionspct",  "CallToGoTransactionspct", "CallDineInTransactionspct", "WalkToGoTransactionspct", "WalkDineInTransactionspct")], id=c("WeeksFromDelivery"))

ggplot(mdata[mdata$WeeksFromDelivery >= -40 & mdata$WeeksFromDelivery <= 40, ], aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Transactions at Introduction of Local Wingstop's Own Delivery")


#delivery basket over time -- Door Dash

deliveryList<-unique(storeDay$StoreNo[storeDay$DoorDashDeliveryAvailable == 1])
deliverystores<-storeDay[storeDay$StoreNo %in% deliveryList, ]
deliverystores$fromDeliveryTime<-as.Date(deliverystores$date) - as.Date(deliverystores$FirstDeliveryDate)
deliverystores$WeeksFromDelivery<-as.numeric(floor(deliverystores$fromDeliveryTime/7))
deliverystores50<-deliverystores$StoreNo[deliverystores$WeeksFromDelivery == 40 ]
deliverystoresneg50<-deliverystores$StoreNo[deliverystores$WeeksFromDelivery == -40 ]
deliverystores<-deliverystores[deliverystores$StoreNo %in% deliverystores50 & deliverystores$StoreNo %in% deliverystoresneg50, ]

DeliverySales<-sqldf('
                     SELECT
                     WeeksFromDelivery
                     
                     ,sum(TotalItemSales)
                     ,sum(TotalComps)
                     ,sum(TotalPromos)
                     ,sum(TotalNetSales)
                     ,sum(totalDelivery)
                     ,sum(webMobileSales)
                     ,sum(appSales)
                     , sum(webMobileSales)/sum(TotalItemSales) as webMobilepct
                     , sum(appSales)/sum(TotalItemSales) as apppct
                     ,sum(WalkToGoSales)
                     ,sum(CallToGoSales)
                     ,sum(WalkDineInSales)
                     ,sum(CallDineInSales)
                     ,sum(DeliverySales)
                     ,sum(DDDeliverySales)
                     ,sum(DoorDashDeliverySales)
                     , (sum(DeliverySales) + sum(DDDeliverySales) + sum(DoorDashDeliverySales))/sum(TotalItemSales) as totalDeliverypct
                     ,(sum(DeliverySales))/sum(TotalItemSales) as Deliverypct
                     ,(sum(DDDeliverySales))/sum(TotalItemSales) as DDDeliverypct
                     ,(sum(DoorDashDeliverySales))/sum(TotalItemSales) as DoorDashDeliverypct
                     , sum(CallDineInSales)/sum(TotalItemSales) as CallDineInpct
                     , sum(WalkDineInSales)/sum(TotalItemSales) as WalkDineInpct
                     , sum(CallToGoSales)/sum(TotalItemSales) as CallToGopct
                     , sum(WalkToGoSales)/sum(TotalItemSales) as WalkToGopct
                     
                     , (sum(internalDeliveryTransactions) + sum(externalDeliveryTransactions))/sum(TransactionCount) as totalDeliveryTransactionspct
                     ,(sum(DeliveryTransactions))/sum(TransactionCount) as DeliveryTransactionspct
                     ,(sum(DDDeliveryTransactions))/sum(TransactionCount) as DDDeliveryTransactionspct
                     ,(sum(DoorDashDeliveryTransactions))/sum(TransactionCount) as DoorDashDeliveryTransactionspct
                     , sum(internalDeliveryTransactions)/sum(TransactionCount) as internalDeliveryTransactionspct
                     , sum(externalDeliveryTransactions)/sum(TransactionCount) as externalDeliveryTransactionspct
                     , sum(webMobileTransactions)/sum(TransactionCount) as webMobileTransactionspct
                     , sum(appTransactions)/sum(TransactionCount) as appTransactionspct
                     , sum(CallDineInTransactions)/sum(TransactionCount) as CallDineInTransactionspct
                     , sum(WalkDineInTransactions)/sum(TransactionCount) as WalkDineInTransactionspct
                     , sum(CallToGoTransactions)/sum(TransactionCount) as CallToGoTransactionspct
                     , sum(WalkToGoTransactions)/sum(TransactionCount) as WalkToGoTransactionspct
                     
                     ,avg(OverallSatisfaction)
                     ,avg(DeliveryOSAT)
                     ,avg(DigitalOSAT)
                     ,avg(TasteOfFood)
                     ,avg(AccuracyOfOrder)
                     ,avg(SpeedOfService)
                     ,avg(unemploymentRate)
                     , count(*) as numberOfStores
                     
                     
                     FROM
                     deliverystores as s
                     
                     GROUP BY
                     WeeksFromDelivery
                     ;')

#write.csv(DeliverySales, "delivery sales and transactions -- any delivery.csv")
DeliverySales$other<-1 - DeliverySales$webMobilepct - DeliverySales$apppct - DeliverySales$DoorDashDeliverypct - DeliverySales$DDDeliverypct -DeliverySales$Deliverypct - DeliverySales$WalkDineInpct -
  DeliverySales$WalkToGopct - DeliverySales$CallDineInpct - DeliverySales$CallToGopct

DeliverySales$otherTransactions<-1 - DeliverySales$webMobileTransactionspct - DeliverySales$appTransactionspct - DeliverySales$DeliveryTransactionspct - DeliverySales$DDDeliveryTransactionspct - DeliverySales$DoorDashDeliveryTransactionspct - DeliverySales$WalkDineInTransactionspct -
  DeliverySales$WalkToGoTransactionspct - DeliverySales$CallDineInTransactionspct - DeliverySales$CallToGoTransactionspct

mdata <- melt(DeliverySales[ , c("WeeksFromDelivery", "Deliverypct","DDDeliverypct","DoorDashDeliverypct","other", "webMobilepct", 
                                 "apppct",  "CallToGopct", "CallDineInpct", "WalkToGopct", "WalkDineInpct")], id=c("WeeksFromDelivery"))

ggplot(mdata[mdata$WeeksFromDelivery >= -40 & mdata$WeeksFromDelivery <= 40, ], aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Sales at Introduction of Door Dash")

mdata <- melt(DeliverySales[ , c("WeeksFromDelivery", "internalDeliveryTransactionspct","DDDeliveryTransactionspct","DoorDashDeliveryTransactionspct", "otherTransactions", "webMobileTransactionspct", 
                                 "appTransactionspct",  "CallToGoTransactionspct", "CallDineInTransactionspct", "WalkToGoTransactionspct", "WalkDineInTransactionspct")], id=c("WeeksFromDelivery"))

ggplot(mdata[mdata$WeeksFromDelivery >= -40 & mdata$WeeksFromDelivery <= 40, ], aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Transactions at Introduction of Door Dash")



#delivery basket over time -- Dispatch

deliveryList<-unique(storeDay$StoreNo[storeDay$DDDeliveryAvailable == 1])
deliverystores<-storeDay[storeDay$StoreNo %in% deliveryList, ]
deliverystores$fromDeliveryTime<-as.Date(deliverystores$date) - as.Date(deliverystores$FirstDeliveryDate)
deliverystores$WeeksFromDelivery<-as.numeric(floor(deliverystores$fromDeliveryTime/7))
deliverystores50<-deliverystores$StoreNo[deliverystores$WeeksFromDelivery == 40 ]
deliverystoresneg50<-deliverystores$StoreNo[deliverystores$WeeksFromDelivery == -40 ]
deliverystores<-deliverystores[deliverystores$StoreNo %in% deliverystores50 & deliverystores$StoreNo %in% deliverystoresneg50, ]

DeliverySales<-sqldf('
                     SELECT
                     WeeksFromDelivery
                     
                     ,sum(TotalItemSales)
                     ,sum(TotalComps)
                     ,sum(TotalPromos)
                     ,sum(TotalNetSales)
                     ,sum(totalDelivery)
                     ,sum(webMobileSales)
                     ,sum(appSales)
                     , sum(webMobileSales)/sum(TotalItemSales) as webMobilepct
                     , sum(appSales)/sum(TotalItemSales) as apppct
                     ,sum(WalkToGoSales)
                     ,sum(CallToGoSales)
                     ,sum(WalkDineInSales)
                     ,sum(CallDineInSales)
                     ,sum(DeliverySales)
                     ,sum(DDDeliverySales)
                     ,sum(DoorDashDeliverySales)
                     , (sum(DeliverySales) + sum(DDDeliverySales) + sum(DoorDashDeliverySales))/sum(TotalItemSales) as totalDeliverypct
                     ,(sum(DeliverySales))/sum(TotalItemSales) as Deliverypct
                     ,(sum(DDDeliverySales))/sum(TotalItemSales) as DDDeliverypct
                     ,(sum(DoorDashDeliverySales))/sum(TotalItemSales) as DoorDashDeliverypct
                     , sum(CallDineInSales)/sum(TotalItemSales) as CallDineInpct
                     , sum(WalkDineInSales)/sum(TotalItemSales) as WalkDineInpct
                     , sum(CallToGoSales)/sum(TotalItemSales) as CallToGopct
                     , sum(WalkToGoSales)/sum(TotalItemSales) as WalkToGopct
                     
                     , (sum(internalDeliveryTransactions) + sum(externalDeliveryTransactions))/sum(TransactionCount) as totalDeliveryTransactionspct
                     ,(sum(DeliveryTransactions))/sum(TransactionCount) as DeliveryTransactionspct
                     ,(sum(DDDeliveryTransactions))/sum(TransactionCount) as DDDeliveryTransactionspct
                     ,(sum(DoorDashDeliveryTransactions))/sum(TransactionCount) as DoorDashDeliveryTransactionspct
                     , sum(internalDeliveryTransactions)/sum(TransactionCount) as internalDeliveryTransactionspct
                     , sum(externalDeliveryTransactions)/sum(TransactionCount) as externalDeliveryTransactionspct
                     , sum(webMobileTransactions)/sum(TransactionCount) as webMobileTransactionspct
                     , sum(appTransactions)/sum(TransactionCount) as appTransactionspct
                     , sum(CallDineInTransactions)/sum(TransactionCount) as CallDineInTransactionspct
                     , sum(WalkDineInTransactions)/sum(TransactionCount) as WalkDineInTransactionspct
                     , sum(CallToGoTransactions)/sum(TransactionCount) as CallToGoTransactionspct
                     , sum(WalkToGoTransactions)/sum(TransactionCount) as WalkToGoTransactionspct
                     
                     ,avg(OverallSatisfaction)
                     ,avg(DeliveryOSAT)
                     ,avg(DigitalOSAT)
                     ,avg(TasteOfFood)
                     ,avg(AccuracyOfOrder)
                     ,avg(SpeedOfService)
                     ,avg(unemploymentRate)
                     , count(*) as numberOfStores
                     
                     
                     FROM
                     deliverystores as s
                     
                     GROUP BY
                     WeeksFromDelivery
                     ;')

#write.csv(DeliverySales, "delivery sales and transactions -- any delivery.csv")
DeliverySales$other<-1 - DeliverySales$webMobilepct - DeliverySales$apppct - DeliverySales$DoorDashDeliverypct - DeliverySales$DDDeliverypct -DeliverySales$Deliverypct - DeliverySales$WalkDineInpct -
  DeliverySales$WalkToGopct - DeliverySales$CallDineInpct - DeliverySales$CallToGopct

DeliverySales$otherTransactions<-1 - DeliverySales$webMobileTransactionspct - DeliverySales$appTransactionspct - DeliverySales$DeliveryTransactionspct - DeliverySales$DDDeliveryTransactionspct - DeliverySales$DoorDashDeliveryTransactionspct- DeliverySales$WalkDineInTransactionspct -
  DeliverySales$WalkToGoTransactionspct - DeliverySales$CallDineInTransactionspct - DeliverySales$CallToGoTransactionspct

mdata <- melt(DeliverySales[ , c("WeeksFromDelivery", "Deliverypct","DDDeliverypct","DoorDashDeliverypct","other", "webMobilepct", 
                                 "apppct",  "CallToGopct", "CallDineInpct", "WalkToGopct", "WalkDineInpct")], id=c("WeeksFromDelivery"))

ggplot(mdata[mdata$WeeksFromDelivery >= -40 & mdata$WeeksFromDelivery <= 40, ], aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Sales at Introduction of Dispatch")

mdata <- melt(DeliverySales[ , c("WeeksFromDelivery", "internalDeliveryTransactionspct","DDDeliveryTransactionspct","DoorDashDeliveryTransactionspct", "otherTransactions", "webMobileTransactionspct", 
                                 "appTransactionspct",  "CallToGoTransactionspct", "CallDineInTransactionspct", "WalkToGoTransactionspct", "WalkDineInTransactionspct")], id=c("WeeksFromDelivery"))

ggplot(mdata[mdata$WeeksFromDelivery >= -40 & mdata$WeeksFromDelivery <= 40, ], aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Transactions at Introduction of Dispatch")



###add additional variables

setwd("~/Documents/Wingstop/Modeling/FROM RMS")
pricing<-read.csv("all price data FILLED.csv", stringsAsFactors = FALSE)

meanPricingVars<-names(pricing)[grepl("mean", names(pricing))]
pricing<-pricing[ , !names(pricing) %in% meanPricingVars]

pricing$date<-as.Date(pricing$date)
storeDay<-left_join(x = storeDay, y = pricing, by = c("date" = "date", "StoreNo" = "StoreNo"))

#DMA density and cumulative ad spend, christmas, new years, season, temp squared/hot/cold
setwd("~/Documents/Wingstop/Modeling")
population<-read_excel("dma hh.xlsx")
names(population)<-c("rank", "DMA_NAME", "households", "percentOfUS")
population<-population[ , c("DMA_NAME", "households")]

storeDay<-left_join(x = storeDay, y = population, by = c("DMA_NAME" = "DMA_NAME"))

###FIX DMASTORECOUNT

dmaCounts<- storeDay%>%
  group_by(DMA_NAME, date) %>%
  summarise(DMAStoreCount = n_distinct(StoreNo))

storeDay<- storeDay[ , !names(storeDay) %in% c("DMAstorecount", "DMAStoreCount.x", "DMAStoreCount.y", "DMAStoreCount")]

storeDay<-left_join(x = storeDay, y = dmaCounts, by = c("DMA_NAME" = "DMA_NAME", "date" = "date"))
storeDay$householdsPerStore<-storeDay$households/storeDay$DMAStoreCount
storeDay<-storeDay[order(storeDay$StoreNo, storeDay$date),]

storeDay$MidTempSq <-storeDay$MidTemp * storeDay$MidTemp
storeDay$hotDay<-ifelse(storeDay$MaxTemp > 100, 1, 0)
storeDay$coldDay<-ifelse(storeDay$MaxTemp < 30, 1, 0)
storeDay$spring<-ifelse(storeDay$month %in% c(3:5), 1, 0)
storeDay$summer<-ifelse(storeDay$month %in% c(6:8), 1, 0)
storeDay$fall<-ifelse(storeDay$month %in% c(9:11), 1, 0)
storeDay$winter<-ifelse(storeDay$month %in% c(12, 1, 2), 1, 0)

storeDay$christmas_eve<-ifelse(storeDay$date %in% c(as.Date("2017-12-24"), as.Date("2018-12-24"), as.Date("2019-12-24")), 1, 0)
storeDay$christmas<-ifelse(storeDay$date %in% c(as.Date("2017-12-25"), as.Date("2018-12-25"), as.Date("2019-12-25")), 1, 0)
storeDay$christmas_after<-ifelse(storeDay$date %in% c(as.Date("2017-12-26"), as.Date("2018-12-26"), as.Date("2019-12-26")), 1, 0)
storeDay$easter<-ifelse(storeDay$date %in% c(as.Date("2017-04-16"), as.Date("2018-04-01"), as.Date("2019-04-21")), 1, 0)

storeDay$thanksgiving<-ifelse(storeDay$date %in% c(as.Date("2017-11-23"), as.Date("2018-11-22"), as.Date("2019-11-28")), 1, 0)

names(storeDay)[names(storeDay) %in% c('price.10.Boneless.Combo.r')]<-'price10BonelessCombor'
names(storeDay)[names(storeDay) %in% c('price.10.Classic.Combo.j')]<-'price10ClassicComboj'
names(storeDay)[names(storeDay) %in% c('price.10.pc.Boneless.r')]<-'price10pcBonelessr'
names(storeDay)[names(storeDay) %in% c('price.10.pc.Classic.j')]<-'price10pcClassicj'
names(storeDay)[names(storeDay) %in% c('price.10.Split.Combo.j')]<-'price10SplitComboj'
names(storeDay)[names(storeDay) %in% c('price.12.pc.Classic.j')]<-'price12pcClassicj'
names(storeDay)[names(storeDay) %in% c('price.15.Boneless.Combo.r')]<-'price15BonelessCombor'
names(storeDay)[names(storeDay) %in% c('price.15.Classic.Combo.j')]<-'price15ClassicComboj'
names(storeDay)[names(storeDay) %in% c('price.15.pc.Boneless.r')]<-'price15pcBonelessr'
names(storeDay)[names(storeDay) %in% c('price.15.pc.Classic.j')]<-'price15pcClassicj'
names(storeDay)[names(storeDay) %in% c('price.15.Split.Combo.j')]<-'price15SplitComboj'
names(storeDay)[names(storeDay) %in% c('price.2.pc.Crispy.Tenders')]<-'price2pcCrispyTenders'
names(storeDay)[names(storeDay) %in% c('price.20.Oz.Beverage')]<-'price20OzBeverage'
names(storeDay)[names(storeDay) %in% c('price.20.pc.Boneless.r')]<-'price20pcBonelessr'
names(storeDay)[names(storeDay) %in% c('price.20.pc.Classic.j')]<-'price20pcClassicj'
names(storeDay)[names(storeDay) %in% c('price.24.pc.Family.Boneless.r')]<-'price24pcFamilyBonelessr'
names(storeDay)[names(storeDay) %in% c('price.24.pc.Family.Classic.j')]<-'price24pcFamilyClassicj'
names(storeDay)[names(storeDay) %in% c('price.24.pc.Family.Split.j')]<-'price24pcFamilySplitj'
names(storeDay)[names(storeDay) %in% c('price.25.BW.Big.Nite.in.Bundle')]<-'price25BWBigNiteinBundle'
names(storeDay)[names(storeDay) %in% c('price.3.Crispy.Tenders.Combo')]<-'price3CrispyTendersCombo'
names(storeDay)[names(storeDay) %in% c('price.3.pc.Boneless.r')]<-'price3pcBonelessr'
names(storeDay)[names(storeDay) %in% c('price.3.pc.Classic.j')]<-'price3pcClassicj'
names(storeDay)[names(storeDay) %in% c('price.30.pc.Classic.j')]<-'price30pcClassicj'
names(storeDay)[names(storeDay) %in% c('price.30.pc.Family.Boneless.r')]<-'price30pcFamilyBonelessr'
names(storeDay)[names(storeDay) %in% c('price.30.pc.Family.Classic.j')]<-'price30pcFamilyClassicj'
names(storeDay)[names(storeDay) %in% c('price.30.pc.Family.Split.j')]<-'price30pcFamilySplitj'
names(storeDay)[names(storeDay) %in% c('price.32.Oz.Beverage')]<-'price32OzBeverage'
names(storeDay)[names(storeDay) %in% c('price.4.pc.Crispy.Tenders')]<-'price4pcCrispyTenders'
names(storeDay)[names(storeDay) %in% c('price.40.pc.Family.Classic.j')]<-'price40pcFamilyClassicj'
names(storeDay)[names(storeDay) %in% c('price.40.pc.Family.Split.j')]<-'price40pcFamilySplitj'
names(storeDay)[names(storeDay) %in% c('price.5.Crispy.Tenders.Combo')]<-'price5CrispyTendersCombo'
names(storeDay)[names(storeDay) %in% c('price.5.pc.Boneless.r')]<-'price5pcBonelessr'
names(storeDay)[names(storeDay) %in% c('price.5.pc.Classic.j')]<-'price5pcClassicj'
names(storeDay)[names(storeDay) %in% c('price.50.pc.Family.Classic.j')]<-'price50pcFamilyClassicj'
names(storeDay)[names(storeDay) %in% c('price.6.Boneless.Combo.r')]<-'price6BonelessCombor'
names(storeDay)[names(storeDay) %in% c('price.6.Classic.Combo.j')]<-'price6ClassicComboj'
names(storeDay)[names(storeDay) %in% c('price.6.Split.Combo.j')]<-'price6SplitComboj'
names(storeDay)[names(storeDay) %in% c('price.8.Boneless.Combo.r')]<-'price8BonelessCombor'
names(storeDay)[names(storeDay) %in% c('price.8.Classic.Combo.j')]<-'price8ClassicComboj'
names(storeDay)[names(storeDay) %in% c('price.8.pc.Boneless.r')]<-'price8pcBonelessr'
names(storeDay)[names(storeDay) %in% c('price.8.pc.Classic.j')]<-'price8pcClassicj'
names(storeDay)[names(storeDay) %in% c('price.8.Split.Combo.j')]<-'price8SplitComboj'
names(storeDay)[names(storeDay) %in% c('price.Add.5.Boneless.Wings.r')]<-'priceAdd5BonelessWingsr'
names(storeDay)[names(storeDay) %in% c('price.Add.5.Classic.Wings.J')]<-'priceAdd5ClassicWingsJ'
names(storeDay)[names(storeDay) %in% c('price.Bleu.Cheese...Regular')]<-'priceBleuCheeseRegular'
names(storeDay)[names(storeDay) %in% c('price.Buffalo.FF')]<-'priceBuffaloFF'
names(storeDay)[names(storeDay) %in% c('price.BW.Lunch.Combo.r')]<-'priceBWLunchCombor'
names(storeDay)[names(storeDay) %in% c('price.Carrot.Sticks')]<-'priceCarrotSticks'
names(storeDay)[names(storeDay) %in% c('price.Celery.Sticks')]<-'priceCelerySticks'
names(storeDay)[names(storeDay) %in% c('price.Cheese.Fries...Regular')]<-'priceCheeseFriesRegular'
names(storeDay)[names(storeDay) %in% c('price.Cheese.Sauce...Medium')]<-'priceCheeseSauceMedium'
names(storeDay)[names(storeDay) %in% c('price.Cheese.Sauce...Regular')]<-'priceCheeseSauceRegular'
names(storeDay)[names(storeDay) %in% c('price.CL.Lunch.Combo.j')]<-'priceCLLunchComboj'
names(storeDay)[names(storeDay) %in% c('price.French.Fries...Large')]<-'priceFrenchFriesLarge'
names(storeDay)[names(storeDay) %in% c('price.French.Fries...Regular')]<-'priceFrenchFriesRegular'
names(storeDay)[names(storeDay) %in% c('price.French.Fries...Regular.e')]<-'priceFrenchFriesRegulare'
names(storeDay)[names(storeDay) %in% c('price.Fried.Corn.Large')]<-'priceFriedCornLarge'
names(storeDay)[names(storeDay) %in% c('price.Fried.Corn.Regular')]<-'priceFriedCornRegular'
names(storeDay)[names(storeDay) %in% c('price.Honey.Mustard...Regular')]<-'priceHoneyMustardRegular'
names(storeDay)[names(storeDay) %in% c('price.Ranch...Large')]<-'priceRanchLarge'
names(storeDay)[names(storeDay) %in% c('price.Ranch...Regular')]<-'priceRanchRegular'
names(storeDay)[names(storeDay) %in% c('price.Roll...Each')]<-'priceRollEach'
names(storeDay)[names(storeDay) %in% c('price.Rolls...Half.Dozen')]<-'priceRollsHalfDozen'
names(storeDay)[names(storeDay) %in% c('price.Sauce')]<-'priceSauce'
names(storeDay)[names(storeDay) %in% c('price.Triple.Chocolate.Brownie')]<-'priceTripleChocolateBrownie'
names(storeDay)[names(storeDay) %in% c('price.Upgrade.Drink')]<-'priceUpgradeDrink'
names(storeDay)[names(storeDay) %in% c('price.Veggie.Sticks')]<-'priceVeggieSticks'
names(storeDay)[names(storeDay) %in% c('price.Voodoo.Fries.Large')]<-'priceVoodooFriesLarge'
names(storeDay)[names(storeDay) %in% c('price.Voodoo.Fries.Regular')]<-'priceVoodooFriesRegular'

setwd("~/Documents/Wingstop/Modeling/Weather")
stores<-read.csv("StoreExtract_2019-05-29.csv", stringsAsFactors = FALSE)
stores$openingDate<-as.Date(stores$OpenDate)
stores<-stores[ , c("StoreNo", "openingDate")]
storeDay<-left_join(x = storeDay, y = stores, by = c("StoreNo" = "StoreNo"))
storeDay$ageOfStoreInWeeks<-floor((as.Date(storeDay$date) - storeDay$openingDate)/7)
storeDay$newStore30weeks<-ifelse(storeDay$ageOfStoreInWeeks <=30, 1, 0)

storeDay$braziliancitruspepper<-ifelse(storeDay$date >= as.Date("2017-03-06") & storeDay$date <= as.Date("2017-05-16"), 1, 0)
storeDay$spicykoreanq<-ifelse(storeDay$date >= as.Date("2018-06-04") & storeDay$date <= as.Date("2018-07-23"), 1, 0)

storeDay$promo1<-ifelse(storeDay$date >= as.Date("2016-11-06") & storeDay$date <= as.Date("2017-01-31"), 1, 0)
storeDay$promo2<-ifelse(storeDay$date >= as.Date("2017-05-08") & storeDay$date <= as.Date("2017-07-31"), 1, 0)
storeDay$promo3<-ifelse(storeDay$date >= as.Date("2017-11-06") & storeDay$date <= as.Date("2018-01-28"), 1, 0)
storeDay$promo4<-ifelse(storeDay$date >= as.Date("2018-04-02") & storeDay$date <= as.Date("2018-06-03"), 1, 0)
storeDay$promo5<-ifelse(storeDay$date >= as.Date("2018-06-04") & storeDay$date <= as.Date("2018-07-28"), 1, 0)

storeDay$web1<-ifelse(storeDay$date >= as.Date("2019-01-08") , 1, 0)
storeDay$web2<-ifelse(storeDay$date >= as.Date("2019-01-17") , 1, 0)
storeDay$web3<-ifelse(storeDay$date >= as.Date("2019-02-13") , 1, 0)
storeDay$web4<-ifelse(storeDay$date >= as.Date("2019-03-27") , 1, 0)
storeDay$web5<-ifelse(storeDay$date >= as.Date("2019-04-16") , 1, 0)


storeDay$app1<-ifelse(storeDay$date >= as.Date("2019-01-10") , 1, 0)
storeDay$app2<-ifelse(storeDay$date >= as.Date("2019-01-23") , 1, 0)
storeDay$app3<-ifelse(storeDay$date >= as.Date("2019-02-26") , 1, 0)
storeDay$app4<-ifelse(storeDay$date >= as.Date("2019-04-02") , 1, 0)
storeDay$app5<-ifelse(storeDay$date >= as.Date("2019-04-22") , 1, 0)

averagesales<-sqldf('
                    SELECT
                    StoreNo
                    ,avg(TotalItemSales) as AvgItemSales
                    FROM
                    storeDay as s
                    
                    GROUP BY
                    StoreNo
                    ;')
averagesales<-averagesales %>%
  mutate(quintile = ntile(AvgItemSales, 5))

averagesales<-averagesales[ , c("StoreNo", "quintile")]
storeDay<-left_join(x = storeDay, y = averagesales, by = c("StoreNo" = "StoreNo"))

quintiles<-dummy(storeDay$quintile)
storeDay<-cbind(storeDay, quintiles)
storeDay$ageOfStoreInWeeks<-as.numeric(storeDay$ageOfStoreInWeeks)

storeDay$samplingEvent<-0

cities<-c(
  'ATLANTA',
  'CHICAGO',
  'DENVER',
  'DALLAS-FT. WORTH',
  'DALLAS-FT. WORTH',
  'CHICAGO',
  'CHICAGO',
  'TOPEKA',
  'LOS ANGELES',
  'PHOENIX (PRESCOTT)',
  'CHICAGO',
  'NEW YORK',
  'NEW YORK',
  'CHICAGO',
  'DALLAS-FT. WORTH',
  'DALLAS-FT. WORTH',
  'DALLAS-FT. WORTH',
  'DALLAS-FT. WORTH',
  'PHOENIX (PRESCOTT)',
  'TOPEKA',
  'HOUSTON',
  'ALBUQUERQUE-SANTA FE',
  'HOUSTON',
  'LOS ANGELES',
  'SAN FRANCISCO-OAK-SAN JOSE',
  'ATLANTA',
  'LUBBOCK',
  'DES MOINES-AMES',
  'NEW YORK',
  'AUSTIN',
  'AUSTIN',
  'AUSTIN',
  'SAN ANTONIO',
  'SAN FRANCISCO-OAK-SAN JOSE',
  'LOS ANGELES',
  'LOS ANGELES',
  'MEMPHIS',
  'NEW YORK',
  'LOS ANGELES',
  'LOS ANGELES',
  'EL PASO (LAS CRUCES)',
  'HOUSTON',
  'ATLANTA',
  'NEW YORK',
  'NEW ORLEANS',
  'SAN ANTONIO',
  'DETROIT',
  'DALLAS-FT. WORTH',
  'DALLAS-FT. WORTH',
  'MIAMI-FT. LAUDERDALE',
  'HOUSTON',
  'PHOENIX (PRESCOTT)',
  'DALLAS-FT. WORTH',
  'ATLANTA',
  'CHICAGO',
  'SAN DIEGO',
  'WASHINGTON, DC (HAGRSTWN)',
  'MEMPHIS',
  'SAN DIEGO',
  'LOS ANGELES',
  'LOS ANGELES',
  'LOS ANGELES',
  'SAN DIEGO')

dates<-c('2017-01-28',
         '2017-02-03',
         '2017-04-20',
         '2017-05-18',
         '2017-04-13',
         '2017-06-08',
         '2017-08-12',
         '2017-10-19',
         '2018-07-12',
         '2018-02-13',
         '2018-09-16',
         '2018-07-05',
         '2018-07-06',
         '2018-09-13',
         '2018-06-08',
         '2018-07-04',
         '2018-11-20',
         '2018-09-07',
         '2018-10-19',
         '2018-10-13',
         '2018-05-25',
         '2018-12-15',
         '2018-10-28',
         '2018-07-09',
         '2018-06-20',
         '2018-10-27',
         '2018-10-22',
         '2018-11-10',
         '2018-11-17',
         '2018-11-21',
         '2018-01-15',
         '2018-01-11',
         '2018-01-26',
         '2018-05-13',
         '2018-02-16',
         '2019-05-05',
         '2019-05-12',
         '2019-05-18',
         '2019-05-20',
         '2019-06-20',
         '2019-05-25',
         '2019-06-01',
         '2019-02-01',
         '2019-02-08',
         '2019-01-16',
         '2019-01-25',
         '2019-04-29',
         '2019-03-18',
         '2019-03-15',
         '2019-02-23',
         '2019-03-02',
         '2019-03-16',
         '2019-03-16',
         '2019-04-06',
         '2019-04-12',
         '2019-04-20',
         '2019-04-27',
         '2019-05-04',
         '2019-05-15',
         '2019-02-19',
         '2019-05-23',
         '2019-06-21',
         '2019-06-22')

for (x in 1:length(cities)){
  print(x)
  
  storeDay$samplingEvent[storeDay$DMA_NAME == cities[x] & as.Date(dates[x]) - as.Date(storeDay$date) <=-1 & as.Date(dates[x]) - as.Date(storeDay$date) >=-30]<-1
  
}


##local media
setwd('~/Documents/Wingstop/Modeling/media')
localMedia<-read.csv("local media spend.csv", stringsAsFactors = FALSE)
localMedia$date<-as.Date(localMedia$date)
storeDay<-left_join(x = storeDay, y = localMedia[ , !names(localMedia) %in% c("month", "year")], by = c("DMA_NAME" = "DMA", "date" = "date"))
storeDay$localsponsorsSpend[is.na(storeDay$localsponsorsSpend)]<-0
storeDay$localsocialSpend[is.na(storeDay$localsocialSpend)]<-0
storeDay$localdigitalSpend[is.na(storeDay$localdigitalSpend)]<-0
storeDay$localradioSpend[is.na(storeDay$localradioSpend)]<-0
storeDay$localoohSpend[is.na(storeDay$localoohSpend)]<-0
storeDay$localotherSpend[is.na(storeDay$localotherSpend)]<-0
storeDay$localtvSpend[is.na(storeDay$localtvSpend)]<-0
storeDay$localmobileSpend[is.na(storeDay$localmobileSpend)]<-0

storeDay$localsponsorsSpend<-storeDay$localsponsorsSpend/storeDay$DMAStoreCount
storeDay$localsocialSpend<-storeDay$localsocialSpend/storeDay$DMAStoreCount
storeDay$localdigitalSpend<-storeDay$localdigitalSpend/storeDay$DMAStoreCount
storeDay$localradioSpend<-storeDay$localradioSpend/storeDay$DMAStoreCount
storeDay$localoohSpend<-storeDay$localoohSpend/storeDay$DMAStoreCount
storeDay$localotherSpend<-storeDay$localotherSpend/storeDay$DMAStoreCount
storeDay$localtvSpend<-storeDay$localtvSpend/storeDay$DMAStoreCount
storeDay$localmobileSpend<-storeDay$localmobileSpend/storeDay$DMAStoreCount


#bigUFCFight
regularUFCFights<-as.Date(c('2019-08-10',
'2019-07-13',
'2019-06-22',
'2019-06-01',
'2019-05-18',
'2019-05-04',
'2019-04-27',
'2019-04-20',
'2019-03-23',
'2019-03-16',
'2019-03-09',
'2019-02-23',
'2019-02-02',
'2019-01-19',
'2018-12-02',
'2018-11-30',
'2018-11-24',
'2018-11-17',
'2018-11-10',
'2018-10-27',
'2018-09-22',
'2018-09-15',
'2018-08-25',
'2018-07-22',
'2018-07-14',
'2018-07-06',
'2018-06-23',
'2018-06-01',
'2018-05-27',
'2018-05-19',
'2018-04-21',
'2018-03-17',
'2018-02-18',
'2018-02-03',
'2018-01-14',
'2017-12-09',
'2017-12-01',
'2017-11-25',
'2017-11-19',
'2017-11-11',
'2017-10-28',
'2017-10-21',
'2017-09-23',
'2017-09-16',
'2017-09-02',
'2017-08-05',
'2017-07-16',
'2017-07-07',
'2017-06-25',
'2017-06-17',
'2017-06-11',
'2017-05-28',
'2017-04-22',
'2017-03-18',
'2017-03-11',
'2017-02-19',
'2017-02-04',
'2017-01-15'))

storeDay$regularUFCFight<-ifelse(storeDay$date %in% regularUFCFights, 1, 0)
storeDay$ppvUFCFight<-ifelse(storeDay$UFC_fight == 1 & storeDay$regularUFCFight == 0, 1, 0)
storeWeek<-sqldf('
                 SELECT
                 StoreNo
                 ,marketStatus
                 ,DMA_NAME
                 ,week
                 ,max(year) as year
                 ,max(quintile2) as quintile2
                 ,max(quintile3) as quintile3
                 ,max(quintile4) as quintile4
                 ,max(quintile5) as quintile5
                 ,avg(samplingEvent) as samplingEvent
                 ,min(date) as WeekOf
                 ,max(ageOfStoreInWeeks) as ageOfStoreInWeeks
                 ,max(newStore30weeks) as newStore30weeks
                 ,max(DMAStoreCount) as DMAStoreCount
                 ,avg(braziliancitruspepper) as braziliancitruspepper
                 ,avg(spicykoreanq) as spicykoreanq
                 
                 ,avg(app1) as app1
                 ,avg(app2) as app2
                 ,avg(app3) as app3
                 ,avg(app4) as app4
                 ,avg(app5) as app5
                 
                 , avg(web1) as web1
                 , avg(web2) as web2
                 , avg(web3) as web3
                 , avg(web4) as web4
                 , avg(web5) as web5
                 
                 , avg(promo1) as promo1
                 , avg(promo2) as promo2
                 , avg(promo3) as promo3
                 , avg(promo4) as promo4
                 , avg(promo5) as promo5
                 
                 ,sum(TotalItemSales) as TotalItemSales
                 ,sum(totalDelivery) as totalDelivery

                 ,sum(DeliverySales) as DeliverySales
                 ,sum(DDDeliverySales) as DDDeliverySales
                 ,sum(DoorDashDeliverySales) as DoorDashDeliverySales
                 ,sum(CallDineInSales) as CallDineInSales
                 ,sum(WalkDineInSales) as WalkDineInSales
                 ,sum(CallToGoSales) as CallToGoSales
                 ,sum(WalkToGoSales) as WalkToGoSales
                 ,sum(appSales) as appSales
,sum(webMobileSales) as webMobileSales

,sum(TotalItemSales)
                 -sum(DeliverySales) 
                 -sum(DDDeliverySales)
                 -sum(DoorDashDeliverySales) 
                 -sum(CallDineInSales) 
                 -sum(WalkDineInSales) 
                 -sum(CallToGoSales) 
                 -sum(WalkToGoSales)
                 -sum(appSales) 
                 -sum(webMobileSales) as otherSales

,sum(TransactionCount) as TransactionCount
,sum(totalDeliveryTransactions) as totalDeliveryTransactions
,sum(DeliveryTransactions) as DeliveryTransactions
,sum(DDDeliveryTransactions) as DDDeliveryTransactions
,sum(DoorDashDeliveryTransactions) as DoorDashDeliveryTransactions
,sum(CallDineInTransactions) as CallDineInTransactions
,sum(WalkDineInTransactions) as WalkDineInTransactions
,sum(CallToGoTransactions) as CallToGoTransactions
,sum(WalkToGoTransactions) as WalkToGoTransactions
,sum(appTransactions) as appTransactions
,sum(webMobileTransactions) as webMobileTransactions

,sum(TransactionCount)
                 -sum(DeliveryTransactions) 
                 -sum(DDDeliveryTransactions)
                 -sum(DoorDashDeliveryTransactions) 
                 -sum(CallDineInTransactions) 
                 -sum(WalkDineInTransactions) 
                 -sum(CallToGoTransactions) 
                 -sum(WalkToGoTransactions)
                 -sum(appTransactions) 
                 -sum(webMobileTransactions) as otherTransactions

                 ,sum(OLOSocialSpend) as OLOSocialSpend
                 ,sum(OLOSearchSpend) as OLOSearchSpend
                 ,sum(OLODisplaySpend) as OLODisplaySpend
                 ,sum(OLVNationalSpend) as OLVNationalSpend
                 ,sum(OLVSocialSpend) as OLVSocialSpend
                 ,sum(DigitalSpend) as DigitalSpend
                 ,sum(tvspend) as tvspend
                 ,sum(AdSpend) as AdSpend

,sum(localsponsorsSpend) as localsponsorsSpend
,sum(localsocialSpend) as localsocialSpend
,sum(localdigitalSpend) as localdigitalSpend
,sum(localradioSpend) as localradioSpend
,sum(localoohSpend) as localoohSpend
,sum(localotherSpend) as localotherSpend
,sum(localtvSpend) as localtvSpend
,sum(localmobileSpend) as localmobileSpend
,sum(localsponsorsSpend)
+sum(localsocialSpend)
                 +sum(localdigitalSpend)
                 +sum(localradioSpend)
                 +sum(localoohSpend)
                 +sum(localotherSpend)
                 +sum(localtvSpend)
                 +sum(localmobileSpend) as localAdSpend

                 
                 ,max(MaxTemp) as MaxTemp
                 ,max(wingstops_within_3_miles) as wingstops_within_3_miles
                 ,max(Buffalo_Wild_Wings_within_3_miles) as Buffalo_Wild_Wings_within_3_miles
                 ,max(Chick_Fil_A_within_3_miles) as Chick_Fil_A_within_3_miles
                 ,max(Chipotle_within_3_miles) as Chipotle_within_3_miles
                 ,max(KFC_within_3_miles) as KFC_within_3_miles
                 ,max(Panda_Express_within_3_miles) as Panda_Express_within_3_miles
                 ,max(Pizza_Hut_within_3_miles) as Pizza_Hut_within_3_miles
                 ,max(Taco_Bell_within_3_miles) as Taco_Bell_within_3_miles
                 ,max(Whataburger_within_3_miles) as Whataburger_within_3_miles
                 ,max(Dominos_within_3_miles) as Dominos_within_3_miles
                 ,max(competitors_within_3_miles) as competitors_within_3_miles
                 
                 ,cast(avg(cast(raining as float)) as float) as raining_float
                 ,cast(avg(cast(snowing as float)) as float) as snowing_float
                 ,avg(OverallSatisfaction) as OverallSatisfaction
,avg(DigitalOSAT) as DigitalOSAT
                 ,avg(TasteOfFood) as TasteOfFood
                 ,avg(AccuracyOfOrder) as AccuracyOfOrder
                 ,avg(SpeedOfService) as SpeedOfService
                 ,avg(unemploymentRate) as unemploymentRate
                 ,max(InternalDeliveryAvailable) as InternalDeliveryAvailable
,max(DDDeliveryAvailable) as DDDeliveryAvailable
,max(DoorDashDeliveryAvailable) as DoorDashDeliveryAvailable
,max(AnyDeliveryAvailable) as AnyDeliveryAvailable
                 ,avg(price10BonelessCombor) as price10BonelessCombor
                 ,avg(price10ClassicComboj) as price10ClassicComboj
                 ,avg(price10pcBonelessr) as price10pcBonelessr
                 ,avg(price10pcClassicj) as price10pcClassicj
                 ,avg(price10SplitComboj) as price10SplitComboj
                 ,avg(price12pcClassicj) as price12pcClassicj
                 ,avg(price15BonelessCombor) as price15BonelessCombor
                 ,avg(price15ClassicComboj) as price15ClassicComboj
                 ,avg(price15pcBonelessr) as price15pcBonelessr
                 ,avg(price15pcClassicj) as price15pcClassicj
                 ,avg(price15SplitComboj) as price15SplitComboj
                 ,avg(price2pcCrispyTenders) as price2pcCrispyTenders
                 ,avg(price20OzBeverage) as price20OzBeverage
                 ,avg(price20pcBonelessr) as price20pcBonelessr
                 ,avg(price20pcClassicj) as price20pcClassicj
                 ,avg(price24pcFamilyBonelessr) as price24pcFamilyBonelessr
                 ,avg(price24pcFamilyClassicj) as price24pcFamilyClassicj
                 ,avg(price24pcFamilySplitj) as price24pcFamilySplitj
                 ,avg(price25BWBigNiteinBundle) as price25BWBigNiteinBundle
                 ,avg(price3CrispyTendersCombo) as price3CrispyTendersCombo
                 ,avg(price3pcBonelessr) as price3pcBonelessr
                 ,avg(price3pcClassicj) as price3pcClassicj
                 ,avg(price30pcClassicj) as price30pcClassicj
                 ,avg(price30pcFamilyBonelessr) as price30pcFamilyBonelessr
                 ,avg(price30pcFamilyClassicj) as price30pcFamilyClassicj
                 ,avg(price30pcFamilySplitj) as price30pcFamilySplitj
                 ,avg(price32OzBeverage) as price32OzBeverage
                 ,avg(price4pcCrispyTenders) as price4pcCrispyTenders
                 ,avg(price40pcFamilyClassicj) as price40pcFamilyClassicj
                 ,avg(price40pcFamilySplitj) as price40pcFamilySplitj
                 ,avg(price5CrispyTendersCombo) as price5CrispyTendersCombo
                 ,avg(price5pcBonelessr) as price5pcBonelessr
                 ,avg(price5pcClassicj) as price5pcClassicj
                 ,avg(price50pcFamilyClassicj) as price50pcFamilyClassicj
                 ,avg(price6BonelessCombor) as price6BonelessCombor
                 ,avg(price6ClassicComboj) as price6ClassicComboj
                 ,avg(price6SplitComboj) as price6SplitComboj
                 ,avg(price8BonelessCombor) as price8BonelessCombor
                 ,avg(price8ClassicComboj) as price8ClassicComboj
                 ,avg(price8pcBonelessr) as price8pcBonelessr
                 ,avg(price8pcClassicj) as price8pcClassicj
                 ,avg(price8SplitComboj) as price8SplitComboj
                 ,avg(priceAdd5BonelessWingsr) as priceAdd5BonelessWingsr
                 ,avg(priceAdd5ClassicWingsJ) as priceAdd5ClassicWingsJ
                 ,avg(priceBleuCheeseRegular) as priceBleuCheeseRegular
                 ,avg(priceBuffaloFF) as priceBuffaloFF
                 ,avg(priceBWLunchCombor) as priceBWLunchCombor
                 ,avg(priceCarrotSticks) as priceCarrotSticks
                 ,avg(priceCelerySticks) as priceCelerySticks
                 ,avg(priceCheeseFriesRegular) as priceCheeseFriesRegular
                 ,avg(priceCheeseSauceMedium) as priceCheeseSauceMedium
                 ,avg(priceCheeseSauceRegular) as priceCheeseSauceRegular
                 ,avg(priceCLLunchComboj) as priceCLLunchComboj
                 ,avg(priceFrenchFriesLarge) as priceFrenchFriesLarge
                 ,avg(priceFrenchFriesRegular) as priceFrenchFriesRegular
                 ,avg(priceFrenchFriesRegulare) as priceFrenchFriesRegulare
                 ,avg(priceFriedCornLarge) as priceFriedCornLarge
                 ,avg(priceFriedCornRegular) as priceFriedCornRegular
                 ,avg(priceHoneyMustardRegular) as priceHoneyMustardRegular
                 ,avg(priceRanchLarge) as priceRanchLarge
                 ,avg(priceRanchRegular) as priceRanchRegular
                 ,avg(priceRollEach) as priceRollEach
                 ,avg(priceRollsHalfDozen) as priceRollsHalfDozen
                 ,avg(priceSauce) as priceSauce
                 ,avg(priceTripleChocolateBrownie) as priceTripleChocolateBrownie
                 ,avg(priceUpgradeDrink) as priceUpgradeDrink
                 ,avg(priceVeggieSticks) as priceVeggieSticks
                 ,avg(priceVoodooFriesLarge) as priceVoodooFriesLarge
                 ,avg(priceVoodooFriesRegular) as priceVoodooFriesRegular
                 ,avg(householdsPerStore) as householdsPerStore
                 ,cast(avg(cast(hotDay as float)) as float) as hotDay_float
                 ,cast(avg(cast(coldDay as float)) as float) as coldDay_float
                 ,cast(avg(cast(spring as float)) as float) as spring_float
                 ,cast(avg(cast(summer as float)) as float) as summer_float
                 ,cast(avg(cast(fall as float)) as float) as fall_float
                 ,cast(avg(cast(winter as float)) as float) as winter_float
                 
                 
                 ,cast(avg(cast(college_football_game as float)) as float) as college_football_game_float
                 ,CASE WHEN SUM(college_football_playoffs) > 0 THEN 1 ELSE 0 END AS college_football_playoffs
                 ,CASE WHEN SUM(cultural_holiday) > 0 THEN 1 ELSE 0 END AS cultural_holiday
                 ,CASE WHEN SUM(federal_holiday) > 0 THEN 1 ELSE 0 END AS federal_holiday
                 ,CASE WHEN SUM(four_twenty) > 0 THEN 1 ELSE 0 END AS four_twenty
                 ,cast(avg(cast(game_of_thrones as float)) as float) as game_of_thrones_float
                 ,cast(avg(cast(march_madness as float)) as float) as march_madness_float 
                 ,cast(avg(cast(nfl_game as float)) as float) as nfl_game_float
                 ,cast(avg(cast(payday_calendar as float)) as float) as payday_calendar_float
                 ,CASE WHEN SUM(superbowl) > 0 THEN 1 ELSE 0 END AS superbowl
                 ,cast(avg(cast(ppvUFCFight as float)) as float) as UFC_fight_float 
                 ,CASE WHEN SUM(christmas_after) > 0 THEN 1 ELSE 0 END AS christmas
                 ,CASE WHEN SUM(easter) > 0 THEN 1 ELSE 0 END AS easter
                 ,CASE WHEN SUM(thanksgiving) > 0 THEN 1 ELSE 0 END AS thanksgiving                    
                 
                 FROM
                 storeDay as s
                 
                 GROUP BY
                 StoreNo
                 ,marketStatus
,DMA_NAME
                 ,week
                 ;')




storeWeek<-storeWeek[order(storeWeek$StoreNo,storeWeek$week), ]
mediaVars<- c("OLOSocialSpend", "OLOSearchSpend" , "OLODisplaySpend", "OLVSocialSpend", "OLVNationalSpend", "DigitalSpend", "tvspend", "AdSpend",
              "localsponsorsSpend","localsocialSpend","localdigitalSpend","localradioSpend","localoohSpend","localotherSpend","localtvSpend","localmobileSpend", "localAdSpend")

##truncate extreme media spend

for (med in mediaVars){
  eval(parse(text = paste0("storeWeek$",med,"Truncated<-storeWeek$",med)))
  eval(parse(text = paste0("storeWeek$",med,"[storeWeek$",med," > mean(storeWeek$",med,") + 2 * sd(storeWeek$",med,")]<-mean(storeWeek$",med,") + 2 * sd(storeWeek$",med,")")))
}


mediaVars<- c("OLOSocialSpend", "OLOSearchSpend" , "OLODisplaySpend", "OLVSocialSpend", "OLVNationalSpend", "DigitalSpend", "tvspend", "AdSpend",
              "localsponsorsSpend","localsocialSpend","localdigitalSpend","localradioSpend","localoohSpend","localotherSpend","localtvSpend","localmobileSpend", "localAdSpend",
              "OLOSocialSpendTruncated", "OLOSearchSpendTruncated" , "OLODisplaySpendTruncated", "OLVSocialSpendTruncated", "OLVNationalSpendTruncated", "DigitalSpendTruncated", "tvspendTruncated", "AdSpendTruncated",
              "localsponsorsSpendTruncated","localsocialSpendTruncated","localdigitalSpendTruncated","localradioSpendTruncated","localoohSpendTruncated","localotherSpendTruncated","localtvSpendTruncated","localmobileSpendTruncated", "localAdSpendTruncated")
for (med in mediaVars){
  for (i in 1:8){
    text<-paste0("storeWeek<- storeWeek %>%
                 group_by(StoreNo)%>%
                 mutate(",med, "lag", i, " = dplyr::lag(", med, ", n = ", i, ", default = NA))")
    eval(parse(text = text))
  }
}

storeWeek$marketStatusCore<-ifelse(storeWeek$marketStatus == "Core", 1, 0)
storeWeek$marketStatusDeveloping<-ifelse(storeWeek$marketStatus == "Developing", 1, 0)
storeWeek$marketStatusEmerging<-ifelse(storeWeek$marketStatus == "Emerging", 1, 0)

for (med in mediaVars){
  text<-paste0("storeWeek$", med, "Discounted<- storeWeek$", med, "+
               storeWeek$", med,"lag1 * 0.75+ 
               storeWeek$", med,"lag2 * 0.5+ 
               storeWeek$", med,"lag3 * 0.25 + 
               storeWeek$", med,"lag4 * 0.1 + 
               storeWeek$", med,"lag5 * 0.1 + 
               storeWeek$", med,"lag6 * 0.05 + 
               storeWeek$", med,"lag7 * 0.05 + 
               storeWeek$", med,"lag8 * 0.05  ")
  print(text)             
  eval(parse(text = text))
  
}

###OSAT and pricing indices
storeWeek$OverallSatisfaction[is.na(storeWeek$OverallSatisfaction)]<-mean(storeWeek$OverallSatisfaction, na.rm = TRUE)
storeWeek$DigitalOSAT[is.na(storeWeek$DigitalOSAT)]<-mean(storeWeek$DigitalOSAT, na.rm = TRUE)
priceVars<-names(storeWeek)[grepl("price", names(storeWeek))]

for (var in priceVars){
  eval(parse(text = paste0("storeWeek$", var, "[is.na(storeWeek$", var, ")]<-mean(storeWeek$", var, ", na.rm = TRUE)")))
}

storeWeek$totalPrice<-rowSums(storeWeek[ , priceVars])

storeWeek<-storeWeek %>%
  group_by(DMA_NAME, week) %>%
  mutate(DMAavgOSAT= mean(OverallSatisfaction), DMAavgDigitalOSAT= mean(DigitalOSAT),  DMAavgPrice = mean(totalPrice))

storeWeek<-storeWeek %>%
  group_by(StoreNo) %>%
  mutate(StoreavgOSAT= mean(OverallSatisfaction), StoreavgDigitalOSAT= mean(DigitalOSAT))

storeWeek<-storeWeek %>%
  group_by(DMA_NAME) %>%
  mutate(DMAOverallAvgPrice = mean(totalPrice))

storeWeek$StoreOSATIndex<-storeWeek$OverallSatisfaction/storeWeek$StoreavgOSAT * 100
storeWeek$StoreDigitalOSATIndex<-storeWeek$DigitalOSAT/storeWeek$StoreavgDigitalOSAT * 100
storeWeek$StoreOSATIndex[is.na(storeWeek$StoreOSATIndex)]<-100
storeWeek$StoreDigitalOSATIndex[is.na(storeWeek$StoreDigitalOSATIndex)]<-100

storeWeek$DMAOSATIndex<-storeWeek$OverallSatisfaction/storeWeek$DMAavgOSAT * 100
storeWeek$DMADigitalOSATIndex<-storeWeek$DigitalOSAT/storeWeek$DMAavgDigitalOSAT * 100

storeWeek<- storeWeek %>%
  group_by(StoreNo)%>%
  mutate(OSATLag5 = dplyr::lag(OverallSatisfaction, n = 5, default = NA))

storeWeek$OSATchange<-storeWeek$OverallSatisfaction - storeWeek$OSATLag5

storeWeek$OSATchange[is.na(storeWeek$OSATchange)]<-0 
storeWeek$DMAPriceIndex<-storeWeek$totalPrice/storeWeek$DMAavgPrice * 100
storeWeek$DMAavgPriceIndex<-storeWeek$DMAavgPrice/storeWeek$DMAOverallAvgPrice * 100 

storeWeek$DMAOSATIndexCentered<-storeWeek$OverallSatisfaction/storeWeek$DMAavgOSAT * 100 - 100

storeWeek$DMAPriceIndexCentered<-storeWeek$totalPrice/storeWeek$DMAavgPrice * 100 - 100
storeWeek$DMAavgPriceIndexCentered<-storeWeek$DMAavgPrice/storeWeek$DMAOverallAvgPrice * 100  - 100

storeWeek$DeliverySales[is.na(storeWeek$DeliverySales)]<-0
storeWeek$InternalDeliveryAvailable[is.na(storeWeek$InternalDeliveryAvailable)]<-0
storeWeek$DDDeliveryAvailable[is.na(storeWeek$DDDeliveryAvailable)]<-0
storeWeek$DoorDashDeliveryAvailable[is.na(storeWeek$DoorDashDeliveryAvailable)]<-0
storeWeek$AnyDeliveryAvailable[is.na(storeWeek$AnyDeliveryAvailable)]<-0

storeWeek$avgTransactionSales<-storeWeek$TotalItemSales/storeWeek$TransactionCount

storeWeek<-storeWeek[!is.na(storeWeek$DMA_NAME) , ]

#bin OSAT
summary(storeWeek$OverallSatisfaction)
storeWeek$OSATBin80<-ifelse(storeWeek$OverallSatisfaction > 0.8, 1, 0)
storeWeek$OSATBin40<-ifelse(storeWeek$OverallSatisfaction < 0.4, 1, 0)
storeWeek$OSATBin90<-ifelse(storeWeek$OverallSatisfaction > 0.9, 1, 0)
averageosat<-sqldf('
                    SELECT
                    StoreNo
                    ,avg(StoreavgOSAT) as StoreavgOSAT
                    FROM
                    storeWeek as s
                    
                    GROUP BY
                    StoreNo
                    ;')
averageosat<-averageosat %>%
  mutate(OSATquintile = ntile(StoreavgOSAT, 5))

averageosat<-averageosat[ , c("StoreNo", "OSATquintile")]
storeWeek<-left_join(x = storeWeek, y = averageosat, by = c("StoreNo" = "StoreNo"))

quintiles<-dummy(storeWeek$OSATquintile)
quintiles<-data.frame(quintiles)
  storeWeek2<-cbind(storeWeek,quintiles)


Salesfit<-lm((storeWeek$TotalItemSales)~
          storeWeek$ageOfStoreInWeeks+
          storeWeek$newStore30weeks + 
          storeWeek$ageOfStoreInWeeks*storeWeek$newStore30weeks+
          storeWeek$quintile2 + 
          storeWeek$quintile3 + 
          storeWeek$quintile4 + 
          storeWeek$quintile5 + 
          #storeWeek$DMAstorecount+
          #storeWeek$app1+
          #storeWeek$app2+
          #storeWeek$app3+
          #storeWeek$app4+
          #storeWeek$app5+
          #storeWeek$web1+
          #storeWeek$web2+
          #storeWeek$web3+
          #storeWeek$web4+
          #storeWeek$web5+
        #storeWeek$promo1+
        #storeWeek$promo2+
        #storeWeek$promo3+
        #storeWeek$promo4+
        #storeWeek$promo5+
        #storeWeek$braziliancitruspepper+
        #storeWeek$spicykoreanq+
        storeWeek$samplingEvent+
          storeWeek$raining_float+
          storeWeek$snowing_float+
          storeWeek$OverallSatisfaction+
          storeWeek$OverallSatisfaction*storeWeek$marketStatusDeveloping+
          storeWeek$OverallSatisfaction*storeWeek$marketStatusEmerging+
          #storeWeek$OSATBin40+
          storeWeek$wingstops_within_3_miles+
          storeWeek$Buffalo_Wild_Wings_within_3_miles+
          storeWeek$Chick_Fil_A_within_3_miles+
          storeWeek$Chipotle_within_3_miles+
          storeWeek$KFC_within_3_miles+
          storeWeek$Panda_Express_within_3_miles+
          storeWeek$Pizza_Hut_within_3_miles+
          storeWeek$Taco_Bell_within_3_miles+
          storeWeek$Whataburger_within_3_miles+
          storeWeek$Dominos_within_3_miles+
          storeWeek$unemploymentRate+
          storeWeek$hotDay_float+
          storeWeek$coldDay_float+
          storeWeek$spring_float+
          storeWeek$fall_float+
          storeWeek$winter_float+
          storeWeek$college_football_game_float+
          storeWeek$college_football_playoffs+
          storeWeek$cultural_holiday+
          storeWeek$federal_holiday+
          storeWeek$four_twenty+
          storeWeek$march_madness_float+
          storeWeek$nfl_game_float+
          storeWeek$payday_calendar_float+
          storeWeek$superbowl+
          storeWeek$UFC_fight_float+
          storeWeek$christmas+
          storeWeek$easter+
          storeWeek$thanksgiving+
          storeWeek$marketStatusEmerging+
          storeWeek$marketStatusDeveloping+
          storeWeek$householdsPerStore+
          
          storeWeek$OLOSocialSpendTruncated + 
          storeWeek$OLOSocialSpendTruncatedlag1 + 
          storeWeek$OLOSearchSpendTruncated +
          storeWeek$OLOSearchSpendTruncatedlag1 +
          storeWeek$OLODisplaySpendTruncated +
          storeWeek$OLODisplaySpendTruncatedlag1 +
          storeWeek$OLVSocialSpendTruncatedDiscounted +
          storeWeek$OLVNationalSpendTruncatedDiscounted +
          storeWeek$tvspendTruncatedDiscounted +
          
          storeWeek$localoohSpendDiscounted +
          storeWeek$localsponsorsSpendDiscounted +
          storeWeek$localsocialSpendDiscounted +
          storeWeek$localdigitalSpendDiscounted +
          storeWeek$localradioSpendDiscounted +
          storeWeek$localotherSpendDiscounted +
          storeWeek$localtvSpendDiscounted +
          storeWeek$localmobileSpendDiscounted +
          
          # 
          # storeWeek$OLOSocialSpend + 
          # storeWeek$OLOSocialSpendlag1 + 
          # storeWeek$OLOSearchSpendTruncated +
          # storeWeek$OLOSearchSpendlag1Truncated +
          # storeWeek$OLODisplaySpend +
          # storeWeek$OLODisplaySpendlag1 +
          # storeWeek$OLVSocialSpendDiscounted +
          # storeWeek$OLVNationalSpendDiscounted +
          # storeWeek$tvspendDiscounted +
          # 
          # storeWeek$localoohSpendDiscounted +
          # storeWeek$localsponsorsSpendDiscounted +
          # storeWeek$localsocialSpendDiscounted +
          # storeWeek$localdigitalSpendDiscounted +
          # storeWeek$localradioSpendDiscounted +
          # storeWeek$localotherSpendDiscounted +
          # storeWeek$localtvSpendDiscounted +
          # storeWeek$localmobileSpendDiscounted +
          
          storeWeek$DoorDashDeliveryAvailable +
          storeWeek$marketStatusEmerging * storeWeek$DoorDashDeliveryAvailable +
          storeWeek$marketStatusDeveloping* storeWeek$DoorDashDeliveryAvailable + 
          
          storeWeek$DDDeliveryAvailable +
          storeWeek$marketStatusEmerging * storeWeek$DDDeliveryAvailable +
          storeWeek$marketStatusDeveloping* storeWeek$DDDeliveryAvailable + 
          
          storeWeek$InternalDeliveryAvailable +
          storeWeek$marketStatusEmerging * storeWeek$InternalDeliveryAvailable +
          storeWeek$marketStatusDeveloping* storeWeek$InternalDeliveryAvailable 
)

summary(Salesfit)



Transactionsfit<-lm((storeWeek$TransactionCount)~
          storeWeek$DMAavgPriceIndexCentered+
          storeWeek$DMAPriceIndexCentered+
            storeWeek$ageOfStoreInWeeks+
            storeWeek$newStore30weeks + 
            storeWeek$ageOfStoreInWeeks*storeWeek$newStore30weeks+
            storeWeek$quintile2 + 
            storeWeek$quintile3 + 
            storeWeek$quintile4 + 
            storeWeek$quintile5 + 
            #storeWeek$DMAstorecount+
            #storeWeek$app1+
            #storeWeek$app2+
            #storeWeek$app3+
            #storeWeek$app4+
            #storeWeek$app5+
            #storeWeek$web1+
            #storeWeek$web2+
            #storeWeek$web3+
            #storeWeek$web4+
            #storeWeek$web5+
          #storeWeek$promo1+
          #storeWeek$promo2+
          #storeWeek$promo3+
          #storeWeek$promo4+
          #storeWeek$promo5+
          #storeWeek$braziliancitruspepper+
          #storeWeek$spicykoreanq+
          storeWeek$samplingEvent+
            storeWeek$raining_float+
            storeWeek$snowing_float+
            storeWeek$StoreOSATIndex+
            storeWeek$wingstops_within_3_miles+
            storeWeek$Buffalo_Wild_Wings_within_3_miles+
            storeWeek$Chick_Fil_A_within_3_miles+
            storeWeek$Chipotle_within_3_miles+
            storeWeek$KFC_within_3_miles+
            storeWeek$Panda_Express_within_3_miles+
            storeWeek$Pizza_Hut_within_3_miles+
            storeWeek$Taco_Bell_within_3_miles+
            storeWeek$Whataburger_within_3_miles+
            storeWeek$Dominos_within_3_miles+
            storeWeek$unemploymentRate+
            storeWeek$hotDay_float+
            storeWeek$coldDay_float+
            storeWeek$spring_float+
            storeWeek$fall_float+
            storeWeek$winter_float+
            storeWeek$college_football_game_float+
            storeWeek$college_football_playoffs+
            storeWeek$cultural_holiday+
            storeWeek$federal_holiday+
            storeWeek$four_twenty+
            storeWeek$march_madness_float+
            storeWeek$nfl_game_float+
            storeWeek$payday_calendar_float+
            storeWeek$superbowl+
            storeWeek$UFC_fight_float+
            storeWeek$christmas+
            storeWeek$easter+
            storeWeek$thanksgiving+
            storeWeek$marketStatusEmerging+
            storeWeek$marketStatusDeveloping+
            storeWeek$householdsPerStore+
            
            storeWeek$OLOSocialSpendTruncated + 
            storeWeek$OLOSocialSpendTruncatedlag1 + 
            storeWeek$OLOSearchSpendTruncated +
            storeWeek$OLOSearchSpendTruncatedlag1 +
            storeWeek$OLODisplaySpendTruncated +
            storeWeek$OLODisplaySpendTruncatedlag1 +
            storeWeek$OLVSocialSpendTruncatedDiscounted +
            storeWeek$OLVNationalSpendTruncatedDiscounted +
            storeWeek$tvspendTruncatedDiscounted +
            
            storeWeek$localoohSpendDiscounted +
            storeWeek$localsponsorsSpendDiscounted +
            storeWeek$localsocialSpendDiscounted +
            storeWeek$localdigitalSpendDiscounted +
            storeWeek$localradioSpendDiscounted +
            storeWeek$localotherSpendDiscounted +
            storeWeek$localtvSpendDiscounted +
            storeWeek$localmobileSpendDiscounted +
            
            # 
            # storeWeek$OLOSocialSpend + 
            # storeWeek$OLOSocialSpendlag1 + 
            # storeWeek$OLOSearchSpendTruncated +
            # storeWeek$OLOSearchSpendlag1Truncated +
            # storeWeek$OLODisplaySpend +
            # storeWeek$OLODisplaySpendlag1 +
            # storeWeek$OLVSocialSpendDiscounted +
            # storeWeek$OLVNationalSpendDiscounted +
            # storeWeek$tvspendDiscounted +
          # 
          # storeWeek$localoohSpendDiscounted +
          # storeWeek$localsponsorsSpendDiscounted +
          # storeWeek$localsocialSpendDiscounted +
          # storeWeek$localdigitalSpendDiscounted +
          # storeWeek$localradioSpendDiscounted +
          # storeWeek$localotherSpendDiscounted +
          # storeWeek$localtvSpendDiscounted +
          # storeWeek$localmobileSpendDiscounted +
          
          storeWeek$DoorDashDeliveryAvailable +
            storeWeek$marketStatusEmerging * storeWeek$DoorDashDeliveryAvailable +
            storeWeek$marketStatusDeveloping* storeWeek$DoorDashDeliveryAvailable + 
            
            storeWeek$DDDeliveryAvailable +
            storeWeek$marketStatusEmerging * storeWeek$DDDeliveryAvailable +
            storeWeek$marketStatusDeveloping* storeWeek$DDDeliveryAvailable + 
            
            storeWeek$InternalDeliveryAvailable +
            storeWeek$marketStatusEmerging * storeWeek$InternalDeliveryAvailable +
            storeWeek$marketStatusDeveloping* storeWeek$InternalDeliveryAvailable 
)

summary(Transactionsfit)


##waterfalls
First52Weeks<-storeWeek[7 < storeWeek$week & storeWeek$week < 60 & !is.na(storeWeek$AdSpendDiscounted), ]
Last52Weeks<-storeWeek[storeWeek$week > max(storeWeek$week) - 52 & !is.na(storeWeek$AdSpendDiscounted), ]

JanMay2018<-storeWeek[51 < storeWeek$week & storeWeek$week < 72 & !is.na(storeWeek$AdSpendDiscounted), ]
JanMay2019<-storeWeek[104 < storeWeek$week & storeWeek$week < 125 & !is.na(storeWeek$AdSpendDiscounted), ]
JanMay2019$InternalDeliveryAvailable[is.na(JanMay2019$InternalDeliveryAvailable)]<-0
WaterfallOutput<-NULL
SalesCoefs<-coef(Salesfit)
for (var in 1:length(names(SalesCoefs))){
  if (var == 1){
    outValueFirst52 <- SalesCoefs[1] * nrow(First52Weeks)
    OutValueLast52 <- SalesCoefs[1] * nrow(Last52Weeks)
    OutValueJanMay2018 <- SalesCoefs[1] * nrow(JanMay2018)
    OutValueJanMay2019 <- SalesCoefs[1] * nrow(JanMay2019)
    WaterfallOutput<-rbind(WaterfallOutput, c("Intercept", SalesCoefs[1], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019))
    
  } else{
    thisVar<-gsub("storeWeek\\$","",  names(SalesCoefs)[var])
    theseVars<-str_split_fixed(thisVar, ":", 2)
    if (theseVars[ , 2] == ""){
      outValueFirst52 <- SalesCoefs[var] * sum(First52Weeks[ , thisVar])
      OutValueLast52 <- SalesCoefs[var] * sum(Last52Weeks[ , thisVar])
      OutValueJanMay2018 <- SalesCoefs[var] * sum(JanMay2018[ , thisVar])
      OutValueJanMay2019 <- SalesCoefs[var] * sum(JanMay2019[ , thisVar])
    } else {
      outValueFirst52 <- SalesCoefs[var] * sum(First52Weeks[ , theseVars[ , 1]] * First52Weeks[ , theseVars[ , 2]])
      OutValueLast52 <- SalesCoefs[var] * sum(Last52Weeks[ , theseVars[ , 1]] * Last52Weeks[ , theseVars[ , 2]])
      OutValueJanMay2018 <- SalesCoefs[var] * sum(JanMay2018[ , theseVars[ , 1]] * JanMay2018[ , theseVars[ , 2]])
      OutValueJanMay2019 <- SalesCoefs[var] * sum(JanMay2019[ , theseVars[ , 1]] * JanMay2019[ , theseVars[ , 2]])
    }
    
    
    WaterfallOutput<-rbind(WaterfallOutput, c(thisVar, SalesCoefs[var], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019))
  }
}
write.csv(WaterfallOutput, "sales waterfall output.csv")

WaterfallOutput<-NULL
TransactionsCoefs<-coef(Transactionsfit)
for (var in 1:length(names(TransactionsCoefs))){
  if (var == 1){
    outValueFirst52 <- TransactionsCoefs[1] * nrow(First52Weeks)
    OutValueLast52 <- TransactionsCoefs[1] * nrow(Last52Weeks)
    OutValueJanMay2018 <- TransactionsCoefs[1] * nrow(JanMay2018)
    OutValueJanMay2019 <- TransactionsCoefs[1] * nrow(JanMay2019)
    WaterfallOutput<-rbind(WaterfallOutput, c("Intercept", TransactionsCoefs[1], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019))
    
  } else{
    thisVar<-gsub("storeWeek\\$","",  names(TransactionsCoefs)[var])
    theseVars<-str_split_fixed(thisVar, ":", 2)
    if (theseVars[ , 2] == ""){
      outValueFirst52 <- TransactionsCoefs[var] * sum(First52Weeks[ , thisVar])
      OutValueLast52 <- TransactionsCoefs[var] * sum(Last52Weeks[ , thisVar])
      OutValueJanMay2018 <- TransactionsCoefs[var] * sum(JanMay2018[ , thisVar])
      OutValueJanMay2019 <- TransactionsCoefs[var] * sum(JanMay2019[ , thisVar])
    } else {
      outValueFirst52 <- TransactionsCoefs[var] * sum(First52Weeks[ , theseVars[ , 1]] * First52Weeks[ , theseVars[ , 2]])
      OutValueLast52 <- TransactionsCoefs[var] * sum(Last52Weeks[ , theseVars[ , 1]] * Last52Weeks[ , theseVars[ , 2]])
      OutValueJanMay2018 <- TransactionsCoefs[var] * sum(JanMay2018[ , theseVars[ , 1]] * JanMay2018[ , theseVars[ , 2]])
      OutValueJanMay2019 <- TransactionsCoefs[var] * sum(JanMay2019[ , theseVars[ , 1]] * JanMay2019[ , theseVars[ , 2]])
    }
    
    
    WaterfallOutput<-rbind(WaterfallOutput, c(thisVar, TransactionsCoefs[var], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019))
  }
}
write.csv(WaterfallOutput, "transactions waterfall output.csv")

###build waterfalls
transWaterfall<-read.csv("transactions waterfall output v2.csv", stringsAsFactors = FALSE)

ssalesfit <- transWaterfall %>%
  filter(Significant == 1) %>%
  group_by(Category) %>%
  summarise(inc = sum(DelPercent)) %>%
  ungroup()

ssalesfit<-ssalesfit[!ssalesfit$Category %in% c("Price", "Sports") , ]

ssalesfit$vcat = as.factor(ssalesfit$Category)

p <- ggplot(data = ssalesfit, aes(x= reorder(as.factor(Category),desc(inc)),y = inc/sum(inc),fill=vcat)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent,limits=c(-.15,1)) +
  ylab("Contribution to Growth") +
  geom_text(label=ssalesfit$Category,size=3,angle=90,hjust=0) +
  ggtitle("Contribution to Average Store Transaction Growth \n Jan-May 2018 to Jan-May 2019") +
  labs(fill = "Category") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust=.5)
  )

p

salesWaterfall<-read.csv("sales waterfall output v2.csv", stringsAsFactors = FALSE)

ssalesfit <- salesWaterfall %>%
  filter(Significant == 1) %>%
  group_by(Category) %>%
  summarise(inc = sum(DelPercent)) %>%
  ungroup()

ssalesfit<-ssalesfit[!ssalesfit$Category %in% c("Price", "Sports") , ]

ssalesfit$vcat = as.factor(ssalesfit$Category)

p <- ggplot(data = ssalesfit, aes(x= reorder(as.factor(Category),desc(inc)),y = inc/sum(inc),fill=vcat)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent,limits=c(-.15,1)) +
  ylab("Contribution to Growth") +
  geom_text(label=ssalesfit$Category,size=3,angle=90,hjust=0) +
  ggtitle("Contribution to Average Store Sales Growth \n Jan-May 2018 to Jan-May 2019") +
  labs(fill = "Category") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust=.5)
  )

p

###contribution to actual sales/transactions
WaterfallOutput<-NULL
SalesCoefs<-coef(Salesfit)
for (var in 1:length(names(SalesCoefs))){
  if (var == 1){
    outValueFirst52 <- SalesCoefs[1] * nrow(First52Weeks)
    OutValueLast52 <- SalesCoefs[1] * nrow(Last52Weeks)
    OutValueJanMay2018 <- SalesCoefs[1] * nrow(JanMay2018)
    OutValueJanMay2019 <- SalesCoefs[1] * nrow(JanMay2019)
    fullContribution<-SalesCoefs[1] * nrow(storeWeek)
    WaterfallOutput<-rbind(WaterfallOutput, c("Intercept", SalesCoefs[1], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019, fullContribution))
    
  } else{
    thisVar<-gsub("storeWeek\\$","",  names(SalesCoefs)[var])
    theseVars<-str_split_fixed(thisVar, ":", 2)
    if (theseVars[ , 2] == ""){
      outValueFirst52 <- SalesCoefs[var] * sum(First52Weeks[ , thisVar])
      OutValueLast52 <- SalesCoefs[var] * sum(Last52Weeks[ , thisVar])
      OutValueJanMay2018 <- SalesCoefs[var] * sum(JanMay2018[ , thisVar])
      OutValueJanMay2019 <- SalesCoefs[var] * sum(JanMay2019[ , thisVar])
      fullContribution<-SalesCoefs[var] *  sum(storeWeek[ , thisVar], na.rm = TRUE)
    } else {
      outValueFirst52 <- SalesCoefs[var] * sum(First52Weeks[ , theseVars[ , 1]] * First52Weeks[ , theseVars[ , 2]])
      OutValueLast52 <- SalesCoefs[var] * sum(Last52Weeks[ , theseVars[ , 1]] * Last52Weeks[ , theseVars[ , 2]])
      OutValueJanMay2018 <- SalesCoefs[var] * sum(JanMay2018[ , theseVars[ , 1]] * JanMay2018[ , theseVars[ , 2]])
      OutValueJanMay2019 <- SalesCoefs[var] * sum(JanMay2019[ , theseVars[ , 1]] * JanMay2019[ , theseVars[ , 2]])
      fullContribution <- SalesCoefs[var] * sum(storeWeek[ , theseVars[ , 1]] * storeWeek[ , theseVars[ , 2]])
    }
    
    
    WaterfallOutput<-rbind(WaterfallOutput, c(thisVar, SalesCoefs[var], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019, fullContribution))
  }
}
write.csv(WaterfallOutput, "sales waterfall output full contribution.csv")

WaterfallOutput<-NULL
TransactionsCoefs<-coef(Transactionsfit)
for (var in 1:length(names(TransactionsCoefs))){
  print(names(TransactionsCoefs)[var])
  if (var == 1){
    outValueFirst52 <- TransactionsCoefs[1] * nrow(First52Weeks)
    OutValueLast52 <- TransactionsCoefs[1] * nrow(Last52Weeks)
    OutValueJanMay2018 <- TransactionsCoefs[1] * nrow(JanMay2018)
    OutValueJanMay2019 <- TransactionsCoefs[1] * nrow(JanMay2019)
    fullContribution<-TransactionsCoefs[var] *  nrow(storeWeek)
    WaterfallOutput<-rbind(WaterfallOutput, c("Intercept", TransactionsCoefs[1], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019,fullContribution))
    
  } else{
    thisVar<-gsub("storeWeek\\$","",  names(TransactionsCoefs)[var])
    theseVars<-str_split_fixed(thisVar, ":", 2)
    print(thisVar)
    if (theseVars[ , 2] == ""){
      outValueFirst52 <- TransactionsCoefs[var] * sum(First52Weeks[ , thisVar])
      OutValueLast52 <- TransactionsCoefs[var] * sum(Last52Weeks[ , thisVar])
      OutValueJanMay2018 <- TransactionsCoefs[var] * sum(JanMay2018[ , thisVar])
      OutValueJanMay2019 <- TransactionsCoefs[var] * sum(JanMay2019[ , thisVar])
      fullContribution<-TransactionsCoefs[var] *  sum(storeWeek[ , thisVar], na.rm = TRUE)
    } else {
      outValueFirst52 <- TransactionsCoefs[var] * sum(First52Weeks[ , theseVars[ , 1]] * First52Weeks[ , theseVars[ , 2]])
      OutValueLast52 <- TransactionsCoefs[var] * sum(Last52Weeks[ , theseVars[ , 1]] * Last52Weeks[ , theseVars[ , 2]])
      OutValueJanMay2018 <- TransactionsCoefs[var] * sum(JanMay2018[ , theseVars[ , 1]] * JanMay2018[ , theseVars[ , 2]])
      OutValueJanMay2019 <- TransactionsCoefs[var] * sum(JanMay2019[ , theseVars[ , 1]] * JanMay2019[ , theseVars[ , 2]])
      fullContribution <- TransactionsCoefs[var] * sum(storeWeek[ , theseVars[ , 1]] * storeWeek[ , theseVars[ , 2]])
    }
    
    
    WaterfallOutput<-rbind(WaterfallOutput, c(thisVar, TransactionsCoefs[var], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019, fullContribution))
  }
}
write.csv(WaterfallOutput, "transactions waterfall output full contribution.csv")

transWaterfall<-read.csv("transactions waterfall output v2.csv", stringsAsFactors = FALSE)

ssalesfit <- transWaterfall %>%
  filter(Significant == 1) %>%
  group_by(Category) %>%
  summarise(inc = sum(DelPercent)) %>%
  ungroup()

ssalesfit<-ssalesfit[!ssalesfit$Category %in% c("Price", "Sports") , ]

ssalesfit$vcat = as.factor(ssalesfit$Category)

p <- ggplot(data = ssalesfit, aes(x= reorder(as.factor(Category),desc(inc)),y = inc/sum(inc),fill=vcat)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent,limits=c(-.15,1)) +
  ylab("Contribution to Growth") +
  geom_text(label=ssalesfit$Category,size=3,angle=90,hjust=0) +
  ggtitle("Contribution to Average Store Transaction Growth \n Jan-May 2018 to Jan-May 2019") +
  labs(fill = "Category") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust=.5)
  )

p

salesWaterfall<-read.csv("sales waterfall output full contribution.csv", stringsAsFactors = FALSE)

ssalesfit <- salesWaterfall %>%
  filter(Significant == 1) %>%
  group_by(Category) %>%
  summarise(inc = sum(PercentNoBase)) %>%
  ungroup()

ssalesfit<-ssalesfit[!ssalesfit$Category %in% c("Price") , ]

ssalesfit$vcat = as.factor(ssalesfit$Category)

p <- ggplot(data = ssalesfit, aes(x= reorder(as.factor(Category),desc(inc)),y = inc/sum(inc),fill=vcat)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent,limits=c(-.15,1)) +
  ylab("Contribution to Sales") +
  geom_text(label=ssalesfit$Category,size=3,angle=90,hjust=0) +
  ggtitle("Contribution to Total Sales, Excluding Base") +
  labs(fill = "Category") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust=.5)
  )

p


ssalesfit <- salesWaterfall %>%
  filter(Significant == 1) %>%
  group_by(Category) %>%
  summarise(inc = sum(Percent)) %>%
  ungroup()

ssalesfit<-ssalesfit[!ssalesfit$Category %in% c("Price") , ]

ssalesfit$vcat = as.factor(ssalesfit$Category)

p <- ggplot(data = ssalesfit, aes(x= reorder(as.factor(Category),desc(inc)),y = inc/sum(inc),fill=vcat)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent,limits=c(-.15,1)) +
  ylab("Contribution to Sales") +
  geom_text(label=ssalesfit$Category,size=3,angle=90,hjust=0) +
  ggtitle("Contribution to Total Sales") +
  labs(fill = "Category") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust=.5)
  )

p

transWaterfall<-read.csv("transactions waterfall output full contribution.csv", stringsAsFactors = FALSE)

ssalesfit <- transWaterfall %>%
  filter(Significant == 1) %>%
  group_by(Category) %>%
  summarise(inc = sum(as.numeric(PercentNoBase))) %>%
  ungroup()

ssalesfit<-ssalesfit[!ssalesfit$Category %in% c("Price") , ]

ssalesfit$vcat = as.factor(ssalesfit$Category)

p <- ggplot(data = ssalesfit, aes(x= reorder(as.factor(Category),desc(inc)),y = inc/sum(inc),fill=vcat)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent,limits=c(-.15,1)) +
  ylab("Contribution to Transactions") +
  geom_text(label=ssalesfit$Category,size=3,angle=90,hjust=0) +
  ggtitle("Contribution to Total Transactions, Excluding Base") +
  labs(fill = "Category") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust=.5)
  )

p

ssalesfit <- transWaterfall %>%
  filter(Significant == 1) %>%
  group_by(Category) %>%
  summarise(inc = sum(Percent)) %>%
  ungroup()

ssalesfit<-ssalesfit[!ssalesfit$Category %in% c("Price") , ]

ssalesfit$vcat = as.factor(ssalesfit$Category)

p <- ggplot(data = ssalesfit, aes(x= reorder(as.factor(Category),desc(inc)),y = inc/sum(inc),fill=vcat)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent,limits=c(-.15,1)) +
  ylab("Contribution to Transactions") +
  geom_text(label=ssalesfit$Category,size=3,angle=90,hjust=0) +
  ggtitle("Contribution to Total Transactions") +
  labs(fill = "Category") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust=.5)
  )

p


#*HT*# storeWeek<-read.csv("C:/Users/hthapa/OneDrive - Epsilon/Projects/Wingstop MMM/RawData/StoreWeekDataset.csv", stringsAsFactors = FALSE)

#*HT*# storeDay<-write.csv(storeDay, file="C:/Users/hthapa/OneDrive - Epsilon/Projects/Wingstop MMM/RawData/StoreDay.csv")

saveRDS(storeWeek, "store week data.RDS")

###############################
##### national model ##########
###############################

dmaStorePcts<-storeWeek[storeWeek$marketStatus %in% c("Core", "Developing"),] %>%
  group_by(DMA_NAME, week) %>%
  summarise(DMAStores = n_distinct(StoreNo))

dmaList<-unique(dmaStorePcts$DMA_NAME)
dmaWide<-dcast(dmaStorePcts, week ~ DMA_NAME, value.var = "DMAStores")
dmaWide[is.na(dmaWide)]<-0
natlStorePcts<-storeWeek %>%
  group_by( week) %>%
  summarise(NationalStores = n_distinct(StoreNo))

allStoreCounts<-left_join(x = dmaWide, y = natlStorePcts, by = c("week"))

dmaCleanList<-NULL
for (dma in dmaList){
  dmaClean<-gsub(" ", "", dma)
  dmaClean<-gsub("-", "", dmaClean)
  dmaClean<-gsub("\\.", "", dmaClean)
  dmaClean<-gsub(",", "", dmaClean)
  dmaClean<-gsub("\\)", "", dmaClean)
  dmaClean<-gsub("\\(", "", dmaClean)
  dmaClean<-gsub("&", "", dmaClean)
  dmaCleanList<-c(dmaCleanList, dmaClean)
  eval(parse(text = paste0("allStoreCounts$",dmaClean,"<-allStoreCounts$`",dma,"` / allStoreCounts$NationalStores")))
  #allStoreCounts$`dma`<-allStoreCounts$`dma` / allStoreCounts$NationalStores
}

#*HT*# -- Creating nationweek using storeweek

nationWeek<-sqldf('
                 SELECT
                  week
                 ,max(year) as year
                 ,count(distinct StoreNo) as StoreCount
                 ,sum(newStore30weeks)/count(distinct StoreNo) as pctNewStores
               
                 , min(Weekof) as WeekOf
              
                 , avg(braziliancitruspepper) as braziliancitruspepper
                 , avg(spicykoreanq) as spicykoreanq
                 
                 , avg(app1) as app1
                 , avg(app2) as app2
                 , avg(app3) as app3
                 , avg(app4) as app4
                 , avg(app5) as app5
                 
                 , avg(web1) as web1
                 , avg(web2) as web2
                 , avg(web3) as web3
                 , avg(web4) as web4
                 , avg(web5) as web5
                 
                 , avg(promo1) as promo1
                 , avg(promo2) as promo2
                 , avg(promo3) as promo3
                 , avg(promo4) as promo4
                 , avg(promo5) as promo5
                 
                 ,sum(TotalItemSales) as TotalItemSales
                
                 
                 ,sum(TransactionCount) as TransactionCount
                
                 ,sum(OLOSocialSpend) as OLOSocialSpend
                 ,sum(OLOSearchSpend) as OLOSearchSpend
                 ,sum(OLODisplaySpend) as OLODisplaySpend
                 ,sum(OLVNationalSpend) as OLVNationalSpend
                 ,sum(OLVSocialSpend) as OLVSocialSpend
                 ,sum(DigitalSpend) as DigitalSpend
                 ,sum(tvspend) as tvspend
                 ,sum(AdSpend) as AdSpend
                 
                 ,sum(localsponsorsSpend) as localsponsorsSpend
                 ,sum(localsocialSpend) as localsocialSpend
                 ,sum(localdigitalSpend) as localdigitalSpend
                 ,sum(localradioSpend) as localradioSpend
                 ,sum(localoohSpend) as localoohSpend
                 ,sum(localotherSpend) as localotherSpend
                 ,sum(localtvSpend) as localtvSpend
                 ,sum(localmobileSpend) as localmobileSpend
                 ,sum(localsponsorsSpend)
                 +sum(localsocialSpend)
                 +sum(localdigitalSpend)
                 +sum(localradioSpend)
                 +sum(localoohSpend)
                 +sum(localotherSpend)
                 +sum(localtvSpend)
                 +sum(localmobileSpend) as localAdSpend
                 
                 
                 ,cast(avg(cast(raining_float as float)) as float) as raining_float
                 ,cast(avg(cast(snowing_float as float)) as float) as snowing_float
                
                 ,avg(unemploymentRate) as unemploymentRate
                 ,cast(avg(cast(InternalDeliveryAvailable as float)) as float) as InternalDeliveryAvailable_float
                 ,cast(avg(cast(DDDeliveryAvailable as float)) as float) as DDDeliveryAvailable_float
                 ,cast(avg(cast(DoorDashDeliveryAvailable as float)) as float) as DoorDashDeliveryAvailable_float

                 ,sum(case when InternalDeliveryAvailable > 0 then 1 else 0 end) as InternalDeliveryAvailable
                 ,sum(case when DDDeliveryAvailable > 0 then 1 else 0 end) as DDDeliveryAvailable
                 ,sum(case when DoorDashDeliveryAvailable > 0 then 1 else 0 end) as DoorDashDeliveryAvailable


                 ,max(AnyDeliveryAvailable) as AnyDeliveryAvailable
                 ,avg(price10BonelessCombor) as price10BonelessCombor
                 ,avg(price10ClassicComboj) as price10ClassicComboj
                 ,avg(price10pcBonelessr) as price10pcBonelessr
                 ,avg(price10pcClassicj) as price10pcClassicj
                 ,avg(price10SplitComboj) as price10SplitComboj
                 ,avg(price12pcClassicj) as price12pcClassicj
                 ,avg(price15BonelessCombor) as price15BonelessCombor
                 ,avg(price15ClassicComboj) as price15ClassicComboj
                 ,avg(price15pcBonelessr) as price15pcBonelessr
                 ,avg(price15pcClassicj) as price15pcClassicj
                 ,avg(price15SplitComboj) as price15SplitComboj
                 ,avg(price2pcCrispyTenders) as price2pcCrispyTenders
                 ,avg(price20OzBeverage) as price20OzBeverage
                 ,avg(price20pcBonelessr) as price20pcBonelessr
                 ,avg(price20pcClassicj) as price20pcClassicj
                 ,avg(price24pcFamilyBonelessr) as price24pcFamilyBonelessr
                 ,avg(price24pcFamilyClassicj) as price24pcFamilyClassicj
                 ,avg(price24pcFamilySplitj) as price24pcFamilySplitj
                 ,avg(price25BWBigNiteinBundle) as price25BWBigNiteinBundle
                 ,avg(price3CrispyTendersCombo) as price3CrispyTendersCombo
                 ,avg(price3pcBonelessr) as price3pcBonelessr
                 ,avg(price3pcClassicj) as price3pcClassicj
                 ,avg(price30pcClassicj) as price30pcClassicj
                 ,avg(price30pcFamilyBonelessr) as price30pcFamilyBonelessr
                 ,avg(price30pcFamilyClassicj) as price30pcFamilyClassicj
                 ,avg(price30pcFamilySplitj) as price30pcFamilySplitj
                 ,avg(price32OzBeverage) as price32OzBeverage
                 ,avg(price4pcCrispyTenders) as price4pcCrispyTenders
                 ,avg(price40pcFamilyClassicj) as price40pcFamilyClassicj
                 ,avg(price40pcFamilySplitj) as price40pcFamilySplitj
                 ,avg(price5CrispyTendersCombo) as price5CrispyTendersCombo
                 ,avg(price5pcBonelessr) as price5pcBonelessr
                 ,avg(price5pcClassicj) as price5pcClassicj
                 ,avg(price50pcFamilyClassicj) as price50pcFamilyClassicj
                 ,avg(price6BonelessCombor) as price6BonelessCombor
                 ,avg(price6ClassicComboj) as price6ClassicComboj
                 ,avg(price6SplitComboj) as price6SplitComboj
                 ,avg(price8BonelessCombor) as price8BonelessCombor
                 ,avg(price8ClassicComboj) as price8ClassicComboj
                 ,avg(price8pcBonelessr) as price8pcBonelessr
                 ,avg(price8pcClassicj) as price8pcClassicj
                 ,avg(price8SplitComboj) as price8SplitComboj
                 ,avg(priceAdd5BonelessWingsr) as priceAdd5BonelessWingsr
                 ,avg(priceAdd5ClassicWingsJ) as priceAdd5ClassicWingsJ
                 ,avg(priceBleuCheeseRegular) as priceBleuCheeseRegular
                 ,avg(priceBuffaloFF) as priceBuffaloFF
                 ,avg(priceBWLunchCombor) as priceBWLunchCombor
                 ,avg(priceCarrotSticks) as priceCarrotSticks
                 ,avg(priceCelerySticks) as priceCelerySticks
                 ,avg(priceCheeseFriesRegular) as priceCheeseFriesRegular
                 ,avg(priceCheeseSauceMedium) as priceCheeseSauceMedium
                 ,avg(priceCheeseSauceRegular) as priceCheeseSauceRegular
                 ,avg(priceCLLunchComboj) as priceCLLunchComboj
                 ,avg(priceFrenchFriesLarge) as priceFrenchFriesLarge
                 ,avg(priceFrenchFriesRegular) as priceFrenchFriesRegular
                 ,avg(priceFrenchFriesRegulare) as priceFrenchFriesRegulare
                 ,avg(priceFriedCornLarge) as priceFriedCornLarge
                 ,avg(priceFriedCornRegular) as priceFriedCornRegular
                 ,avg(priceHoneyMustardRegular) as priceHoneyMustardRegular
                 ,avg(priceRanchLarge) as priceRanchLarge
                 ,avg(priceRanchRegular) as priceRanchRegular
                 ,avg(priceRollEach) as priceRollEach
                 ,avg(priceRollsHalfDozen) as priceRollsHalfDozen
                 ,avg(priceSauce) as priceSauce
                 ,avg(priceTripleChocolateBrownie) as priceTripleChocolateBrownie
                 ,avg(priceUpgradeDrink) as priceUpgradeDrink
                 ,avg(priceVeggieSticks) as priceVeggieSticks
                 ,avg(priceVoodooFriesLarge) as priceVoodooFriesLarge
                 ,avg(priceVoodooFriesRegular) as priceVoodooFriesRegular
                 ,avg(householdsPerStore) as householdsPerStore
                 ,cast(avg(cast(hotDay_float as float)) as float) as hotDay_float
                 ,cast(avg(cast(coldDay_float as float)) as float) as coldDay_float
                 ,cast(avg(cast(spring_float as float)) as float) as spring_float
                 ,cast(avg(cast(summer_float as float)) as float) as summer_float
                 ,cast(avg(cast(fall_float as float)) as float) as fall_float
                 ,cast(avg(cast(winter_float as float)) as float) as winter_float
                 
                  ,cast(avg(cast(DigitalOSAT as float)) as float) as DigitalOSAT_float
                  ,sum(case when wingstops_within_3_miles > 0 then 1 else 0 end) as wingstops_within_3_miles
                  ,sum(case when Buffalo_Wild_Wings_within_3_miles > 0 then 1 else 0 end) as Buffalo_Wild_Wings_within_3_miles
                  ,sum(case when Chick_Fil_A_within_3_miles > 0 then 1 else 0 end) as Chick_Fil_A_within_3_miles
                  ,sum(case when KFC_within_3_miles > 0 then 1 else 0 end) as KFC_within_3_miles
                  ,sum(case when Chipotle_within_3_miles > 0 then 1 else 0 end) as Chipotle_within_3_miles
                  ,sum(case when Pizza_Hut_within_3_miles > 0 then 1 else 0 end) as Pizza_Hut_within_3_miles
                  ,sum(case when Panda_Express_within_3_miles > 0 then 1 else 0 end) as Panda_Express_within_3_miles
                  ,sum(case when Pizza_Hut_within_3_miles > 0 then 1 else 0 end) as Pizza_Hut_within_3_miles
                  ,sum(case when Taco_Bell_within_3_miles > 0 then 1 else 0 end) as Taco_Bell_within_3_miles
                  ,sum(case when Whataburger_within_3_miles > 0 then 1 else 0 end) as Whataburger_within_3_miles
                  ,sum(case when Dominos_within_3_miles > 0 then 1 else 0 end) as Dominos_within_3_miles
     
                 
                 ,cast(avg(cast(college_football_game_float as float)) as float) as college_football_game_float
                 ,CASE WHEN SUM(college_football_playoffs) > 0 THEN 1 ELSE 0 END AS college_football_playoffs
                 ,CASE WHEN SUM(cultural_holiday) > 0 THEN 1 ELSE 0 END AS cultural_holiday
                 ,CASE WHEN SUM(federal_holiday) > 0 THEN 1 ELSE 0 END AS federal_holiday
                 ,CASE WHEN SUM(four_twenty) > 0 THEN 1 ELSE 0 END AS four_twenty
                 ,cast(avg(cast(game_of_thrones_float as float)) as float) as game_of_thrones_float
                 ,cast(avg(cast(march_madness_float as float)) as float) as march_madness_float 
                 ,cast(avg(cast(nfl_game_float as float)) as float) as nfl_game_float
                 ,cast(avg(cast(payday_calendar_float as float)) as float) as payday_calendar_float
                 ,CASE WHEN SUM(superbowl) > 0 THEN 1 ELSE 0 END AS superbowl
                 ,cast(avg(cast(UFC_fight_float as float)) as float) as UFC_fight_float 
                 ,CASE WHEN SUM(christmas) > 0 THEN 1 ELSE 0 END AS christmas
                 ,CASE WHEN SUM(easter) > 0 THEN 1 ELSE 0 END AS easter
                 ,CASE WHEN SUM(thanksgiving) > 0 THEN 1 ELSE 0 END AS thanksgiving                    
                 
                 FROM
                 storeWeek as s
                 
                 GROUP BY
                 week
                 ;')

nationWeek<-left_join(x = nationWeek, y = allStoreCounts, by = c("week"))
mediaVars<- c("OLOSocialSpend", "OLOSearchSpend" , "OLODisplaySpend", "OLVSocialSpend", "OLVNationalSpend", "DigitalSpend", "tvspend", "AdSpend",
              "localsponsorsSpend","localsocialSpend","localdigitalSpend","localradioSpend","localoohSpend","localotherSpend","localtvSpend","localmobileSpend", "localAdSpend")

##truncate extreme media spend

for (med in mediaVars){
  eval(parse(text = paste0("nationWeek$",med,"Truncated<-nationWeek$",med)))
  eval(parse(text = paste0("nationWeek$",med,"[nationWeek$",med," > mean(nationWeek$",med,") + 2 * sd(nationWeek$",med,")]<-mean(nationWeek$",med,") + 2 * sd(nationWeek$",med,")")))
}


mediaVars<- c("OLOSocialSpend", "OLOSearchSpend" , "OLODisplaySpend", "OLVSocialSpend", "OLVNationalSpend", "DigitalSpend", "tvspend", "AdSpend",
              "localsponsorsSpend","localsocialSpend","localdigitalSpend","localradioSpend","localoohSpend","localotherSpend","localtvSpend","localmobileSpend", "localAdSpend",
              "OLOSocialSpendTruncated", "OLOSearchSpendTruncated" , "OLODisplaySpendTruncated", "OLVSocialSpendTruncated", "OLVNationalSpendTruncated", "DigitalSpendTruncated", "tvspendTruncated", "AdSpendTruncated",
              "localsponsorsSpendTruncated","localsocialSpendTruncated","localdigitalSpendTruncated","localradioSpendTruncated","localoohSpendTruncated","localotherSpendTruncated","localtvSpendTruncated","localmobileSpendTruncated", "localAdSpendTruncated")
for (med in mediaVars){
  for (i in 1:8){
    text<-paste0("nationWeek<- nationWeek %>%
                 mutate(",med, "lag", i, " = dplyr::lag(", med, ", n = ", i, ", default = NA))")
    eval(parse(text = text))
  }
}

for (med in mediaVars){
  text<-paste0("nationWeek$", med, "Discounted<- nationWeek$", med, "+
               nationWeek$", med,"lag1 * 0.75+ 
               nationWeek$", med,"lag2 * 0.5+ 
               nationWeek$", med,"lag3 * 0.25 + 
               nationWeek$", med,"lag4 * 0.1 + 
               nationWeek$", med,"lag5 * 0.1 + 
               nationWeek$", med,"lag6 * 0.05 + 
               nationWeek$", med,"lag7 * 0.05 + 
               nationWeek$", med,"lag8 * 0.05  ")
  print(text)             
  eval(parse(text = text))
  
}

nationWeek$avgWeekSales<-nationWeek$TotalItemSales/nationWeek$StoreCount

###OSAT and pricing indices
nationWeek$OverallSatisfaction[is.na(nationWeek$OverallSatisfaction)]<-mean(nationWeek$OverallSatisfaction, na.rm = TRUE)
nationWeek$DigitalOSAT[is.na(nationWeek$DigitalOSAT)]<-mean(nationWeek$DigitalOSAT, na.rm = TRUE)
priceVars<-names(nationWeek)[grepl("price", names(nationWeek))]

for (var in priceVars){
  eval(parse(text = paste0("nationWeek$", var, "[is.na(nationWeek$", var, ")]<-mean(nationWeek$", var, ", na.rm = TRUE)")))
}

nationWeek$totalPrice<-rowSums(nationWeek[ , priceVars])

#*HT* DMA_NAME <- sqldf('SELECT
#                         DMA_NAME, week, count(DMA_NAME) as counts
#                           FROM
#                           storeWeek 
#                         GROUP BY
#                         DMA_NAME, week;')


nationWeek<-nationWeek %>%
  group_by(DMA_NAME, week) %>%
  mutate(DMAavgOSAT= mean(OverallSatisfaction), DMAavgDigitalOSAT= mean(DigitalOSAT),  DMAavgPrice = mean(totalPrice))

nationWeek<-nationWeek %>%
  group_by(DMA_NAME) %>%
  mutate(DMAOverallAvgPrice = mean(totalPrice))

nationWeek$DMAOSATIndex<-nationWeek$OverallSatisfaction/nationWeek$DMAavgOSAT * 100
nationWeek$DMADigitalOSATIndex<-nationWeek$DigitalOSAT/nationWeek$DMAavgDigitalOSAT * 100
nationWeek$DMAPriceIndex<-nationWeek$totalPrice/nationWeek$DMAavgPrice * 100
nationWeek$DMAavgPriceIndex<-nationWeek$DMAavgPrice/nationWeek$DMAOverallAvgPrice * 100 

nationWeek$DMAOSATIndexCentered<-nationWeek$OverallSatisfaction/nationWeek$DMAavgOSAT * 100 - 100

nationWeek$DMAPriceIndexCentered<-nationWeek$totalPrice/nationWeek$DMAavgPrice * 100 - 100
nationWeek$DMAavgPriceIndexCentered<-nationWeek$DMAavgPrice/nationWeek$DMAOverallAvgPrice * 100  - 100

nationWeek$DeliverySales[is.na(nationWeek$DeliverySales)]<-0
nationWeek$InternalDeliveryAvailable[is.na(nationWeek$InternalDeliveryAvailable)]<-0
nationWeek$DDDeliveryAvailable[is.na(nationWeek$DDDeliveryAvailable)]<-0
nationWeek$DoorDashDeliveryAvailable[is.na(nationWeek$DoorDashDeliveryAvailable)]<-0
nationWeek$AnyDeliveryAvailable[is.na(nationWeek$AnyDeliveryAvailable)]<-0

nationWeek$avgTransactionSales<-nationWeek$TotalItemSales/nationWeek$TransactionCount

nationWeek$avgSales<-nationWeek$TotalItemSales/nationWeek$StoreCount
nationWeek$avgTransactions<-nationWeek$TransactionCount/nationWeek$StoreCount

nationWeek<-left_join(x = nationWeek, y = trendAndRemainderData, by = c("week"))
nationWeek<-left_join(x = nationWeek, y = trendOnlyData, by = c("week"))

Salesfit<-lm((nationWeek$ trendAndRemainder)~
               nationWeek$AUSTIN
             +nationWeek$CHICAGO
             +nationWeek$DALLASFTWORTH
             +nationWeek$DENVER
             +nationWeek$HOUSTON
             +nationWeek$LASVEGAS
             +nationWeek$LOSANGELES
             +nationWeek$MIAMIFTLAUDERDALE
             +nationWeek$PHOENIXPRESCOTT
             +nationWeek$SACRAMNTOSTKTONMODESTO
             +nationWeek$SANANTONIO
             +nationWeek$SANDIEGO
             +nationWeek$SANFRANCISCOOAKSANJOSE
             +nationWeek$NationalStores
             +nationWeek$pctNewStores
             +nationWeek$InternalDeliveryAvailable
             +nationWeek$DDDeliveryAvailable
             +nationWeek$DoorDashDeliveryAvailable
             +nationWeek$DigitalOSAT_float
             +nationWeek$wingstops_within_3_miles
             +nationWeek$Buffalo_Wild_Wings_within_3_miles
             +nationWeek$Chick_Fil_A_within_3_miles
             +nationWeek$KFC_within_3_miles
             +nationWeek$Chipotle_within_3_miles
             +nationWeek$Pizza_Hut_within_3_miles
             +nationWeek$Panda_Express_within_3_miles
             +nationWeek$Pizza_Hut_within_3_miles
             +nationWeek$Taco_Bell_within_3_miles
             +nationWeek$Whataburger_within_3_miles
             +nationWeek$Dominos_within_3_miles +
             
            
             
               # nationWeek$unemploymentRate+
               # nationWeek$hotDay_float+
               # nationWeek$coldDay_float+
               # nationWeek$spring_float+
               # nationWeek$fall_float+
               # nationWeek$winter_float+
               # nationWeek$college_football_game_float+
               # nationWeek$college_football_playoffs+
               # nationWeek$cultural_holiday+
               # nationWeek$federal_holiday+
               # nationWeek$four_twenty+
               # nationWeek$march_madness_float+
               # nationWeek$nfl_game_float+
               # nationWeek$payday_calendar_float+
               # nationWeek$superbowl+
               # nationWeek$UFC_fight_float+
               # nationWeek$christmas+
               # nationWeek$easter+
               # nationWeek$thanksgiving+
               # 
               nationWeek$OLOSocialSpendTruncated + 
               nationWeek$OLOSocialSpendTruncatedlag1 + 
               nationWeek$OLOSearchSpendTruncated +
               nationWeek$OLOSearchSpendTruncatedlag1 +
               nationWeek$OLODisplaySpendTruncated +
               nationWeek$OLODisplaySpendTruncatedlag1 +
               nationWeek$OLVSocialSpendTruncatedDiscounted +
               nationWeek$OLVNationalSpendTruncatedDiscounted +
               nationWeek$tvspendTruncatedDiscounted +
               
              nationWeek$localoohSpendDiscounted +
               nationWeek$localsponsorsSpendDiscounted +
               nationWeek$localsocialSpendDiscounted +
               nationWeek$localdigitalSpendDiscounted +
               nationWeek$localradioSpendDiscounted +
               nationWeek$localotherSpendDiscounted +
               nationWeek$localtvSpendDiscounted +
               nationWeek$localmobileSpendDiscounted 
)

summary(Salesfit)



Transactionsfit<-lm((nationWeek$avgTransactions)~
                     
                      #nationWeek$DMAstorecount+
                      #nationWeek$app1+
                      #nationWeek$app2+
                      #nationWeek$app3+
                      #nationWeek$app4+
                      #nationWeek$app5+
                      #nationWeek$web1+
                      #nationWeek$web2+
                      #nationWeek$web3+
                      #nationWeek$web4+
                      #nationWeek$web5+
                    #nationWeek$promo1+
                    #nationWeek$promo2+
                    #nationWeek$promo3+
                    #nationWeek$promo4+
                    #nationWeek$promo5+
                    #nationWeek$braziliancitruspepper+
                    #nationWeek$spicykoreanq+
                   
                      nationWeek$raining_float+
                      nationWeek$snowing_float+
                     
                      nationWeek$unemploymentRate+
                      nationWeek$hotDay_float+
                      nationWeek$coldDay_float+
                      nationWeek$spring_float+
                      nationWeek$fall_float+
                      nationWeek$winter_float+
                      nationWeek$college_football_game_float+
                      nationWeek$college_football_playoffs+
                      nationWeek$cultural_holiday+
                      nationWeek$federal_holiday+
                      nationWeek$four_twenty+
                      nationWeek$march_madness_float+
                      nationWeek$nfl_game_float+
                      nationWeek$payday_calendar_float+
                      nationWeek$superbowl+
                      nationWeek$UFC_fight_float+
                      nationWeek$christmas+
                      nationWeek$easter+
                      nationWeek$thanksgiving+
                      
                      
                      nationWeek$OLOSocialSpendTruncated + 
                      nationWeek$OLOSocialSpendTruncatedlag1 + 
                      nationWeek$OLOSearchSpendTruncated +
                      nationWeek$OLOSearchSpendTruncatedlag1 +
                      nationWeek$OLODisplaySpendTruncated +
                      nationWeek$OLODisplaySpendTruncatedlag1 +
                      nationWeek$OLVSocialSpendTruncatedDiscounted +
                      nationWeek$OLVNationalSpendTruncatedDiscounted +
                      nationWeek$tvspendTruncatedDiscounted +
                      
                      nationWeek$localoohSpendDiscounted +
                      nationWeek$localsponsorsSpendDiscounted +
                      nationWeek$localsocialSpendDiscounted +
                      nationWeek$localdigitalSpendDiscounted +
                      nationWeek$localradioSpendDiscounted +
                      nationWeek$localotherSpendDiscounted +
                      nationWeek$localtvSpendDiscounted +
                      nationWeek$localmobileSpendDiscounted +
                      
                      # 
                      # nationWeek$OLOSocialSpend + 
                      # nationWeek$OLOSocialSpendlag1 + 
                      # nationWeek$OLOSearchSpendTruncated +
                      # nationWeek$OLOSearchSpendlag1Truncated +
                      # nationWeek$OLODisplaySpend +
                      # nationWeek$OLODisplaySpendlag1 +
                      # nationWeek$OLVSocialSpendDiscounted +
                      # nationWeek$OLVNationalSpendDiscounted +
                      # nationWeek$tvspendDiscounted +
                    # 
                    # nationWeek$localoohSpendDiscounted +
                    # nationWeek$localsponsorsSpendDiscounted +
                    # nationWeek$localsocialSpendDiscounted +
                    # nationWeek$localdigitalSpendDiscounted +
                    # nationWeek$localradioSpendDiscounted +
                    # nationWeek$localotherSpendDiscounted +
                    # nationWeek$localtvSpendDiscounted +
                    # nationWeek$localmobileSpendDiscounted +
                    
                    nationWeek$DoorDashDeliveryAvailable_float +
                    nationWeek$DDDeliveryAvailable_float +
                      
                      nationWeek$InternalDeliveryAvailable_float
)

summary(Transactionsfit)

saveRDS(nationWeek, "nation week data.RDS")



##waterfalls
First52Weeks<-nationWeek[7 < nationWeek$week & nationWeek$week < 60 & !is.na(nationWeek$AdSpendDiscounted), ]
Last52Weeks<-nationWeek[nationWeek$week > max(nationWeek$week) - 52 & !is.na(nationWeek$AdSpendDiscounted), ]

JanMay2018<-nationWeek[51 < nationWeek$week & nationWeek$week < 72 & !is.na(nationWeek$AdSpendDiscounted), ]
JanMay2019<-nationWeek[104 < nationWeek$week & nationWeek$week < 125 & !is.na(nationWeek$AdSpendDiscounted), ]
JanMay2019$InternalDeliveryAvailable[is.na(JanMay2019$InternalDeliveryAvailable)]<-0
WaterfallOutput<-NULL
SalesCoefs<-coef(Salesfit)
for (var in 1:length(names(SalesCoefs))){
  if (var == 1){
    outValueFirst52 <- SalesCoefs[1] * nrow(First52Weeks)
    OutValueLast52 <- SalesCoefs[1] * nrow(Last52Weeks)
    OutValueJanMay2018 <- SalesCoefs[1] * nrow(JanMay2018)
    OutValueJanMay2019 <- SalesCoefs[1] * nrow(JanMay2019)
    WaterfallOutput<-rbind(WaterfallOutput, c("Intercept", SalesCoefs[1], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019))
    
  } else{
    thisVar<-gsub("nationWeek\\$","",  names(SalesCoefs)[var])
    theseVars<-str_split_fixed(thisVar, ":", 2)
    if (theseVars[ , 2] == ""){
      outValueFirst52 <- SalesCoefs[var] * sum(First52Weeks[ , thisVar])
      OutValueLast52 <- SalesCoefs[var] * sum(Last52Weeks[ , thisVar])
      OutValueJanMay2018 <- SalesCoefs[var] * sum(JanMay2018[ , thisVar])
      OutValueJanMay2019 <- SalesCoefs[var] * sum(JanMay2019[ , thisVar])
    } else {
      outValueFirst52 <- SalesCoefs[var] * sum(First52Weeks[ , theseVars[ , 1]] * First52Weeks[ , theseVars[ , 2]])
      OutValueLast52 <- SalesCoefs[var] * sum(Last52Weeks[ , theseVars[ , 1]] * Last52Weeks[ , theseVars[ , 2]])
      OutValueJanMay2018 <- SalesCoefs[var] * sum(JanMay2018[ , theseVars[ , 1]] * JanMay2018[ , theseVars[ , 2]])
      OutValueJanMay2019 <- SalesCoefs[var] * sum(JanMay2019[ , theseVars[ , 1]] * JanMay2019[ , theseVars[ , 2]])
    }
    
    
    WaterfallOutput<-rbind(WaterfallOutput, c(thisVar, SalesCoefs[var], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019))
  }
}
write.csv(WaterfallOutput, "national sales waterfall output avg.csv")

WaterfallOutput<-NULL
TransactionsCoefs<-coef(Transactionsfit)
for (var in 1:length(names(TransactionsCoefs))){
  if (var == 1){
    outValueFirst52 <- TransactionsCoefs[1] * nrow(First52Weeks)
    OutValueLast52 <- TransactionsCoefs[1] * nrow(Last52Weeks)
    OutValueJanMay2018 <- TransactionsCoefs[1] * nrow(JanMay2018)
    OutValueJanMay2019 <- TransactionsCoefs[1] * nrow(JanMay2019)
    WaterfallOutput<-rbind(WaterfallOutput, c("Intercept", TransactionsCoefs[1], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019))
    
  } else{
    thisVar<-gsub("nationWeek\\$","",  names(TransactionsCoefs)[var])
    theseVars<-str_split_fixed(thisVar, ":", 2)
    if (theseVars[ , 2] == ""){
      outValueFirst52 <- TransactionsCoefs[var] * sum(First52Weeks[ , thisVar])
      OutValueLast52 <- TransactionsCoefs[var] * sum(Last52Weeks[ , thisVar])
      OutValueJanMay2018 <- TransactionsCoefs[var] * sum(JanMay2018[ , thisVar])
      OutValueJanMay2019 <- TransactionsCoefs[var] * sum(JanMay2019[ , thisVar])
    } else {
      outValueFirst52 <- TransactionsCoefs[var] * sum(First52Weeks[ , theseVars[ , 1]] * First52Weeks[ , theseVars[ , 2]])
      OutValueLast52 <- TransactionsCoefs[var] * sum(Last52Weeks[ , theseVars[ , 1]] * Last52Weeks[ , theseVars[ , 2]])
      OutValueJanMay2018 <- TransactionsCoefs[var] * sum(JanMay2018[ , theseVars[ , 1]] * JanMay2018[ , theseVars[ , 2]])
      OutValueJanMay2019 <- TransactionsCoefs[var] * sum(JanMay2019[ , theseVars[ , 1]] * JanMay2019[ , theseVars[ , 2]])
    }
    
    
    WaterfallOutput<-rbind(WaterfallOutput, c(thisVar, TransactionsCoefs[var], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019))
  }
}
write.csv(WaterfallOutput, "national transactions waterfall output avg.csv")

###############################
## ** HT ** ##
# Changed nationWeek to dmaWeek
###############################

########dma level model
dmaWeek <-sqldf('
                 SELECT
                  DMA_NAME
                 , week
                  ,max(year) as year
                  , count(distinct StoreNo) as DMAStoreCount
                  ,sum(newStore30weeks)/count(distinct StoreNo) as pctNewStores
                  
                  ,min(WeekOf) as WeekOf
                  
                  , avg(braziliancitruspepper) as braziliancitruspepper
                  , avg(spicykoreanq) as spicykoreanq
                  
                  , avg(app1) as app1
                  , avg(app2) as app2
                  , avg(app3) as app3
                  , avg(app4) as app4
                  , avg(app5) as app5
                  
                  , avg(web1) as web1
                  , avg(web2) as web2
                  , avg(web3) as web3
                  , avg(web4) as web4
                  , avg(web5) as web5
                  
                  , avg(promo1) as promo1
                  , avg(promo2) as promo2
                  , avg(promo3) as promo3
                  , avg(promo4) as promo4
                  , avg(promo5) as promo5
                  
                  ,sum(TotalItemSales) as TotalItemSales
                  
                  
                  ,sum(TransactionCount) as TransactionCount
                  
                  ,sum(OLOSocialSpend) as OLOSocialSpend
                  ,sum(OLOSearchSpend) as OLOSearchSpend
                  ,sum(OLODisplaySpend) as OLODisplaySpend
                  ,sum(OLVNationalSpend) as OLVNationalSpend
                  ,sum(OLVSocialSpend) as OLVSocialSpend
                  ,sum(DigitalSpend) as DigitalSpend
                  ,sum(tvspend) as tvspend
                  ,sum(AdSpend) as AdSpend
                  
                  ,sum(localsponsorsSpend) as localsponsorsSpend
                  ,sum(localsocialSpend) as localsocialSpend
                  ,sum(localdigitalSpend) as localdigitalSpend
                  ,sum(localradioSpend) as localradioSpend
                  ,sum(localoohSpend) as localoohSpend
                  ,sum(localotherSpend) as localotherSpend
                  ,sum(localtvSpend) as localtvSpend
                  ,sum(localmobileSpend) as localmobileSpend
                  ,sum(localsponsorsSpend)
                  +sum(localsocialSpend)
                  +sum(localdigitalSpend)
                  +sum(localradioSpend)
                  +sum(localoohSpend)
                  +sum(localotherSpend)
                  +sum(localtvSpend)
                  +sum(localmobileSpend) as localAdSpend
                  
                  
                  ,cast(avg(cast(raining_float as float)) as float) as raining_float
                  ,cast(avg(cast(snowing_float as float)) as float) as snowing_float
                  
                  ,avg(unemploymentRate) as unemploymentRate
                  ,cast(avg(cast(InternalDeliveryAvailable as float)) as float) as InternalDeliveryAvailable_float
                  ,cast(avg(cast(DDDeliveryAvailable as float)) as float) as DDDeliveryAvailable_float
                  ,cast(avg(cast(DoorDashDeliveryAvailable as float)) as float) as DoorDashDeliveryAvailable_float
                  
                  ,sum(case when InternalDeliveryAvailable > 0 then 1 else 0 end) as InternalDeliveryAvailable
                  ,sum(case when DDDeliveryAvailable > 0 then 1 else 0 end) as DDDeliveryAvailable
                  ,sum(case when DoorDashDeliveryAvailable > 0 then 1 else 0 end) as DoorDashDeliveryAvailable
                  
                  
                  ,max(AnyDeliveryAvailable) as AnyDeliveryAvailable
                  ,avg(price10BonelessCombor) as price10BonelessCombor
                  ,avg(price10ClassicComboj) as price10ClassicComboj
                  ,avg(price10pcBonelessr) as price10pcBonelessr
                  ,avg(price10pcClassicj) as price10pcClassicj
                  ,avg(price10SplitComboj) as price10SplitComboj
                  ,avg(price12pcClassicj) as price12pcClassicj
                  ,avg(price15BonelessCombor) as price15BonelessCombor
                  ,avg(price15ClassicComboj) as price15ClassicComboj
                  ,avg(price15pcBonelessr) as price15pcBonelessr
                  ,avg(price15pcClassicj) as price15pcClassicj
                  ,avg(price15SplitComboj) as price15SplitComboj
                  ,avg(price2pcCrispyTenders) as price2pcCrispyTenders
                  ,avg(price20OzBeverage) as price20OzBeverage
                  ,avg(price20pcBonelessr) as price20pcBonelessr
                  ,avg(price20pcClassicj) as price20pcClassicj
                  ,avg(price24pcFamilyBonelessr) as price24pcFamilyBonelessr
                  ,avg(price24pcFamilyClassicj) as price24pcFamilyClassicj
                  ,avg(price24pcFamilySplitj) as price24pcFamilySplitj
                  ,avg(price25BWBigNiteinBundle) as price25BWBigNiteinBundle
                  ,avg(price3CrispyTendersCombo) as price3CrispyTendersCombo
                  ,avg(price3pcBonelessr) as price3pcBonelessr
                  ,avg(price3pcClassicj) as price3pcClassicj
                  ,avg(price30pcClassicj) as price30pcClassicj
                  ,avg(price30pcFamilyBonelessr) as price30pcFamilyBonelessr
                  ,avg(price30pcFamilyClassicj) as price30pcFamilyClassicj
                  ,avg(price30pcFamilySplitj) as price30pcFamilySplitj
                  ,avg(price32OzBeverage) as price32OzBeverage
                  ,avg(price4pcCrispyTenders) as price4pcCrispyTenders
                  ,avg(price40pcFamilyClassicj) as price40pcFamilyClassicj
                  ,avg(price40pcFamilySplitj) as price40pcFamilySplitj
                  ,avg(price5CrispyTendersCombo) as price5CrispyTendersCombo
                  ,avg(price5pcBonelessr) as price5pcBonelessr
                  ,avg(price5pcClassicj) as price5pcClassicj
                  ,avg(price50pcFamilyClassicj) as price50pcFamilyClassicj
                  ,avg(price6BonelessCombor) as price6BonelessCombor
                  ,avg(price6ClassicComboj) as price6ClassicComboj
                  ,avg(price6SplitComboj) as price6SplitComboj
                  ,avg(price8BonelessCombor) as price8BonelessCombor
                  ,avg(price8ClassicComboj) as price8ClassicComboj
                  ,avg(price8pcBonelessr) as price8pcBonelessr
                  ,avg(price8pcClassicj) as price8pcClassicj
                  ,avg(price8SplitComboj) as price8SplitComboj
                  ,avg(priceAdd5BonelessWingsr) as priceAdd5BonelessWingsr
                  ,avg(priceAdd5ClassicWingsJ) as priceAdd5ClassicWingsJ
                  ,avg(priceBleuCheeseRegular) as priceBleuCheeseRegular
                  ,avg(priceBuffaloFF) as priceBuffaloFF
                  ,avg(priceBWLunchCombor) as priceBWLunchCombor
                  ,avg(priceCarrotSticks) as priceCarrotSticks
                  ,avg(priceCelerySticks) as priceCelerySticks
                  ,avg(priceCheeseFriesRegular) as priceCheeseFriesRegular
                  ,avg(priceCheeseSauceMedium) as priceCheeseSauceMedium
                  ,avg(priceCheeseSauceRegular) as priceCheeseSauceRegular
                  ,avg(priceCLLunchComboj) as priceCLLunchComboj
                  ,avg(priceFrenchFriesLarge) as priceFrenchFriesLarge
                  ,avg(priceFrenchFriesRegular) as priceFrenchFriesRegular
                  ,avg(priceFrenchFriesRegulare) as priceFrenchFriesRegulare
                  ,avg(priceFriedCornLarge) as priceFriedCornLarge
                  ,avg(priceFriedCornRegular) as priceFriedCornRegular
                  ,avg(priceHoneyMustardRegular) as priceHoneyMustardRegular
                  ,avg(priceRanchLarge) as priceRanchLarge
                  ,avg(priceRanchRegular) as priceRanchRegular
                  ,avg(priceRollEach) as priceRollEach
                  ,avg(priceRollsHalfDozen) as priceRollsHalfDozen
                  ,avg(priceSauce) as priceSauce
                  ,avg(priceTripleChocolateBrownie) as priceTripleChocolateBrownie
                  ,avg(priceUpgradeDrink) as priceUpgradeDrink
                  ,avg(priceVeggieSticks) as priceVeggieSticks
                  ,avg(priceVoodooFriesLarge) as priceVoodooFriesLarge
                  ,avg(priceVoodooFriesRegular) as priceVoodooFriesRegular
                  ,avg(householdsPerStore) as householdsPerStore
                  ,cast(avg(cast(hotDay_float as float)) as float) as hotDay_float
                  ,cast(avg(cast(coldDay_float as float)) as float) as coldDay_float
                  ,cast(avg(cast(spring_float as float)) as float) as spring_float
                  ,cast(avg(cast(summer_float as float)) as float) as summer_float
                  ,cast(avg(cast(fall_float as float)) as float) as fall_float
                  ,cast(avg(cast(winter_float as float)) as float) as winter_float
                  ,cast(avg(cast(DigitalOSAT as float)) as float) as DigitalOSAT_float
                  ,sum(case when wingstops_within_3_miles > 0 then 1 else 0 end) as wingstops_within_3_miles
                  ,sum(case when Buffalo_Wild_Wings_within_3_miles > 0 then 1 else 0 end) as Buffalo_Wild_Wings_within_3_miles
                  ,sum(case when Chick_Fil_A_within_3_miles > 0 then 1 else 0 end) as Chick_Fil_A_within_3_miles
                  ,sum(case when KFC_within_3_miles > 0 then 1 else 0 end) as KFC_within_3_miles
                  ,sum(case when Chipotle_within_3_miles > 0 then 1 else 0 end) as Chipotle_within_3_miles
                  ,sum(case when Pizza_Hut_within_3_miles > 0 then 1 else 0 end) as Pizza_Hut_within_3_miles
                  ,sum(case when Panda_Express_within_3_miles > 0 then 1 else 0 end) as Panda_Express_within_3_miles
                  ,sum(case when Pizza_Hut_within_3_miles > 0 then 1 else 0 end) as Pizza_Hut_within_3_miles
                  ,sum(case when Taco_Bell_within_3_miles > 0 then 1 else 0 end) as Taco_Bell_within_3_miles
                  ,sum(case when Whataburger_within_3_miles > 0 then 1 else 0 end) as Whataburger_within_3_miles
                  ,sum(case when Dominos_within_3_miles > 0 then 1 else 0 end) as Dominos_within_3_miles
                  
                  
                  ,cast(avg(cast(college_football_game_float as float)) as float) as college_football_game_float
                  ,CASE WHEN SUM(college_football_playoffs) > 0 THEN 1 ELSE 0 END AS college_football_playoffs
                  ,CASE WHEN SUM(cultural_holiday) > 0 THEN 1 ELSE 0 END AS cultural_holiday
                  ,CASE WHEN SUM(federal_holiday) > 0 THEN 1 ELSE 0 END AS federal_holiday
                  ,CASE WHEN SUM(four_twenty) > 0 THEN 1 ELSE 0 END AS four_twenty
                  ,cast(avg(cast(game_of_thrones_float as float)) as float) as game_of_thrones_float
                  ,cast(avg(cast(march_madness_float as float)) as float) as march_madness_float 
                  ,cast(avg(cast(nfl_game_float as float)) as float) as nfl_game_float
                  ,cast(avg(cast(payday_calendar_float as float)) as float) as payday_calendar_float
                  ,CASE WHEN SUM(superbowl) > 0 THEN 1 ELSE 0 END AS superbowl
                  ,cast(avg(cast(UFC_fight_float as float)) as float) as UFC_fight_float 
                  ,CASE WHEN SUM(christmas) > 0 THEN 1 ELSE 0 END AS christmas
                  ,CASE WHEN SUM(easter) > 0 THEN 1 ELSE 0 END AS easter
                  ,CASE WHEN SUM(thanksgiving) > 0 THEN 1 ELSE 0 END AS thanksgiving                    
                  
                  FROM
                  storeWeek as s
                  
                  GROUP BY
                  DMA_NAME,
                  week
                  ;')

#dmaWeek<-left_join(x = dmaWeek, y = allStoreCounts, by = c("week"))

mediaVars<- c("OLOSocialSpend", "OLOSearchSpend" , "OLODisplaySpend", "OLVSocialSpend", "OLVNationalSpend", "DigitalSpend", "tvspend", "AdSpend",
              "localsponsorsSpend","localsocialSpend","localdigitalSpend","localradioSpend","localoohSpend","localotherSpend","localtvSpend","localmobileSpend", "localAdSpend")

##truncate extreme media spend

for (med in mediaVars){
  eval(parse(text = paste0("dmaWeek$",med,"Truncated<-dmaWeek$",med)))
  eval(parse(text = paste0("dmaWeek$",med,"[dmaWeek$",med," > mean(dmaWeek$",med,") + 2 * sd(dmaWeek$",med,")]<-mean(dmaWeek$",med,") + 2 * sd(dmaWeek$",med,")")))
}


mediaVars<- c("OLOSocialSpend", "OLOSearchSpend" , "OLODisplaySpend", "OLVSocialSpend", "OLVNationalSpend", "DigitalSpend", "tvspend", "AdSpend",
              "localsponsorsSpend","localsocialSpend","localdigitalSpend","localradioSpend","localoohSpend","localotherSpend","localtvSpend","localmobileSpend", "localAdSpend",
              "OLOSocialSpendTruncated", "OLOSearchSpendTruncated" , "OLODisplaySpendTruncated", "OLVSocialSpendTruncated", "OLVNationalSpendTruncated", "DigitalSpendTruncated", "tvspendTruncated", "AdSpendTruncated",
              "localsponsorsSpendTruncated","localsocialSpendTruncated","localdigitalSpendTruncated","localradioSpendTruncated","localoohSpendTruncated","localotherSpendTruncated","localtvSpendTruncated","localmobileSpendTruncated", "localAdSpendTruncated")

for (med in mediaVars){
  for (i in 1:8){
    text<-paste0("dmaWeek<- dmaWeek %>%
                 mutate(",med, "lag", i, " = dplyr::lag(", med, ", n = ", i, ", default = NA))")
    eval(parse(text = text))
  }
}

for (med in mediaVars){
  text<-paste0("dmaWeek$", med, "Discounted<- dmaWeek$", med, "+
               dmaWeek$", med,"lag1 * 0.75+ 
               dmaWeek$", med,"lag2 * 0.5+ 
               dmaWeek$", med,"lag3 * 0.25 + 
               dmaWeek$", med,"lag4 * 0.1 + 
               dmaWeek$", med,"lag5 * 0.1 + 
               dmaWeek$", med,"lag6 * 0.05 + 
               dmaWeek$", med,"lag7 * 0.05 + 
               dmaWeek$", med,"lag8 * 0.05  ")
  print(text)             
  eval(parse(text = text))
  
}

dmaWeek$InternalDeliveryAvailable[is.na(dmaWeek$InternalDeliveryAvailable)]<-0
dmaWeek$DDDeliveryAvailable[is.na(dmaWeek$DDDeliveryAvailable)]<-0
dmaWeek$DoorDashDeliveryAvailable[is.na(dmaWeek$DoorDashDeliveryAvailable)]<-0

Salesfit<-lm((dmaWeek$TotalItemSales)~
               dmaWeek$pctNewStores
             +dmaWeek$DMAStoreCount
             +dmaWeek$InternalDeliveryAvailable
             +dmaWeek$DDDeliveryAvailable
             +dmaWeek$DoorDashDeliveryAvailable
             +dmaWeek$DigitalOSAT_float
             +dmaWeek$wingstops_within_3_miles
             +dmaWeek$Buffalo_Wild_Wings_within_3_miles
             +dmaWeek$Chick_Fil_A_within_3_miles
             +dmaWeek$KFC_within_3_miles
             +dmaWeek$Chipotle_within_3_miles
             +dmaWeek$Pizza_Hut_within_3_miles
             +dmaWeek$Panda_Express_within_3_miles
             +dmaWeek$Pizza_Hut_within_3_miles
             +dmaWeek$Taco_Bell_within_3_miles
             +dmaWeek$Whataburger_within_3_miles
             +dmaWeek$Dominos_within_3_miles +
               
               dmaWeek$raining_float+
               dmaWeek$snowing_float+
               
               dmaWeek$unemploymentRate+
               dmaWeek$hotDay_float+
               dmaWeek$coldDay_float+
               dmaWeek$spring_float+
               dmaWeek$fall_float+
               dmaWeek$winter_float+
               dmaWeek$college_football_game_float+
               dmaWeek$college_football_playoffs+
               dmaWeek$cultural_holiday+
               dmaWeek$federal_holiday+
               dmaWeek$four_twenty+
               dmaWeek$march_madness_float+
               dmaWeek$nfl_game_float+
               dmaWeek$payday_calendar_float+
               dmaWeek$superbowl+
               dmaWeek$UFC_fight_float+
               dmaWeek$christmas+
               dmaWeek$easter+
               dmaWeek$thanksgiving+
               
               dmaWeek$OLOSocialSpendTruncated + 
               dmaWeek$OLOSocialSpendTruncatedlag1 + 
               dmaWeek$OLOSearchSpendTruncated +
               dmaWeek$OLOSearchSpendTruncatedlag1 +
               dmaWeek$OLODisplaySpendTruncated +
               dmaWeek$OLODisplaySpendTruncatedlag1 +
               dmaWeek$OLVSocialSpendTruncatedDiscounted +
               dmaWeek$OLVNationalSpendTruncatedDiscounted +
               dmaWeek$tvspendTruncatedDiscounted +
               
               dmaWeek$localoohSpendDiscounted +
               dmaWeek$localsponsorsSpendDiscounted +
               dmaWeek$localsocialSpendDiscounted +
               dmaWeek$localdigitalSpendDiscounted +
               dmaWeek$localradioSpendDiscounted +
               dmaWeek$localotherSpendDiscounted +
               dmaWeek$localtvSpendDiscounted +
               dmaWeek$localmobileSpendDiscounted 
)

summary(Salesfit)



Transactionsfit<-lm((dmaWeek$avgTransactions)~
                      
                      #dmaWeek$DMAstorecount+
                      #dmaWeek$app1+
                      #dmaWeek$app2+
                      #dmaWeek$app3+
                      #dmaWeek$app4+
                      #dmaWeek$app5+
                      #dmaWeek$web1+
                      #dmaWeek$web2+
                      #dmaWeek$web3+
                      #dmaWeek$web4+
                    #dmaWeek$web5+
                    #dmaWeek$promo1+
                    #dmaWeek$promo2+
                    #dmaWeek$promo3+
                    #dmaWeek$promo4+
                    #dmaWeek$promo5+
                    #dmaWeek$braziliancitruspepper+
                    #dmaWeek$spicykoreanq+
                    
                    dmaWeek$raining_float+
                      dmaWeek$snowing_float+
                      
                      dmaWeek$unemploymentRate+
                      dmaWeek$hotDay_float+
                      dmaWeek$coldDay_float+
                      dmaWeek$spring_float+
                      dmaWeek$fall_float+
                      dmaWeek$winter_float+
                      dmaWeek$college_football_game_float+
                      dmaWeek$college_football_playoffs+
                      dmaWeek$cultural_holiday+
                      dmaWeek$federal_holiday+
                      dmaWeek$four_twenty+
                      dmaWeek$march_madness_float+
                      dmaWeek$nfl_game_float+
                      dmaWeek$payday_calendar_float+
                      dmaWeek$superbowl+
                      dmaWeek$UFC_fight_float+
                      dmaWeek$christmas+
                      dmaWeek$easter+
                      dmaWeek$thanksgiving+
                      
                      
                      dmaWeek$OLOSocialSpendTruncated + 
                      dmaWeek$OLOSocialSpendTruncatedlag1 + 
                      dmaWeek$OLOSearchSpendTruncated +
                      dmaWeek$OLOSearchSpendTruncatedlag1 +
                      dmaWeek$OLODisplaySpendTruncated +
                      dmaWeek$OLODisplaySpendTruncatedlag1 +
                      dmaWeek$OLVSocialSpendTruncatedDiscounted +
                      dmaWeek$OLVNationalSpendTruncatedDiscounted +
                      dmaWeek$tvspendTruncatedDiscounted +
                      
                      dmaWeek$localoohSpendDiscounted +
                      dmaWeek$localsponsorsSpendDiscounted +
                      dmaWeek$localsocialSpendDiscounted +
                      dmaWeek$localdigitalSpendDiscounted +
                      dmaWeek$localradioSpendDiscounted +
                      dmaWeek$localotherSpendDiscounted +
                      dmaWeek$localtvSpendDiscounted +
                      dmaWeek$localmobileSpendDiscounted +
                      
                      # 
                      # dmaWeek$OLOSocialSpend + 
                      # dmaWeek$OLOSocialSpendlag1 + 
                      # dmaWeek$OLOSearchSpendTruncated +
                      # dmaWeek$OLOSearchSpendlag1Truncated +
                      # dmaWeek$OLODisplaySpend +
                      # dmaWeek$OLODisplaySpendlag1 +
                      # dmaWeek$OLVSocialSpendDiscounted +
                      # dmaWeek$OLVNationalSpendDiscounted +
                      # dmaWeek$tvspendDiscounted +
                    # 
                    # dmaWeek$localoohSpendDiscounted +
                    # dmaWeek$localsponsorsSpendDiscounted +
                    # dmaWeek$localsocialSpendDiscounted +
                    # dmaWeek$localdigitalSpendDiscounted +
                    # dmaWeek$localradioSpendDiscounted +
                    # dmaWeek$localotherSpendDiscounted +
                    # dmaWeek$localtvSpendDiscounted +
                    # dmaWeek$localmobileSpendDiscounted +
                    
                    dmaWeek$DoorDashDeliveryAvailable_float +
                      dmaWeek$DDDeliveryAvailable_float +
                      
                      dmaWeek$InternalDeliveryAvailable_float
)

summary(Transactionsfit)

##unemployment rate over time

unemploymentOverTime<-storeWeek%>%
  group_by(week)%>%
  summarise(rate = mean(unemploymentRate, na.rm=TRUE))

OLOovertime<-storeWeek%>%
  group_by(week)%>%
  summarise(rate = sum(OLOSearchSpend, na.rm=TRUE))

salesovertime<-storeWeek%>%
  group_by(week)%>%
  summarise(avgSales = mean(TotalItemSales, na.rm=TRUE))

delivery<-storeWeek%>%
  group_by(week)%>%
  summarise(delivery = sum(DeliverySales, na.rm=TRUE), dd = sum(DoorDashDeliverySales, na.rm=TRUE))


###how does delivery impact transaction size
transVars<-names(storeWeek)[grepl("Transaction", names(storeWeek))]
salesVars<-names(storeWeek)[grepl("Sales", names(storeWeek))]

storeWeek[ , names(storeWeek) %in% transVars][is.na(storeWeek[ , names(storeWeek) %in% transVars])]<-0
storeWeek[ , names(storeWeek) %in% salesVars][is.na(storeWeek[ , names(storeWeek) %in% salesVars])]<-0
storeWeekTrans<-storeWeek[ , names(storeWeek) %in% c("week",salesVars, transVars)]
transactionSize<-storeWeekTrans%>%
  group_by(week )%>%
  summarise_all(sum)
write.csv(transactionSize, "transaction by order2.csv")

transactions<-c("DDDeliveryTransactions","DeliveryTransactions", "DoorDashDeliveryTransactions","webMobileTransactions", 
                "appTransactions",  "CallToGoTransactions", "CallDineInTransactions", "WalkToGoTransactions", "WalkDineInTransactions")
for (var in transactions){
  eval(parse(text = paste0("storeDay$",var,"[is.na(storeDay$",var,")]<-0")))
}

deliveryList<-unique(storeDay$StoreNo[storeDay$AnyDeliveryAvailable == 1])
deliverystores<-storeDay[storeDay$StoreNo %in% deliveryList, ]
deliverystores$fromDeliveryTime<-as.Date(deliverystores$date) - as.Date(deliverystores$FirstDeliveryDate)
deliverystores$WeeksFromDelivery<-as.numeric(floor(deliverystores$fromDeliveryTime/7))

DeliverySales<-sqldf('
                     SELECT
                     WeeksFromDelivery
                     
                     ,sum(TotalItemSales) as TotalItemSales
                     ,sum(TotalComps)
                     ,sum(TotalPromos)
                     ,sum(TotalNetSales)
                     ,sum(totalDelivery)
                     ,sum(webMobileSales) as webMobileSales
                     ,sum(appSales) as appSales
                     , sum(webMobileSales)/sum(TotalItemSales) as webMobilepct
                     , sum(appSales)/sum(TotalItemSales) as apppct
                     ,sum(WalkToGoSales) as WalkToGoSales
                     ,sum(CallToGoSales) as CallToGoSales
                     ,sum(WalkDineInSales) as WalkDineInSales
                     ,sum(CallDineInSales) as CallDineInSales
                     ,sum(DeliverySales) as DeliverySales
                     ,sum(DDDeliverySales) as DDDeliverySales
                     ,sum(DoorDashDeliverySales) as DoorDashDeliverySales
                     , (sum(DeliverySales) + sum(DDDeliverySales) + sum(DoorDashDeliverySales))/sum(TotalItemSales) as totalDeliverypct
                     ,(sum(DeliverySales))/sum(TotalItemSales) as Deliverypct
                     ,(sum(DDDeliverySales))/sum(TotalItemSales) as DDDeliverypct
                     ,(sum(DoorDashDeliverySales))/sum(TotalItemSales) as DoorDashDeliverypct
                     , sum(CallDineInSales)/sum(TotalItemSales) as CallDineInpct
                     , sum(WalkDineInSales)/sum(TotalItemSales) as WalkDineInpct
                     , sum(CallToGoSales)/sum(TotalItemSales) as CallToGopct
                     , sum(WalkToGoSales)/sum(TotalItemSales) as WalkToGopct
                     
                     , (sum(internalDeliveryTransactions) + sum(externalDeliveryTransactions))/sum(TransactionCount) as totalDeliveryTransactionspct
                     ,(sum(DeliveryTransactions))/sum(TransactionCount) as DeliveryTransactionspct
                     ,(sum(DDDeliveryTransactions))/sum(TransactionCount) as DDDeliveryTransactionspct
                     ,(sum(DoorDashDeliveryTransactions))/sum(TransactionCount) as DoorDashDeliveryTransactionspct
                     , sum(internalDeliveryTransactions)/sum(TransactionCount) as internalDeliveryTransactionspct
                     , sum(externalDeliveryTransactions)/sum(TransactionCount) as externalDeliveryTransactionspct
                     , sum(webMobileTransactions)/sum(TransactionCount) as webMobileTransactionspct
                     , sum(appTransactions)/sum(TransactionCount) as appTransactionspct
                     , sum(CallDineInTransactions)/sum(TransactionCount) as CallDineInTransactionspct
                     , sum(WalkDineInTransactions)/sum(TransactionCount) as WalkDineInTransactionspct
                     , sum(CallToGoTransactions)/sum(TransactionCount) as CallToGoTransactionspct
                     , sum(WalkToGoTransactions)/sum(TransactionCount) as WalkToGoTransactionspct
                     
                     ,avg(OverallSatisfaction)
                     ,avg(DeliveryOSAT)
                     ,avg(DigitalOSAT)
                     ,avg(TasteOfFood)
                     ,avg(AccuracyOfOrder)
                     ,avg(SpeedOfService)
                     ,avg(unemploymentRate)
                     , count(*) as numberOfStores
                     
                     
                     FROM
                     deliverystores as s
                     
                     GROUP BY
                     WeeksFromDelivery
                     ;')

write.csv(DeliverySales, "delivery transactions -- any delivery all stores.csv")

DeliverySales$otherSales<-1 - DeliverySales$webMobileSales - DeliverySales$appSales- DeliverySales$DeliverySales - DeliverySales$DDDeliverySales - DeliverySales$DoorDashDeliverySales - DeliverySales$WalkDineInSales -
  DeliverySales$WalkToGoSales - DeliverySales$CallDineInSales - DeliverySales$CallToGoSales

DeliverySales$otherSalespct<-1 - DeliverySales$webMobileSalespct - DeliverySales$appSalespct - DeliverySales$DeliverySalespct - DeliverySales$DDDeliverySalespct - DeliverySales$DoorDashDeliverySalespct - DeliverySales$WalkDineInSalespct -
  DeliverySales$WalkToGoSalespct - DeliverySales$CallDineInSalespct - DeliverySales$CallToGoSalespct

DeliverySales$otherTransactions<-1 - DeliverySales$webMobileTransactions - DeliverySales$appTransactions- DeliverySales$DeliveryTransactions - DeliverySales$DDDeliveryTransactions - DeliverySales$DoorDashDeliveryTransactions - DeliverySales$WalkDineInTransactions -
  DeliverySales$WalkToGoTransactions - DeliverySales$CallDineInTransactions - DeliverySales$CallToGoTransactions

DeliverySales$otherTransactionspct<-1 - DeliverySales$webMobileTransactionspct - DeliverySales$appTransactionspct - DeliverySales$DeliveryTransactionspct - DeliverySales$DDDeliveryTransactionspct - DeliverySales$DoorDashDeliveryTransactionspct - DeliverySales$WalkDineInTransactionspct -
  DeliverySales$WalkToGoTransactionspct - DeliverySales$CallDineInTransactionspct - DeliverySales$CallToGoTransactionspct

orderModes<-c("other", "webMobile", "Delivery", "DDDelivery", "DoorDashDelivery", "WalkDineIn", "WalkToGo", "CallDineIn", "CallToGo", "app")

###percent increase in prices impact on transactions

storeWeeklog<-storeWeek
spendVars<-names(storeWeeklog)[grepl("Spend|spend", names(storeWeeklog))]
for (var in spendVars){
  eval(parse(text = paste0("storeWeeklog$",var,"[storeWeeklog$",var," == 0]<-1")))
}
storeWeeklog$TransactionCount[storeWeeklog$TransactionCount == 0]<-1

Transactionsfit<-lm(log(storeWeeklog$TransactionCount)~
                      log(storeWeeklog$totalPrice)+
                      storeWeeklog$DMAPriceIndexCentered+
                      storeWeeklog$ageOfStoreInWeeks+
                      storeWeeklog$newStore30weeks + 
                      storeWeeklog$ageOfStoreInWeeks*storeWeeklog$newStore30weeks+
                      storeWeeklog$quintile2 + 
                      storeWeeklog$quintile3 + 
                      storeWeeklog$quintile4 + 
                      storeWeeklog$quintile5 + 
                      #storeWeeklog$DMAstorecount+
                      #storeWeeklog$app1+
                      #storeWeeklog$app2+
                      #storeWeeklog$app3+
                      #storeWeeklog$app4+
                      #storeWeeklog$app5+
                      #storeWeeklog$web1+
                      #storeWeeklog$web2+
                      #storeWeeklog$web3+
                      #storeWeeklog$web4+
                      #storeWeeklog$web5+
                    #storeWeeklog$promo1+
                    #storeWeeklog$promo2+
                    #storeWeeklog$promo3+
                    #storeWeeklog$promo4+
                    #storeWeeklog$promo5+
                    #storeWeeklog$braziliancitruspepper+
                    #storeWeeklog$spicykoreanq+
                    storeWeeklog$samplingEvent+
                      storeWeeklog$raining+
                      storeWeeklog$snowing+
                      storeWeeklog$OverallSatisfaction+
                      storeWeeklog$wingstops_within_3_miles+
                      storeWeeklog$Buffalo_Wild_Wings_within_3_miles+
                      storeWeeklog$Chick_Fil_A_within_3_miles+
                      storeWeeklog$Chipotle_within_3_miles+
                      storeWeeklog$KFC_within_3_miles+
                      storeWeeklog$Panda_Express_within_3_miles+
                      storeWeeklog$Pizza_Hut_within_3_miles+
                      storeWeeklog$Taco_Bell_within_3_miles+
                      storeWeeklog$Whataburger_within_3_miles+
                      storeWeeklog$Dominos_within_3_miles+
                      storeWeeklog$unemploymentRate+
                      storeWeeklog$hotDay+
                      storeWeeklog$coldDay+
                      storeWeeklog$spring+
                      storeWeeklog$fall+
                      storeWeeklog$winter+
                      storeWeeklog$college_football_game+
                      storeWeeklog$college_football_playoffs+
                      storeWeeklog$cultural_holiday+
                      storeWeeklog$federal_holiday+
                      storeWeeklog$four_twenty+
                      storeWeeklog$game_of_thrones+
                      storeWeeklog$march_madness+
                      storeWeeklog$nfl_game+
                      storeWeeklog$payday_calendar+
                      storeWeeklog$superbowl+
                      storeWeeklog$UFC_fight+
                      storeWeeklog$christmas+
                      storeWeeklog$easter+
                      storeWeeklog$thanksgiving+
                      storeWeeklog$marketStatusEmerging+
                      storeWeeklog$marketStatusDeveloping+
                      storeWeeklog$householdsPerStore+
                      log(storeWeeklog$OLOSocialSpend) + 
                      log(storeWeeklog$OLOSocialSpendlag1) + 
                      log(storeWeeklog$OLOSearchSpendTruncated) +
                      log(storeWeeklog$OLOSearchSpendlag1Truncated) +
                      log(storeWeeklog$OLODisplaySpend) +
                      log(storeWeeklog$OLODisplaySpendlag1) +
                      log(storeWeeklog$OLVSocialSpendDiscounted) +
                      log(storeWeeklog$OLVNationalSpendDiscounted) +
                      log(storeWeeklog$tvspendDiscounted) +
                      
                      log(storeWeeklog$localoohSpendDiscounted) +
                      log(storeWeeklog$localsponsorsSpendDiscounted) +
                      log(storeWeeklog$localsocialSpendDiscounted) +
                      log(storeWeeklog$localdigitalSpendDiscounted) +
                      log(storeWeeklog$localradioSpendDiscounted) +
                      log(storeWeeklog$localotherSpendDiscounted) +
                      log(storeWeeklog$localtvSpendDiscounted) +
                      log(storeWeeklog$localmobileSpendDiscounted) +
                      
                      storeWeeklog$DoorDashDeliveryAvailable +
                      storeWeeklog$marketStatusEmerging * storeWeeklog$DoorDashDeliveryAvailable +
                      storeWeeklog$marketStatusDeveloping* storeWeeklog$DoorDashDeliveryAvailable + 
                      
                      storeWeeklog$DDDeliveryAvailable +
                      storeWeeklog$marketStatusEmerging * storeWeeklog$DDDeliveryAvailable +
                      storeWeeklog$marketStatusDeveloping* storeWeeklog$DDDeliveryAvailable + 
                      
                      storeWeeklog$InternalDeliveryAvailable +
                      storeWeeklog$marketStatusEmerging * storeWeeklog$InternalDeliveryAvailable +
                      storeWeeklog$marketStatusDeveloping* storeWeeklog$InternalDeliveryAvailable 
)

summary(Transactionsfit)


###transactions over time
transovertime<-storeWeek%>%
  group_by(week)%>%
  summarise(avgTransactionSize = mean(avgTransactionSales, na.rm=TRUE))

nationalWeekTS <- ts(transovertime$avgTransactionSize, frequency = 52)
stl_national = stl(nationalWeekTS, "periodic")
plot(stl_national)
adSpend<-stl_national$time.series[ , 2]
write.csv(stl_national$time.series, "national avg transaction size trend data.csv")

quint5overtime<-Last52Weeks%>%
  group_by(quintile5, quintile4, quintile3, quintile2)%>%
  summarise(stores = n())


transSize2017<-First52Weeks%>%
  group_by(StoreNo)%>%
  summarise(avgTransactionSize = mean(avgTransactionSales, na.rm=TRUE), weeksOfDelivery= sum(AnyDeliveryAvailable))

transSize2019<-Last52Weeks%>%
  group_by(StoreNo)%>%
  summarise(avgTransactionSize = mean(avgTransactionSales, na.rm=TRUE), weeksOfDelivery= sum(AnyDeliveryAvailable))

changeInTransSize<-inner_join(transSize2017, transSize2019, by = c("StoreNo"))

changeInTransSize$PercentChange<-(changeInTransSize$avgTransactionSize.y - changeInTransSize$avgTransactionSize.x)/changeInTransSize$avgTransactionSize.x * 100
  changeInTransSize$ActualChange<-changeInTransSize$avgTransactionSize.y - changeInTransSize$avgTransactionSize.x
  
  changeInTransSize$PercentChangeDelivery<-(changeInTransSize$weeksOfDelivery.y - changeInTransSize$weeksOfDelivery.x)/changeInTransSize$weeksOfDelivery.x * 100
  changeInTransSize$ActualChangeDelivery<-changeInTransSize$weeksOfDelivery.y - changeInTransSize$weeksOfDelivery.x
  
  
  ggplot(changeInTransSize, aes(x=PercentChange)) + 
    geom_histogram(binwidth=1)

  ggplot(changeInTransSize, aes(x=ActualChange)) + 
    geom_histogram(binwidth=0.25)
  
  ggplot(changeInTransSize, aes(x = PercentChange, y = ActualChangeDelivery)) + geom_point()
  ggplot(changeInTransSize, aes(x = PercentChange, y = ActualChangeDelivery)) + geom_point() + ylim(c(0, 50)) + xlim(c(-10, 40)) + ylab("Change in Number of Delivery Weeks")+ xlab("Percent Change In Transaction Size")

  
  justMarketStatus<- storeWeek[ , c("StoreNo", "marketStatus")]
  justMarketStatus<-justMarketStatus[!duplicated(justMarketStatus$StoreNo), ]
  
  changeInTransSize<-left_join(x = changeInTransSize, y = justMarketStatus, by = c("StoreNo"))
  ggplot(changeInTransSize, aes(x = PercentChange, y = ActualChangeDelivery, colour = marketStatus)) + geom_point() + ylim(c(0, 50)) + xlim(c(-10, 40)) + ylab("Change in Number of Delivery Weeks")+ xlab("Percent Change In Transaction Size")
  
  
  ###delivery cohorts
  
  
  deliveryDates<-sqldf(
    '
select

StoreNo,
min(CASE WHEN COALESCE(AnyDeliveryAvailable,0) > 0 THEN week ELSE NULL END) as FirstDeliveryDate

from
storeWeek

group by
StoreNo
  ;'
  )
  
  deliveryDates$internalDeliveryDate<-as.Date(deliveryDates$internalDeliveryDate)
  deliveryDates$DDDeliveryDate<-as.Date(deliveryDates$DDDeliveryDate)
  deliveryDates$DoorDashDeliveryDate<-as.Date(deliveryDates$DoorDashDeliveryDate)
  deliveryDates<-deliveryDates %>% 
    rowwise() %>%
    mutate(FirstDeliveryDate = pmin(internalDeliveryDate, DDDeliveryDate, DoorDashDeliveryDate, na.rm = TRUE))
  
  
  
  storeWeek<-left_join(x = storeWeek, y = deliveryDates, by = c("StoreNo"))
  storeWeek$deliveryCohort<-"Never"
  storeWeek$deliveryCohort[storeWeek$FirstDeliveryDate %in% c(0:52)]<-"2017 or before"
  storeWeek$deliveryCohort[storeWeek$FirstDeliveryDate %in% c(53:104)]<-"2018"
  storeWeek$deliveryCohort[storeWeek$FirstDeliveryDate %in% c(105:124)]<-"2019"

  storeWeek$yearApprox<-2017
  storeWeek$yearApprox[storeWeek$week %in% c(53:104)]<-2018
  storeWeek$yearApprox[storeWeek$week>104]<-2019
  

  DeliverySales<-sqldf(
    '
select
StoreNo,
week,
#deliveryCohort
#yearApprox
 ,sum(TotalItemSales) as TotalItemSales
,avg(TotalItemSales) as AvgItemSales
,sum(TransactionCount) as TransactionCount
,avg(TransactionCount) as AvgTransactionCount   
 ,sum(TotalItemSales)/sum(TransactionCount) as avgOrderSize
    ,sum(webMobileSales) as webMobileSales
    ,sum(appSales) as appSales
    , sum(webMobileSales)/sum(TotalItemSales) as webMobilepct
    , sum(appSales)/sum(TotalItemSales) as apppct
    ,sum(WalkToGoSales) as WalkToGoSales
    ,sum(CallToGoSales) as CallToGoSales
    ,sum(WalkDineInSales) as WalkDineInSales
    ,sum(CallDineInSales) as CallDineInSales
    ,sum(DeliverySales) as DeliverySales
    ,sum(DDDeliverySales) as DDDeliverySales
    ,sum(DoorDashDeliverySales) as DoorDashDeliverySales
    , (sum(DeliverySales) + sum(DDDeliverySales) + sum(DoorDashDeliverySales))/sum(TotalItemSales) as totalDeliverypct
    ,(sum(DeliverySales))/sum(TotalItemSales) as Deliverypct
    ,(sum(DDDeliverySales))/sum(TotalItemSales) as DDDeliverypct
    ,(sum(DoorDashDeliverySales))/sum(TotalItemSales) as DoorDashDeliverypct
    , sum(CallDineInSales)/sum(TotalItemSales) as CallDineInpct
    , sum(WalkDineInSales)/sum(TotalItemSales) as WalkDineInpct
    , sum(CallToGoSales)/sum(TotalItemSales) as CallToGopct
    , sum(WalkToGoSales)/sum(TotalItemSales) as WalkToGopct
    
    ,(sum(DeliveryTransactions))/sum(TransactionCount) as DeliveryTransactionspct
    ,(sum(DDDeliveryTransactions))/sum(TransactionCount) as DDDeliveryTransactionspct
    ,(sum(DoorDashDeliveryTransactions))/sum(TransactionCount) as DoorDashDeliveryTransactionspct
    , sum(webMobileTransactions)/sum(TransactionCount) as webMobileTransactionspct
    , sum(appTransactions)/sum(TransactionCount) as appTransactionspct
    , sum(CallDineInTransactions)/sum(TransactionCount) as CallDineInTransactionspct
    , sum(WalkDineInTransactions)/sum(TransactionCount) as WalkDineInTransactionspct
    , sum(CallToGoTransactions)/sum(TransactionCount) as CallToGoTransactionspct
    , sum(WalkToGoTransactions)/sum(TransactionCount) as WalkToGoTransactionspct

from
storeWeek

group by
StoreNo
week,
#deliveryCohort
#yearApprox
  ;'
  )
  
  DeliverySales$otherSales<-1 - DeliverySales$webMobileSales - DeliverySales$appSales- DeliverySales$DeliverySales - DeliverySales$DDDeliverySales - DeliverySales$DoorDashDeliverySales - DeliverySales$WalkDineInSales -
    DeliverySales$WalkToGoSales - DeliverySales$CallDineInSales - DeliverySales$CallToGoSales
  
  DeliverySales$otherSalespct<-1 - DeliverySales$webMobilepct - DeliverySales$apppct - DeliverySales$Deliverypct - DeliverySales$DDDeliverypct - DeliverySales$DoorDashDeliverypct - DeliverySales$WalkDineInpct -
    DeliverySales$WalkToGopct - DeliverySales$CallDineInpct - DeliverySales$CallToGopct
  
  DeliverySales$otherTransactions<-1 - DeliverySales$webMobileTransactions - DeliverySales$appTransactions- DeliverySales$DeliveryTransactions - DeliverySales$DDDeliveryTransactions - DeliverySales$DoorDashDeliveryTransactions - DeliverySales$WalkDineInTransactions -
    DeliverySales$WalkToGoTransactions - DeliverySales$CallDineInTransactions - DeliverySales$CallToGoTransactions
  
  DeliverySales$otherTransactionspct<-1 - DeliverySales$webMobileTransactionspct - DeliverySales$appTransactionspct - DeliverySales$DeliveryTransactionspct - DeliverySales$DDDeliveryTransactionspct - DeliverySales$DoorDashDeliveryTransactionspct - DeliverySales$WalkDineInTransactionspct -
    DeliverySales$WalkToGoTransactionspct - DeliverySales$CallDineInTransactionspct - DeliverySales$CallToGoTransactionspct
  
  DeliverySales$other<-1 - DeliverySales$webMobilepct - DeliverySales$apppct - DeliverySales$DoorDashDeliverypct - DeliverySales$DDDeliverypct -DeliverySales$Deliverypct -
    DeliverySales$WalkDineInpct -
    DeliverySales$WalkToGopct - DeliverySales$CallDineInpct - DeliverySales$CallToGopct
  
  DeliverySales$otherTransactions<-1 - DeliverySales$webMobileTransactionspct - DeliverySales$appTransactionspct - DeliverySales$DeliveryTransactionspct - DeliverySales$DDDeliveryTransactionspct - DeliverySales$DoorDashDeliveryTransactionspct - DeliverySales$WalkDineInTransactionspct -
    DeliverySales$WalkToGoTransactionspct - DeliverySales$CallDineInTransactionspct - DeliverySales$CallToGoTransactionspct
  
  for (cohort in c("Never","2017 or before","2018","2019")){
    mdata <- melt(DeliverySales[DeliverySales$deliveryCohort == cohort , c("yearApprox", "Deliverypct","DDDeliverypct","DoorDashDeliverypct","other", "webMobilepct", 
                                     "apppct",  "CallToGopct", "CallDineInpct", "WalkToGopct", "WalkDineInpct")], id=c("yearApprox"))
    
    p<-ggplot(mdata, aes(x=yearApprox, y=value, fill=variable)) + 
      geom_area() + ggtitle(paste0("Sales By Order Mode, ", cohort, " Cohort")) + xlab("year")
    print(p)
  }
  
  DeliverySales<-sqldf(
    '
    select
    StoreNo,
    week
    ,sum(TotalItemSales) as TotalItemSales
    ,avg(TotalItemSales) as AvgItemSales
    ,sum(TransactionCount) as TransactionCount
    ,avg(TransactionCount) as AvgTransactionCount   
    ,sum(TotalItemSales)/sum(TransactionCount) as avgOrderSize
    ,sum(webMobileSales) as webMobileSales
    ,sum(appSales) as appSales
    , sum(webMobileSales)/sum(TotalItemSales) as webMobilepct
    , sum(appSales)/sum(TotalItemSales) as apppct
    ,sum(WalkToGoSales) as WalkToGoSales
    ,sum(CallToGoSales) as CallToGoSales
    ,sum(WalkDineInSales) as WalkDineInSales
    ,sum(CallDineInSales) as CallDineInSales
    ,sum(DeliverySales) as DeliverySales
    ,sum(DDDeliverySales) as DDDeliverySales
    ,sum(DoorDashDeliverySales) as DoorDashDeliverySales
    , (sum(DeliverySales) + sum(DDDeliverySales) + sum(DoorDashDeliverySales))/sum(TotalItemSales) as totalDeliverypct
    ,(sum(DeliverySales))/sum(TotalItemSales) as Deliverypct
    ,(sum(DDDeliverySales))/sum(TotalItemSales) as DDDeliverypct
    ,(sum(DoorDashDeliverySales))/sum(TotalItemSales) as DoorDashDeliverypct
    , sum(CallDineInSales)/sum(TotalItemSales) as CallDineInpct
    , sum(WalkDineInSales)/sum(TotalItemSales) as WalkDineInpct
    , sum(CallToGoSales)/sum(TotalItemSales) as CallToGopct
    , sum(WalkToGoSales)/sum(TotalItemSales) as WalkToGopct
    
    ,(sum(DeliveryTransactions))/sum(TransactionCount) as DeliveryTransactionspct
    ,(sum(DDDeliveryTransactions))/sum(TransactionCount) as DDDeliveryTransactionspct
    ,(sum(DoorDashDeliveryTransactions))/sum(TransactionCount) as DoorDashDeliveryTransactionspct
    , sum(webMobileTransactions)/sum(TransactionCount) as webMobileTransactionspct
    , sum(appTransactions)/sum(TransactionCount) as appTransactionspct
    , sum(CallDineInTransactions)/sum(TransactionCount) as CallDineInTransactionspct
    , sum(WalkDineInTransactions)/sum(TransactionCount) as WalkDineInTransactionspct
    , sum(CallToGoTransactions)/sum(TransactionCount) as CallToGoTransactionspct
    , sum(WalkToGoTransactions)/sum(TransactionCount) as WalkToGoTransactionspct
,max(DoorDashDeliveryAvailable) as DoorDashDeliveryAvailable
,max(InternalDeliveryAvailable) as InternalDeliveryAvailable
    
    from
    storeWeek
    
    group by
    StoreNo,
    week
    ;'
  )
  
  DeliverySales$otherSales<-1 - DeliverySales$webMobileSales - DeliverySales$appSales- DeliverySales$DeliverySales - DeliverySales$DDDeliverySales - DeliverySales$DoorDashDeliverySales - DeliverySales$WalkDineInSales -
    DeliverySales$WalkToGoSales - DeliverySales$CallDineInSales - DeliverySales$CallToGoSales
  
  DeliverySales$otherSalespct<-1 - DeliverySales$webMobilepct - DeliverySales$apppct - DeliverySales$Deliverypct - DeliverySales$DDDeliverypct - DeliverySales$DoorDashDeliverypct - DeliverySales$WalkDineInpct -
    DeliverySales$WalkToGopct - DeliverySales$CallDineInpct - DeliverySales$CallToGopct
  
  DeliverySales$otherTransactions<-1 - DeliverySales$webMobileTransactions - DeliverySales$appTransactions- DeliverySales$DeliveryTransactions - DeliverySales$DDDeliveryTransactions - DeliverySales$DoorDashDeliveryTransactions - DeliverySales$WalkDineInTransactions -
    DeliverySales$WalkToGoTransactions - DeliverySales$CallDineInTransactions - DeliverySales$CallToGoTransactions
  
  DeliverySales$otherTransactionspct<-1 - DeliverySales$webMobileTransactionspct - DeliverySales$appTransactionspct - DeliverySales$DeliveryTransactionspct - DeliverySales$DDDeliveryTransactionspct - DeliverySales$DoorDashDeliveryTransactionspct - DeliverySales$WalkDineInTransactionspct -
    DeliverySales$WalkToGoTransactionspct - DeliverySales$CallDineInTransactionspct - DeliverySales$CallToGoTransactionspct
  
  DeliverySales$other<-1 - DeliverySales$webMobilepct - DeliverySales$apppct - DeliverySales$DoorDashDeliverypct - DeliverySales$DDDeliverypct -DeliverySales$Deliverypct -
    DeliverySales$WalkDineInpct -
    DeliverySales$WalkToGopct - DeliverySales$CallDineInpct - DeliverySales$CallToGopct
  
  DeliverySales$otherTransactions<-1 - DeliverySales$webMobileTransactionspct - DeliverySales$appTransactionspct - DeliverySales$DeliveryTransactionspct - DeliverySales$DDDeliveryTransactionspct - DeliverySales$DoorDashDeliveryTransactionspct - DeliverySales$WalkDineInTransactionspct -
    DeliverySales$WalkToGoTransactionspct - DeliverySales$CallDineInTransactionspct - DeliverySales$CallToGoTransactionspct
  
 DoorDashList<-unique(storeWeek$StoreNo[storeWeek$DoorDashDeliveryAvailable == 1])
InternalList<-unique(storeWeek$StoreNo[storeWeek$InternalDeliveryAvailable == 1])

test<-DeliverySales[DeliverySales$StoreNo %in% DoorDashList &DeliverySales$StoreNo %in% InternalList, ] %>%
  group_by(DoorDashDeliveryAvailable, InternalDeliveryAvailable) %>%
  summarise(DeliverySales = sum(DeliverySales), DoorDashDeliverySales = sum(DoorDashDeliverySales))
  
ggplot(DeliverySales, aes(x=yearApprox, y=AvgItemSales, colour=deliveryCohort)) + geom_line()+ ggtitle("Avg Sales Growth by Cohort") + xlab("year")
ggplot(DeliverySales, aes(x=yearApprox, y=AvgTransactionCount, colour=deliveryCohort)) + geom_line()+ ggtitle("Avg Transactions Growth by Cohort") + xlab("year")
ggplot(DeliverySales, aes(x=yearApprox, y=avgOrderSize, colour=deliveryCohort)) + geom_line()+ ggtitle("Avg Ticket Size by Cohort") + xlab("year")

###delivery quintiles
deliveryList<-unique(storeWeek$StoreNo[storeWeek$AnyDeliveryAvailable == 1])
DeliveryStoreWeek<-storeWeek[storeWeek$StoreNo %in% deliveryList, ]

DeliverySales<-sqldf(
  '
  select
  
  AnyDeliveryAvailable,
  StoreNo
  
  ,sum(TotalItemSales) as TotalItemSales
  ,avg(TotalItemSales) as AvgItemSales
  ,sum(TransactionCount) as TransactionCount
  ,avg(TransactionCount) as AvgTransactionCount   
  
  , (sum(DeliverySales) + sum(DDDeliverySales) + sum(DoorDashDeliverySales))/sum(TotalItemSales) as totalDeliverypct
  
  ,(sum(DoorDashDeliveryTransactions) + sum(DDDeliveryTransactions) + sum(DeliveryTransactions)) as AllDeliveryTransactions
,(sum(DoorDashDeliveryTransactions) + sum(DDDeliveryTransactions) + sum(DeliveryTransactions))/sum(TransactionCount) as AllDeliveryTransactionsPct
  from
  DeliveryStoreWeek
  
  group by
  AnyDeliveryAvailable,
  StoreNo
  ;'
  )

salesWide<-dcast(DeliverySales, StoreNo ~ AnyDeliveryAvailable, value.var = "AvgItemSales")
names(salesWide)<-c("StoreNo", "PreDeliverySales", "PostDeliverySales")
salesWide$PctSalesIncrease<-(salesWide$PostDeliverySales - salesWide$PreDeliverySales)/salesWide$PreDeliverySales

transactionsWide<-dcast(DeliverySales, StoreNo ~ AnyDeliveryAvailable, value.var = "AvgTransactionCount")
names(transactionsWide)<-c("StoreNo", "PreDeliveryTransactions", "PostDeliveryTransactions")
transactionsWide$PctTransactionsIncrease<-(transactionsWide$PostDeliveryTransactions - transactionsWide$PreDeliveryTransactions)/transactionsWide$PreDeliveryTransactions

transactionsWide<-transactionsWide %>%
  mutate(quintile = ntile(PctTransactionsIncrease, 5))

transactionsWide<-transactionsWide[ , c("StoreNo", "TransactionQuintile")]
storeWeek<-left_join(x = storeWeek, y = transactionsWide, by = c("StoreNo" = "StoreNo"))
names(storeWeek)[names(storeWeek) %in% "quintile"] <-"TransactionQuintile"

table(storeWeek$TransactionQuintile)
delivWeek<-storeWeek[!is.na(storeWeek$TransactionQuintile), ]

deliveryDates<-sqldf(
  '
select

StoreNo,
min(CASE WHEN COALESCE(AnyDeliveryAvailable,0) > 0 THEN WeekOf ELSE NULL END) as FirstDeliveryDate

from
storeWeek

group by
StoreNo
  ;'
)

deliveryDates$FirstDeliveryDate<-as.Date(deliveryDates$FirstDeliveryDate)

delivWeek<-left_join(x = delivWeek, y = deliveryDates, by = c("StoreNo"))
delivWeek<-left_join(x = delivWeek, y = stores, by = c("StoreNo"))
delivWeek$openingDate<-as.Date(delivWeek$openingDate)
delivWeek$ageAtDelivery<-as.numeric(floor((delivWeek$FirstDeliveryDate.y - delivWeek$openingDate)/7))

deliverySummary<-delivWeek%>%
  group_by(TransactionQuintile)%>%
  summarise(
    quintile2 = mean(quintile2),
    quintile3 = mean(quintile3),
    quintile4 = mean(quintile4),
    quintile5 = mean(quintile5),
    DMAStoreCount = mean(DMAStoreCount),
    wingstops_within_3_miles = mean(wingstops_within_3_miles),
    Buffalo_Wild_Wings_within_3_miles = mean(Buffalo_Wild_Wings_within_3_miles),
    Chick_Fil_A_within_3_miles = mean(Chick_Fil_A_within_3_miles), 
    Chipotle_within_3_miles = mean(Chipotle_within_3_miles), 
    KFC_within_3_miles = mean(KFC_within_3_miles), 
    Panda_Express_within_3_miles = mean(Panda_Express_within_3_miles),
    Pizza_Hut_within_3_miles = mean(Pizza_Hut_within_3_miles),
    Taco_Bell_within_3_miles = mean(Taco_Bell_within_3_miles),
    Whataburger_within_3_miles = mean(Whataburger_within_3_miles),
    Dominos_within_3_miles = mean(Dominos_within_3_miles), 
    competitors_within_3_miles = mean(competitors_within_3_miles), 
    marketStatusCore = mean(marketStatusCore), 
    marketStatusDeveloping = mean(marketStatusDeveloping),
    Deliverydate = mean(as.Date(FirstDeliveryDate.y)),
    ageAtDelivery = median(ageAtDelivery),
    openingDate = mean(as.Date(openingDate)),
    householdsPerStore = mean(householdsPerStore), 
    AvgSatisfaction = mean(OverallSatisfaction),
    n = n()
  )


write.csv(t(deliverySummary), "delivery quintile summary.csv")


###dma level variation in incrementality -- store level

transSize2017<-First52Weeks%>%
  group_by(StoreNo)%>%
  summarise(avgTransactionSize = mean(avgTransactionSales, na.rm=TRUE), avgSales = mean(TotalItemSales, na.rm=TRUE))

transSize2019<-Last52Weeks%>%
  group_by(StoreNo)%>%
  summarise(avgTransactionSize = mean(avgTransactionSales, na.rm=TRUE), avgSales = mean(TotalItemSales, na.rm=TRUE))

changeInTransSize<-inner_join(transSize2017, transSize2019, by = c("StoreNo"))

changeInTransSize$PercentChangeTransactions<-(changeInTransSize$avgTransactionSize.y - changeInTransSize$avgTransactionSize.x)/changeInTransSize$avgTransactionSize.x * 100

changeInTransSize$PercentChangeSales<-(changeInTransSize$avgSales.y - changeInTransSize$avgSales.x)/changeInTransSize$avgSales.x * 100

incrementality<-left_join(x = storeWeek, y = changeInTransSize, by = c("StoreNo"))

increment<-sqldf('
                     SELECT
                 StoreNo,
DMA_NAME
                ,avg(DMAPriceIndex) as DMAPriceIndex
,avg(Buffalo_Wild_Wings_within_3_miles + 
Chick_Fil_A_within_3_miles + 
                 Dominos_within_3_miles) as replacement_competitors_within_3_miles
,avg(wingstops_within_3_miles) as wingstops_within_3_miles
,avg(competitors_within_3_miles) as competitors_within_3_miles
,avg(ageOfStoreInWeeks) as avgAgeofStore
                 ,avg(OverallSatisfaction) as AvgOSAT
,avg(PercentChangeTransactions) as PercentChangeTransactions
,avg(PercentChangeSales) as PercentChangeSales  
,avg(householdsPerStore)  as    householdsPerStore 
                 
                 
                 FROM
                 incrementality as s
                 
                 GROUP BY
                 StoreNo, DMA_NAME
                 ;')

dmas<-c("DALLAS-FT. WORTH", "LOS ANGELES", "CHICAGO")
hold<-increment[ increment$DMA_NAME %in% dmas & increment$PercentChangeSales < 100, ]
hold<-hold[ !is.na(hold$DMA_NAME), ]
ggplot(hold, aes(x = DMAPriceIndex, y = PercentChangeTransactions, colour = DMA_NAME )) + geom_point()
ggplot(hold, aes(x = replacement_competitors_within_3_miles, y = PercentChangeTransactions, colour = DMA_NAME )) + geom_point()
ggplot(hold, aes(x = competitors_within_3_miles, y = PercentChangeTransactions, colour = DMA_NAME )) + geom_point()
ggplot(hold, aes(x = wingstops_within_3_miles, y = PercentChangeTransactions, colour = DMA_NAME )) + geom_point()
ggplot(hold, aes(x = avgAgeofStore, y = PercentChangeTransactions, colour = DMA_NAME )) + geom_point()
ggplot(hold, aes(x = AvgOSAT, y = PercentChangeTransactions, colour = DMA_NAME )) + geom_point()
ggplot(hold, aes(x = householdsPerStore, y = PercentChangeTransactions, colour = DMA_NAME )) + geom_point()

ggplot(hold, aes(x = DMAPriceIndex, y = PercentChangeSales, colour = DMA_NAME )) + geom_point()
ggplot(hold, aes(x = replacement_competitors_within_3_miles, y = PercentChangeSales, colour = DMA_NAME )) + geom_point()
ggplot(hold, aes(x = avgAgeofStore, y = PercentChangeSales, colour = DMA_NAME )) + geom_point()
ggplot(hold, aes(x = AvgOSAT, y = PercentChangeSales, colour = DMA_NAME )) + geom_point()
ggplot(hold, aes(x = householdsPerStore, y = PercentChangeSales, colour = DMA_NAME )) + geom_point()
ggplot(hold, aes(x = competitors_within_3_miles, y = PercentChangeSales, colour = DMA_NAME )) + geom_point()
ggplot(hold, aes(x = wingstops_within_3_miles, y = PercentChangeSales, colour = DMA_NAME )) + geom_point()


increment<-sqldf('
                     SELECT
DMA_NAME
               
,avg(Buffalo_Wild_Wings_within_3_miles + 
Chick_Fil_A_within_3_miles + 
                 Dominos_within_3_miles) as replacement_competitors_within_3_miles
,avg(wingstops_within_3_miles) as wingstops_within_3_miles
,avg(competitors_within_3_miles) as competitors_within_3_miles
,avg(ageOfStoreInWeeks) as avgAgeofStore
                 ,avg(OverallSatisfaction) as AvgOSAT
,avg(PercentChangeTransactions) as PercentChangeTransactions
,avg(PercentChangeSales) as PercentChangeSales  
,avg(householdsPerStore)  as    householdsPerStore 
                 
                 
                 FROM
                 incrementality as s
                 
                 GROUP BY
                 DMA_NAME
                 ;')

hold<-increment[!is.na(increment$DMA_NAME),]
ggplot(hold, aes(x = replacement_competitors_within_3_miles, y = PercentChangeSales )) + geom_point()
ggplot(hold, aes(x = avgAgeofStore, y = PercentChangeSales )) + geom_point()
ggplot(hold, aes(x = AvgOSAT, y = PercentChangeSales)) + geom_point()
ggplot(hold, aes(x = householdsPerStore, y = PercentChangeSales)) + geom_point()
ggplot(hold, aes(x = competitors_within_3_miles, y = PercentChangeSales )) + geom_point()
ggplot(hold, aes(x = wingstops_within_3_miles, y = PercentChangeSales)) + geom_point()



####Incrementality by delivery source
storeWeeklog<-storeWeek
spendVars<-names(storeWeeklog)[grepl("Spend|spend", names(storeWeeklog))]
for (var in spendVars){
  eval(parse(text = paste0("storeWeeklog$",var,"[storeWeeklog$",var," == 0]<-1")))
}
fit<-lm(log(storeWeeklog$TotalItemSales)~
          storeWeeklog$ageOfStoreInWeeks+
          storeWeeklog$newStore30weeks + 
          storeWeeklog$ageOfStoreInWeeks*storeWeeklog$newStore30weeks+
          storeWeeklog$quintile2 + 
          storeWeeklog$quintile3 + 
          storeWeeklog$quintile4 + 
          storeWeeklog$quintile5 + 
        storeWeeklog$samplingEvent+
          storeWeeklog$raining_float+
          storeWeeklog$snowing_float+
          storeWeeklog$DigitalOSAT+
          storeWeeklog$wingstops_within_3_miles+
          storeWeeklog$Buffalo_Wild_Wings_within_3_miles+
          storeWeeklog$Chick_Fil_A_within_3_miles+
          storeWeeklog$Chipotle_within_3_miles+
          storeWeeklog$KFC_within_3_miles+
          storeWeeklog$Panda_Express_within_3_miles+
          storeWeeklog$Pizza_Hut_within_3_miles+
          storeWeeklog$Taco_Bell_within_3_miles+
          storeWeeklog$Whataburger_within_3_miles+
          storeWeeklog$Dominos_within_3_miles+
          storeWeeklog$unemploymentRate+
          storeWeeklog$hotDay_float+
          storeWeeklog$coldDay_float+
          storeWeeklog$spring_float+
          storeWeeklog$fall_float+
          storeWeeklog$winter_float+
          storeWeeklog$college_football_game_float+
          storeWeeklog$college_football_playoffs+
          storeWeeklog$cultural_holiday+
          storeWeeklog$federal_holiday+
          storeWeeklog$four_twenty+
          storeWeeklog$march_madness_float+
          storeWeeklog$nfl_game_float+
          storeWeeklog$payday_calendar_float+
          storeWeeklog$superbowl+
          storeWeeklog$UFC_fight_float+
          storeWeeklog$christmas+
          storeWeeklog$easter+
          storeWeeklog$thanksgiving+
          storeWeeklog$marketStatusEmerging+
          storeWeeklog$marketStatusDeveloping+
          storeWeeklog$householdsPerStore+
          
         
          storeWeeklog$InternalDeliveryAvailable +
          storeWeeklog$DoorDashDeliveryAvailable+
          storeWeeklog$InternalDeliveryAvailable*storeWeeklog$DoorDashDeliveryAvailable+
          storeWeeklog$InternalDeliveryAvailable * storeWeeklog$quintile2+
          storeWeeklog$InternalDeliveryAvailable * storeWeeklog$quintile3 +
          storeWeeklog$InternalDeliveryAvailable * storeWeeklog$quintile4+
          storeWeeklog$InternalDeliveryAvailable * storeWeeklog$quintile5+
          storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$quintile2+
          storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$quintile3 +
          storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$quintile4+
          storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$quintile5
)

summary(fit)

fit<-lm(log(storeWeeklog$TransactionCount)~
          storeWeeklog$ageOfStoreInWeeks+
          storeWeeklog$newStore30weeks + 
          storeWeeklog$ageOfStoreInWeeks*storeWeeklog$newStore30weeks+
          storeWeeklog$quintile2 + 
          storeWeeklog$quintile3 + 
          storeWeeklog$quintile4 + 
          storeWeeklog$quintile5 + 
          storeWeeklog$samplingEvent+
          storeWeeklog$raining_float+
          storeWeeklog$snowing_float+
          storeWeeklog$DigitalOSAT+
          storeWeeklog$wingstops_within_3_miles+
          storeWeeklog$Buffalo_Wild_Wings_within_3_miles+
          storeWeeklog$Chick_Fil_A_within_3_miles+
          storeWeeklog$Chipotle_within_3_miles+
          storeWeeklog$KFC_within_3_miles+
          storeWeeklog$Panda_Express_within_3_miles+
          storeWeeklog$Pizza_Hut_within_3_miles+
          storeWeeklog$Taco_Bell_within_3_miles+
          storeWeeklog$Whataburger_within_3_miles+
          storeWeeklog$Dominos_within_3_miles+
          storeWeeklog$unemploymentRate+
          storeWeeklog$hotDay_float+
          storeWeeklog$coldDay_float+
          storeWeeklog$spring_float+
          storeWeeklog$fall_float+
          storeWeeklog$winter_float+
          storeWeeklog$college_football_game_float+
          storeWeeklog$college_football_playoffs+
          storeWeeklog$cultural_holiday+
          storeWeeklog$federal_holiday+
          storeWeeklog$four_twenty+
          storeWeeklog$march_madness_float+
          storeWeeklog$nfl_game_float+
          storeWeeklog$payday_calendar_float+
          storeWeeklog$superbowl+
          storeWeeklog$UFC_fight_float+
          storeWeeklog$christmas+
          storeWeeklog$easter+
          storeWeeklog$thanksgiving+
          storeWeeklog$marketStatusEmerging+
          storeWeeklog$marketStatusDeveloping+
          storeWeeklog$householdsPerStore+
          
          log(storeWeeklog$OLOSocialSpendTruncated) + 
          log(storeWeeklog$OLOSocialSpendTruncatedlag1) + 
          log(storeWeeklog$OLOSearchSpendTruncated) +
          log(storeWeeklog$OLOSearchSpendTruncatedlag1 )+
          log(storeWeeklog$OLODisplaySpendTruncated) +
          log(storeWeeklog$OLODisplaySpendTruncatedlag1) +
          log(storeWeeklog$OLVSocialSpendTruncatedDiscounted) +
          log(storeWeeklog$OLVNationalSpendTruncatedDiscounted) +
          log(storeWeeklog$tvspendTruncatedDiscounted) +
          
          log(storeWeeklog$localoohSpendDiscounted) +
          log(storeWeeklog$localsponsorsSpendDiscounted) +
          log(storeWeeklog$localsocialSpendDiscounted) +
          log(storeWeeklog$localdigitalSpendDiscounted) +
          log(storeWeeklog$localradioSpendDiscounted) +
          log(storeWeeklog$localotherSpendDiscounted) +
          log(storeWeeklog$localtvSpendDiscounted) +
          log(storeWeeklog$localmobileSpendDiscounted) +
          storeWeeklog$InternalDeliveryAvailable +
          storeWeeklog$DoorDashDeliveryAvailable+
          storeWeeklog$InternalDeliveryAvailable*storeWeeklog$DoorDashDeliveryAvailable+
          storeWeeklog$InternalDeliveryAvailable * storeWeeklog$quintile2+
          storeWeeklog$InternalDeliveryAvailable * storeWeeklog$quintile3 +
          storeWeeklog$InternalDeliveryAvailable * storeWeeklog$quintile4+
          storeWeeklog$InternalDeliveryAvailable * storeWeeklog$quintile5+
          storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$quintile2+
          storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$quintile3 +
          storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$quintile4+
          storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$quintile5
)

summary(fit)

storeWeek$quintile<-"Quintile 1"
storeWeek$quintile[storeWeek$quintile2 == 1]<-"Quintile 2"
storeWeek$quintile[storeWeek$quintile3 == 1]<-"Quintile 3"
storeWeek$quintile[storeWeek$quintile4 == 1]<-"Quintile 4"
storeWeek$quintile[storeWeek$quintile5 == 1]<-"Quintile 5"


fit<-lm(log(storeWeeklog$TotalItemSales)~
          storeWeeklog$ageOfStoreInWeeks+
          storeWeeklog$newStore30weeks + 
          storeWeeklog$ageOfStoreInWeeks*storeWeeklog$newStore30weeks+
          storeWeeklog$quintile2 + 
          storeWeeklog$quintile3 + 
          storeWeeklog$quintile4 + 
          storeWeeklog$quintile5 + 
          storeWeeklog$samplingEvent+
          storeWeeklog$raining_float+
          storeWeeklog$snowing_float+
          storeWeeklog$DigitalOSAT+
          storeWeeklog$wingstops_within_3_miles+
          storeWeeklog$Buffalo_Wild_Wings_within_3_miles+
          storeWeeklog$Chick_Fil_A_within_3_miles+
          storeWeeklog$Chipotle_within_3_miles+
          storeWeeklog$KFC_within_3_miles+
          storeWeeklog$Panda_Express_within_3_miles+
          storeWeeklog$Pizza_Hut_within_3_miles+
          storeWeeklog$Taco_Bell_within_3_miles+
          storeWeeklog$Whataburger_within_3_miles+
          storeWeeklog$Dominos_within_3_miles+
          storeWeeklog$unemploymentRate+
          storeWeeklog$hotDay_float+
          storeWeeklog$coldDay_float+
          storeWeeklog$spring_float+
          storeWeeklog$fall_float+
          storeWeeklog$winter_float+
          storeWeeklog$college_football_game_float+
          storeWeeklog$college_football_playoffs+
          storeWeeklog$cultural_holiday+
          storeWeeklog$federal_holiday+
          storeWeeklog$four_twenty+
          storeWeeklog$march_madness_float+
          storeWeeklog$nfl_game_float+
          storeWeeklog$payday_calendar_float+
          storeWeeklog$superbowl+
          storeWeeklog$UFC_fight_float+
          storeWeeklog$christmas+
          storeWeeklog$easter+
          storeWeeklog$thanksgiving+
          storeWeeklog$marketStatusEmerging+
          storeWeeklog$marketStatusDeveloping+
          storeWeeklog$householdsPerStore+
          
          log(storeWeeklog$OLOSocialSpendTruncated) + 
          log(storeWeeklog$OLOSocialSpendTruncatedlag1) + 
          log(storeWeeklog$OLOSearchSpendTruncated) +
          log(storeWeeklog$OLOSearchSpendTruncatedlag1 )+
          log(storeWeeklog$OLODisplaySpendTruncated) +
          log(storeWeeklog$OLODisplaySpendTruncatedlag1) +
          log(storeWeeklog$OLVSocialSpendTruncatedDiscounted) +
          log(storeWeeklog$OLVNationalSpendTruncatedDiscounted) +
          log(storeWeeklog$tvspendTruncatedDiscounted) +
          
          log(storeWeeklog$localoohSpendDiscounted) +
          log(storeWeeklog$localsponsorsSpendDiscounted) +
          log(storeWeeklog$localsocialSpendDiscounted) +
          log(storeWeeklog$localdigitalSpendDiscounted) +
          log(storeWeeklog$localradioSpendDiscounted) +
          log(storeWeeklog$localotherSpendDiscounted) +
          log(storeWeeklog$localtvSpendDiscounted) +
          log(storeWeeklog$localmobileSpendDiscounted) +
          storeWeeklog$AnyDeliveryAvailable +
          
          storeWeeklog$AnyDeliveryAvailable * storeWeeklog$marketStatusEmerging+
          storeWeeklog$AnyDeliveryAvailable * storeWeeklog$marketStatusDeveloping#+
          #storeWeeklog$InternalDeliveryAvailable*storeWeeklog$DoorDashDeliveryAvailable+
          #storeWeeklog$InternalDeliveryAvailable*storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$marketStatusEmerging+
          #storeWeeklog$InternalDeliveryAvailable * storeWeeklog$marketStatusEmerging+
          #storeWeeklog$InternalDeliveryAvailable * storeWeeklog$marketStatusDeveloping +
          #storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$marketStatusEmerging+
          #storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$marketStatusDeveloping
          #storeWeeklog$InternalDeliveryAvailable*storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$marketStatusDeveloping
)

summary(fit)


marketCounts<-storeWeek%>%
  group_by(marketStatus)%>%
  filter(AnyDeliveryAvailable == 1) %>%
  summarise(storeCounts = n_distinct(StoreNo))

fit<-lm(log(storeWeeklog$TransactionCount)~
          storeWeeklog$ageOfStoreInWeeks+
          storeWeeklog$newStore30weeks + 
          storeWeeklog$ageOfStoreInWeeks*storeWeeklog$newStore30weeks+
          storeWeeklog$quintile2 + 
          storeWeeklog$quintile3 + 
          storeWeeklog$quintile4 + 
          storeWeeklog$quintile5 + 
          storeWeeklog$samplingEvent+
          storeWeeklog$raining_float+
          storeWeeklog$snowing_float+
          storeWeeklog$DigitalOSAT+
          storeWeeklog$wingstops_within_3_miles+
          storeWeeklog$Buffalo_Wild_Wings_within_3_miles+
          storeWeeklog$Chick_Fil_A_within_3_miles+
          storeWeeklog$Chipotle_within_3_miles+
          storeWeeklog$KFC_within_3_miles+
          storeWeeklog$Panda_Express_within_3_miles+
          storeWeeklog$Pizza_Hut_within_3_miles+
          storeWeeklog$Taco_Bell_within_3_miles+
          storeWeeklog$Whataburger_within_3_miles+
          storeWeeklog$Dominos_within_3_miles+
          storeWeeklog$unemploymentRate+
          storeWeeklog$hotDay_float+
          storeWeeklog$coldDay_float+
          storeWeeklog$spring_float+
          storeWeeklog$fall_float+
          storeWeeklog$winter_float+
          storeWeeklog$college_football_game_float+
          storeWeeklog$college_football_playoffs+
          storeWeeklog$cultural_holiday+
          storeWeeklog$federal_holiday+
          storeWeeklog$four_twenty+
          storeWeeklog$march_madness_float+
          storeWeeklog$nfl_game_float+
          storeWeeklog$payday_calendar_float+
          storeWeeklog$superbowl+
          storeWeeklog$UFC_fight_float+
          storeWeeklog$christmas+
          storeWeeklog$easter+
          storeWeeklog$thanksgiving+
          storeWeeklog$marketStatusEmerging+
          storeWeeklog$marketStatusDeveloping+
          storeWeeklog$householdsPerStore+
          log(storeWeeklog$OLOSocialSpendTruncated) + 
          log(storeWeeklog$OLOSocialSpendTruncatedlag1) + 
          log(storeWeeklog$OLOSearchSpendTruncated) +
          log(storeWeeklog$OLOSearchSpendTruncatedlag1 )+
          log(storeWeeklog$OLODisplaySpendTruncated) +
          log(storeWeeklog$OLODisplaySpendTruncatedlag1) +
          log(storeWeeklog$OLVSocialSpendTruncatedDiscounted) +
          log(storeWeeklog$OLVNationalSpendTruncatedDiscounted) +
          log(storeWeeklog$tvspendTruncatedDiscounted) +
          
          log(storeWeeklog$localoohSpendDiscounted) +
          log(storeWeeklog$localsponsorsSpendDiscounted) +
          log(storeWeeklog$localsocialSpendDiscounted) +
          log(storeWeeklog$localdigitalSpendDiscounted) +
          log(storeWeeklog$localradioSpendDiscounted) +
          log(storeWeeklog$localotherSpendDiscounted) +
          log(storeWeeklog$localtvSpendDiscounted) +
          log(storeWeeklog$localmobileSpendDiscounted) +

          storeWeeklog$AnyDeliveryAvailable +
          
          storeWeeklog$AnyDeliveryAvailable * storeWeeklog$marketStatusEmerging+
          storeWeeklog$AnyDeliveryAvailable * storeWeeklog$marketStatusDeveloping
          #storeWeeklog$InternalDeliveryAvailable*storeWeeklog$DoorDashDeliveryAvailable+
          #storeWeeklog$InternalDeliveryAvailable*storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$marketStatusEmerging+
          

          
        #storeWeeklog$InternalDeliveryAvailable*storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$marketStatusDeveloping
        )

summary(fit)



################## total delivery


fit<-lm(log(storeWeeklog$TotalItemSales)~
          storeWeeklog$ageOfStoreInWeeks+
          storeWeeklog$newStore30weeks + 
          storeWeeklog$ageOfStoreInWeeks*storeWeeklog$newStore30weeks+
          storeWeeklog$quintile2 + 
          storeWeeklog$quintile3 + 
          storeWeeklog$quintile4 + 
          storeWeeklog$quintile5 + 
          storeWeeklog$samplingEvent+
          storeWeeklog$raining_float+
          storeWeeklog$snowing_float+
          storeWeeklog$DigitalOSAT+
          storeWeeklog$wingstops_within_3_miles+
          storeWeeklog$Buffalo_Wild_Wings_within_3_miles+
          storeWeeklog$Chick_Fil_A_within_3_miles+
          storeWeeklog$Chipotle_within_3_miles+
          storeWeeklog$KFC_within_3_miles+
          storeWeeklog$Panda_Express_within_3_miles+
          storeWeeklog$Pizza_Hut_within_3_miles+
          storeWeeklog$Taco_Bell_within_3_miles+
          storeWeeklog$Whataburger_within_3_miles+
          storeWeeklog$Dominos_within_3_miles+
          storeWeeklog$unemploymentRate+
          storeWeeklog$hotDay_float+
          storeWeeklog$coldDay_float+
          storeWeeklog$spring_float+
          storeWeeklog$fall_float+
          storeWeeklog$winter_float+
          storeWeeklog$college_football_game_float+
          storeWeeklog$college_football_playoffs+
          storeWeeklog$cultural_holiday+
          storeWeeklog$federal_holiday+
          storeWeeklog$four_twenty+
          storeWeeklog$march_madness_float+
          storeWeeklog$nfl_game_float+
          storeWeeklog$payday_calendar_float+
          storeWeeklog$superbowl+
          storeWeeklog$UFC_fight_float+
          storeWeeklog$christmas+
          storeWeeklog$easter+
          storeWeeklog$thanksgiving+
          storeWeeklog$marketStatusEmerging+
          storeWeeklog$marketStatusDeveloping+
          storeWeeklog$householdsPerStore+
          log(storeWeeklog$OLOSocialSpendTruncated) + 
          log(storeWeeklog$OLOSocialSpendTruncatedlag1) + 
          log(storeWeeklog$OLOSearchSpendTruncated) +
          log(storeWeeklog$OLOSearchSpendTruncatedlag1 )+
          log(storeWeeklog$OLODisplaySpendTruncated) +
          log(storeWeeklog$OLODisplaySpendTruncatedlag1) +
          log(storeWeeklog$OLVSocialSpendTruncatedDiscounted) +
          log(storeWeeklog$OLVNationalSpendTruncatedDiscounted) +
          log(storeWeeklog$tvspendTruncatedDiscounted) +
          
          log(storeWeeklog$localoohSpendDiscounted) +
          log(storeWeeklog$localsponsorsSpendDiscounted) +
          log(storeWeeklog$localsocialSpendDiscounted) +
          log(storeWeeklog$localdigitalSpendDiscounted) +
          log(storeWeeklog$localradioSpendDiscounted) +
          log(storeWeeklog$localotherSpendDiscounted) +
          log(storeWeeklog$localtvSpendDiscounted) +
          log(storeWeeklog$localmobileSpendDiscounted) +
          
          storeWeeklog$AnyDeliveryAvailable +
          
          storeWeeklog$AnyDeliveryAvailable * storeWeeklog$marketStatusEmerging+
          storeWeeklog$AnyDeliveryAvailable * storeWeeklog$marketStatusDeveloping
        #storeWeeklog$InternalDeliveryAvailable*storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$marketStatusDeveloping
)

summary(fit)

fit<-lm(log(storeWeeklog$TransactionCount)~
          storeWeeklog$ageOfStoreInWeeks+
          storeWeeklog$newStore30weeks + 
          storeWeeklog$ageOfStoreInWeeks*storeWeeklog$newStore30weeks+
          storeWeeklog$quintile2 + 
          storeWeeklog$quintile3 + 
          storeWeeklog$quintile4 + 
          storeWeeklog$quintile5 + 
          storeWeeklog$samplingEvent+
          storeWeeklog$raining_float+
          storeWeeklog$snowing_float+
          storeWeeklog$DigitalOSAT+
          storeWeeklog$wingstops_within_3_miles+
          storeWeeklog$Buffalo_Wild_Wings_within_3_miles+
          storeWeeklog$Chick_Fil_A_within_3_miles+
          storeWeeklog$Chipotle_within_3_miles+
          storeWeeklog$KFC_within_3_miles+
          storeWeeklog$Panda_Express_within_3_miles+
          storeWeeklog$Pizza_Hut_within_3_miles+
          storeWeeklog$Taco_Bell_within_3_miles+
          storeWeeklog$Whataburger_within_3_miles+
          storeWeeklog$Dominos_within_3_miles+
          storeWeeklog$unemploymentRate+
          storeWeeklog$hotDay_float+
          storeWeeklog$coldDay_float+
          storeWeeklog$spring_float+
          storeWeeklog$fall_float+
          storeWeeklog$winter_float+
          storeWeeklog$college_football_game_float+
          storeWeeklog$college_football_playoffs+
          storeWeeklog$cultural_holiday+
          storeWeeklog$federal_holiday+
          storeWeeklog$four_twenty+
          storeWeeklog$march_madness_float+
          storeWeeklog$nfl_game_float+
          storeWeeklog$payday_calendar_float+
          storeWeeklog$superbowl+
          storeWeeklog$UFC_fight_float+
          storeWeeklog$christmas+
          storeWeeklog$easter+
          storeWeeklog$thanksgiving+
          storeWeeklog$marketStatusEmerging+
          storeWeeklog$marketStatusDeveloping+
          storeWeeklog$householdsPerStore+
          
          log(storeWeeklog$OLOSocialSpendTruncated) + 
          log(storeWeeklog$OLOSocialSpendTruncatedlag1) + 
          log(storeWeeklog$OLOSearchSpendTruncated) +
          log(storeWeeklog$OLOSearchSpendTruncatedlag1 )+
          log(storeWeeklog$OLODisplaySpendTruncated) +
          log(storeWeeklog$OLODisplaySpendTruncatedlag1) +
          log(storeWeeklog$OLVSocialSpendTruncatedDiscounted) +
          log(storeWeeklog$OLVNationalSpendTruncatedDiscounted) +
          log(storeWeeklog$tvspendTruncatedDiscounted) +
          
          log(storeWeeklog$localoohSpendDiscounted) +
          log(storeWeeklog$localsponsorsSpendDiscounted) +
          log(storeWeeklog$localsocialSpendDiscounted) +
          log(storeWeeklog$localdigitalSpendDiscounted) +
          log(storeWeeklog$localradioSpendDiscounted) +
          log(storeWeeklog$localotherSpendDiscounted) +
          log(storeWeeklog$localtvSpendDiscounted) +
          log(storeWeeklog$localmobileSpendDiscounted) +
          storeWeeklog$AnyDeliveryAvailable  +
          
          storeWeeklog$AnyDeliveryAvailable * storeWeeklog$marketStatusEmerging+
          storeWeeklog$AnyDeliveryAvailable * storeWeeklog$marketStatusDeveloping
        #storeWeeklog$InternalDeliveryAvailable*storeWeeklog$DoorDashDeliveryAvailable+
        #storeWeeklog$InternalDeliveryAvailable*storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$marketStatusEmerging+
        
        
        
        #storeWeeklog$InternalDeliveryAvailable*storeWeeklog$DoorDashDeliveryAvailable * storeWeeklog$marketStatusDeveloping
)

summary(fit)

actualTransactionsIncreaseAny<- storeWeek[storeWeek$StoreNo %in% InternalList | storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(AnyDeliveryAvailable) %>%
  summarise(avgTransactions = mean(TransactionCount), count = n_distinct(StoreNo))

totalTransactionsIncreaseAny<- storeWeek[storeWeek$StoreNo %in% InternalList | storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(AnyDeliveryAvailable) %>%
  summarise(totalTransactions = sum(TransactionCount), count = n_distinct(StoreNo))

actualSalesIncreaseAny<- storeWeek[storeWeek$StoreNo %in% InternalList | storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(AnyDeliveryAvailable,marketStatus) %>%
  summarise(avgTransactions = mean(TransactionCount), count = n_distinct(StoreNo))

actualTransactionsIncreaseAny<- storeWeek[storeWeek$StoreNo %in% InternalList | storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(AnyDeliveryAvailable, marketStatus) %>%
  summarise(avgDelivery = mean(totalDeliveryTransactions), count = n_distinct(StoreNo))

actualSalesIncreaseAny<- storeWeek[storeWeek$StoreNo %in% InternalList | storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(AnyDeliveryAvailable) %>%
  summarise(avgSales = mean(TotalItemSales), count = n_distinct(StoreNo))

actualSalesIncreaseAny<- storeWeek[storeWeek$StoreNo %in% InternalList | storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(AnyDeliveryAvailable,marketStatus) %>%
  summarise(avgSales = mean(TotalItemSales), count = n_distinct(StoreNo))

actualSalesIncreaseAny<- storeWeek[storeWeek$StoreNo %in% InternalList | storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(AnyDeliveryAvailable) %>%
  summarise(avgDelivery = mean(totalDelivery), count = n_distinct(StoreNo))

actualSalesIncreaseAny<- storeWeek[storeWeek$StoreNo %in% InternalList | storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(AnyDeliveryAvailable, marketStatus) %>%
  summarise(totalTransactions = totalDelivery = sum(totalDeliveryTransactions), count = n_distinct(StoreNo))

actualSalesIncreaseBoth<- storeWeek[storeWeek$StoreNo %in% InternalList & storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(DoorDashDeliveryAvailable, InternalDeliveryAvailable,marketStatus) %>%
  summarise(avgSales = mean(TotalItemSales), count = n_distinct(StoreNo))

actualSalesIncreaseDoorDash<- storeWeek[!storeWeek$StoreNo %in% InternalList & storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(DoorDashDeliveryAvailable, InternalDeliveryAvailable,marketStatus) %>%
  summarise(avgSales = mean(TotalItemSales), count = n_distinct(StoreNo))

actualSalesIncreaseInternal<- storeWeek[storeWeek$StoreNo %in% InternalList & !storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(DoorDashDeliveryAvailable, InternalDeliveryAvailable,marketStatus) %>%
  summarise(avgSales = mean(TotalItemSales), count = n_distinct(StoreNo))

DeliverySalesIncreaseBoth<- storeWeek[storeWeek$StoreNo %in% InternalList & storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(DoorDashDeliveryAvailable, InternalDeliveryAvailable,marketStatus) %>%
  summarise(avgSales = mean(TotalItemSales), avgDelivery = mean(DeliverySales), avgDoorDashDelivery = mean(DoorDashDeliverySales),count = n_distinct(StoreNo))

DeliverySalesIncreaseDoorDash<- storeWeek[!storeWeek$StoreNo %in% InternalList & storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(DoorDashDeliveryAvailable, InternalDeliveryAvailable,marketStatus) %>%
  summarise(avgSales = mean(TotalItemSales), avgDelivery = mean(DoorDashDeliverySales), count = n_distinct(StoreNo))

DeliveryIncreaseInternal<- storeWeek[storeWeek$StoreNo %in% InternalList & !storeWeek$StoreNo %in% DoorDashList,]%>%
  group_by(DoorDashDeliveryAvailable, InternalDeliveryAvailable,marketStatus) %>%
  summarise(avgSales = mean(TotalItemSales), avgDelivery = mean(DeliverySales), count = n_distinct(StoreNo))

Totalstoregrowth<- storeWeek%>%
  group_by(year,marketStatus) %>%
  summarise(avgSales = mean(TotalItemSales), avgDelivery = mean(DeliverySales), count = n_distinct(StoreNo))


storeWeek$DeliveryWave<-"never"
storeWeek$DeliveryWave[storeWeek$FirstDeliveryDate <=71]<-"first"
storeWeek$DeliveryWave[storeWeek$FirstDeliveryDate >71]<-"second"
table(storeWeek$DeliveryWave)
deliverybystartdate<-storeWeek%>%
  group_by(DeliveryWave, week) %>%
  summarise(totalDDdelivery = sum(DDDeliverySales), totalDoorDashdelivery = sum(DoorDashDeliverySales), totaldelivery = sum(DeliverySales))

write.csv(deliverybystartdate, "delivery by wave v2.csv")

deliverybystartdate<-storeWeek%>%
  group_by(DeliveryWave, week) %>%
  summarise(DDdelivery = mean(DDDeliverySales), DoorDashdelivery = mean(DoorDashDeliverySales), Dispatchdelivery = mean(DeliverySales))

write.csv(deliverybystartdate, "avg delivery by wave.csv")
deliverybystartdate<-storeWeek%>%
  group_by(week) %>%
  summarise(totalDDdelivery = sum(DDDeliverySales), totalDoorDashdelivery = sum(DoorDashDeliverySales), totaldelivery = sum(DeliverySales))

write.csv(deliverybystartdate, "total delivery per week.csv")

anyDelivery<-storeWeek[storeWeek$AnyDeliveryAvailable == 1, ]

deliverybystartdate<-anyDelivery%>%
  group_by(week) %>%
  summarise(avgDDdelivery = mean(DDDeliverySales), avgDoorDashdelivery = mean(DoorDashDeliverySales), avgdelivery = mean(DeliverySales))

write.csv(deliverybystartdate, "avg delivery per week.csv")



####OSAT by DMA
osat<-storeWeek%>%
  group_by(DMA_NAME, marketStatus) %>%
  summarise(avgOSAT = mean(OverallSatisfaction), avgStoreSales = mean(TotalItemSales))


ggplot(osat, aes(x = avgOSAT, y = avgStoreSales, color = marketStatus)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) +
  ggtitle("Average Satisaction and Store Sales by DMA")+xlab("Average Overall Satisfaction Score")+ylab("Average Weekly Sales by Store")+
  theme(
    plot.title = element_text(hjust=.5)
  )

storeWeek$OSATMinusDMAavgOSAT<-(storeWeek$OverallSatisfaction - storeWeek$DMAavgOSAT)*100

Salesfit<-lm((storeWeek$TotalItemSales)~
               storeWeek$ageOfStoreInWeeks+
               storeWeek$newStore30weeks + 
               storeWeek$ageOfStoreInWeeks*storeWeek$newStore30weeks+
               storeWeek$quintile2 + 
               storeWeek$quintile3 + 
               storeWeek$quintile4 + 
               storeWeek$quintile5 + 
               #storeWeek$DMAstorecount+
               #storeWeek$app1+
               #storeWeek$app2+
               #storeWeek$app3+
               #storeWeek$app4+
               #storeWeek$app5+
               #storeWeek$web1+
               #storeWeek$web2+
               #storeWeek$web3+
               #storeWeek$web4+
               #storeWeek$web5+
               #storeWeek$promo1+
               #storeWeek$promo2+
               #storeWeek$promo3+
               #storeWeek$promo4+
               #storeWeek$promo5+
               #storeWeek$braziliancitruspepper+
               #storeWeek$spicykoreanq+
               storeWeek$samplingEvent+
               storeWeek$raining_float+
               storeWeek$snowing_float+
               storeWeek$DMAavgOSAT+
               storeWeek$OSATMinusDMAavgOSAT+
               #storeWeek$OverallSatisfaction*storeWeek$marketStatusDeveloping+
               #storeWeek$OverallSatisfaction*storeWeek$marketStatusEmerging+
               #storeWeek$OSATBin40+
               storeWeek$wingstops_within_3_miles+
               storeWeek$Buffalo_Wild_Wings_within_3_miles+
               storeWeek$Chick_Fil_A_within_3_miles+
               storeWeek$Chipotle_within_3_miles+
               storeWeek$KFC_within_3_miles+
               storeWeek$Panda_Express_within_3_miles+
               storeWeek$Pizza_Hut_within_3_miles+
               storeWeek$Taco_Bell_within_3_miles+
               storeWeek$Whataburger_within_3_miles+
               storeWeek$Dominos_within_3_miles+
               storeWeek$unemploymentRate+
               storeWeek$hotDay_float+
               storeWeek$coldDay_float+
               storeWeek$spring_float+
               storeWeek$fall_float+
               storeWeek$winter_float+
               storeWeek$college_football_game_float+
               storeWeek$college_football_playoffs+
               storeWeek$cultural_holiday+
               storeWeek$federal_holiday+
               storeWeek$four_twenty+
               storeWeek$march_madness_float+
               storeWeek$nfl_game_float+
               storeWeek$payday_calendar_float+
               storeWeek$superbowl+
               storeWeek$UFC_fight_float+
               storeWeek$christmas+
               storeWeek$easter+
               storeWeek$thanksgiving+
               storeWeek$marketStatusEmerging+
               storeWeek$marketStatusDeveloping+
               storeWeek$householdsPerStore+
               
               storeWeek$OLOSocialSpendTruncated + 
               storeWeek$OLOSocialSpendTruncatedlag1 + 
               storeWeek$OLOSearchSpendTruncated +
               storeWeek$OLOSearchSpendTruncatedlag1 +
               storeWeek$OLODisplaySpendTruncated +
               storeWeek$OLODisplaySpendTruncatedlag1 +
               storeWeek$OLVSocialSpendTruncatedDiscounted +
               storeWeek$OLVNationalSpendTruncatedDiscounted +
               storeWeek$tvspendTruncatedDiscounted +
               
               storeWeek$localoohSpendDiscounted +
               storeWeek$localsponsorsSpendDiscounted +
               storeWeek$localsocialSpendDiscounted +
               storeWeek$localdigitalSpendDiscounted +
               storeWeek$localradioSpendDiscounted +
               storeWeek$localotherSpendDiscounted +
               storeWeek$localtvSpendDiscounted +
               storeWeek$localmobileSpendDiscounted +
               
               
               # storeWeek$OLOSocialSpend + 
               # storeWeek$OLOSocialSpendlag1 + 
               # storeWeek$OLOSearchSpendTruncated +
               # storeWeek$OLOSearchSpendlag1Truncated +
               # storeWeek$OLODisplaySpend +
               # storeWeek$OLODisplaySpendlag1 +
               # storeWeek$OLVSocialSpendDiscounted +
               # storeWeek$OLVNationalSpendDiscounted +
               # storeWeek$tvspendDiscounted +
               # 
               # storeWeek$localoohSpendDiscounted +
               # storeWeek$localsponsorsSpendDiscounted +
               # storeWeek$localsocialSpendDiscounted +
               # storeWeek$localdigitalSpendDiscounted +
               # storeWeek$localradioSpendDiscounted +
               # storeWeek$localotherSpendDiscounted +
               # storeWeek$localtvSpendDiscounted +
               # storeWeek$localmobileSpendDiscounted +
             
             storeWeek$DoorDashDeliveryAvailable +
               storeWeek$marketStatusEmerging * storeWeek$DoorDashDeliveryAvailable +
               storeWeek$marketStatusDeveloping* storeWeek$DoorDashDeliveryAvailable + 
               
               storeWeek$DDDeliveryAvailable +
               storeWeek$marketStatusEmerging * storeWeek$DDDeliveryAvailable +
               storeWeek$marketStatusDeveloping* storeWeek$DDDeliveryAvailable + 
               
               storeWeek$InternalDeliveryAvailable +
               storeWeek$marketStatusEmerging * storeWeek$InternalDeliveryAvailable +
               storeWeek$marketStatusDeveloping* storeWeek$InternalDeliveryAvailable 
)

summary(Salesfit)
