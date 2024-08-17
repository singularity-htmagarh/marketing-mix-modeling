
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
WHERE
marketStatus = "Emerging"
                     
                     GROUP BY
                     WeeksFromDelivery
                     ;')

DeliverySales$other<-1 - DeliverySales$webMobilepct - DeliverySales$apppct - DeliverySales$DoorDashDeliverypct - DeliverySales$DDDeliverypct -DeliverySales$Deliverypct -
  DeliverySales$WalkDineInpct -
  DeliverySales$WalkToGopct - DeliverySales$CallDineInpct - DeliverySales$CallToGopct

DeliverySales$otherTransactions<-1 - DeliverySales$webMobileTransactionspct - DeliverySales$appTransactionspct - DeliverySales$DeliveryTransactionspct - DeliverySales$DDDeliveryTransactionspct - DeliverySales$DoorDashDeliveryTransactionspct - DeliverySales$WalkDineInTransactionspct -
  DeliverySales$WalkToGoTransactionspct - DeliverySales$CallDineInTransactionspct - DeliverySales$CallToGoTransactionspct

mdata <- melt(DeliverySales[ , c("WeeksFromDelivery", "Deliverypct","DDDeliverypct","DoorDashDeliverypct","other", "webMobilepct", 
                                 "apppct",  "CallToGopct", "CallDineInpct", "WalkToGopct", "WalkDineInpct")], id=c("WeeksFromDelivery"))

ggplot(mdata, aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Emerging Sales at introduction of any Delivery")

mdata <- melt(DeliverySales[ , c("WeeksFromDelivery", "DeliveryTransactionspct","DDDeliveryTransactionspct","DoorDashDeliveryTransactionspct", "otherTransactions", "webMobileTransactionspct", 
                                 "appTransactionspct",  "CallToGoTransactionspct", "CallDineInTransactionspct", "WalkToGoTransactionspct", "WalkDineInTransactionspct")], id=c("WeeksFromDelivery"))

ggplot(mdata, aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Emerging Sales at Introduction of Any Delivery")


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
                     
WHERE
marketStatus = "Developing"
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
write.csv(t(DeliverySales2), "developing prepost v2.csv")



###incrementality by duration of delivery

deliveryList<-unique(storeDay$StoreNo[storeDay$AnyDeliveryAvailable == 1])
deliverystores<-storeDay[storeDay$StoreNo %in% deliveryList, ]
deliverystores$fromDeliveryTime<-as.Date(deliverystores$date) - as.Date(deliverystores$FirstDeliveryDate.x)
deliverystores$WeeksFromDelivery<-as.numeric(floor(deliverystores$fromDeliveryTime/7))
deliverystores$DeliveryWave<-2019
deliverystores$DeliveryWave[as.Date(deliverystores$FirstDeliveryDate.x) < as.Date("2019-01-01")]<-2018
deliverystores$DeliveryWave[as.Date(deliverystores$FirstDeliveryDate.x) < as.Date("2018-01-01")]<-2017
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
                     WHERE
                     DeliveryWave = 2017 and WeeksFromDelivery<=150
                     
                     GROUP BY
                     WeeksFromDelivery
                     ;')

DeliverySales$other<-1 - DeliverySales$webMobilepct - DeliverySales$apppct - DeliverySales$DoorDashDeliverypct - DeliverySales$DDDeliverypct -DeliverySales$Deliverypct -
  DeliverySales$WalkDineInpct -
  DeliverySales$WalkToGopct - DeliverySales$CallDineInpct - DeliverySales$CallToGopct

DeliverySales$otherTransactions<-1 - DeliverySales$webMobileTransactionspct - DeliverySales$appTransactionspct - DeliverySales$DeliveryTransactionspct - DeliverySales$DDDeliveryTransactionspct - DeliverySales$DoorDashDeliveryTransactionspct - DeliverySales$WalkDineInTransactionspct -
  DeliverySales$WalkToGoTransactionspct - DeliverySales$CallDineInTransactionspct - DeliverySales$CallToGoTransactionspct

mdata <- melt(DeliverySales[ , c("WeeksFromDelivery", "Deliverypct","DDDeliverypct","DoorDashDeliverypct","other", "webMobilepct", 
                                 "apppct",  "CallToGopct", "CallDineInpct", "WalkToGopct", "WalkDineInpct")], id=c("WeeksFromDelivery"))

ggplot(mdata, aes(x=WeeksFromDelivery, y=value, fill=variable)) + 
  geom_area() + ggtitle("Earliest Deliverers Sales at introduction of Delivery")


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
                     
                     WHERE
                     DeliveryWave = 2017 and WeeksFromDelivery<=150
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
write.csv(t(DeliverySales2), "2017 prepost v2.csv")
