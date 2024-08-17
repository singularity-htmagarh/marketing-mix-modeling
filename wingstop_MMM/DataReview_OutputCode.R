DMAStoreNo <- sqldf(' SELECT
                    DMA_NAME,StoreNo
                        from storeWeek
                    group by 
                    DMA_NAME
                    ,StoreNo;')


geohiertable <- read.csv("C:/Users/hthapa/OneDrive - Epsilon/Projects/Wingstop MMM/Modeling/geohiertable.csv")
prodhiertable <- read.csv("C:/Users/hthapa/OneDrive - Epsilon/Projects/Wingstop MMM/Modeling/prodhiertable.csv")
weekmap <- read.csv("C:/Users/hthapa/OneDrive - Epsilon/Projects/Wingstop MMM/Modeling/weekmap.csv")

MarketWeek<-sqldf('
                 SELECT
                  marketStatus
                 ,week
                 ,max(year) as year
                 ,max(ageOfStoreInWeeks) as ageOfStoreInWeeks
                 ,max(newStore30weeks) as newStore30weeks
                 ,max(DMAStoreCount) as DMAStoreCount
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
                 
                 ,cast(avg(cast(raining_float as float)) as float) as raining_float
                 ,cast(avg(cast(snowing_float as float)) as float) as snowing_float
                 
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
                 
                 FROM
                 storeWeek as s
                  GROUP BY
                 marketStatus ,week;')


NationalWeek  <- sqldf('
                 SELECT
                 "National" as marketStatus
                 ,week
                 ,max(year) as year
                 ,max(ageOfStoreInWeeks) as ageOfStoreInWeeks
                 ,max(newStore30weeks) as newStore30weeks
                 ,max(DMAStoreCount) as DMAStoreCount
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
                 
                 ,cast(avg(cast(raining_float as float)) as float) as raining_float
                 ,cast(avg(cast(snowing_float as float)) as float) as snowing_float
                 
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
                 
                 FROM
                 MarketWeek as s
                  GROUP BY week;')

DataReviewWeekly <- rbind(MarketWeek,NationalWeek)
  
DataReviewWeekly <- sqldf('
                 SELECT
                  dr.*, map.*
                  From DataReviewWeekly as dr inner join 
                  weekmap as map
                  on dr.week = map.weekNo;')

#"WINGSTOP" as BRAND, "WG" as BRAND_CODE %>% DataReviewWeekly

DataReviewWeekly <- DataReviewWeekly %>% mutate(BRAND="WINSTOP") %>% mutate(BRAND_CODE="WG")



write.csv(DataReviewWeekly,file = "C:/Users/hthapa/OneDrive - Epsilon/Projects/Wingstop MMM/Modeling/DataReviewWeeklyData.csv")                  
                  
