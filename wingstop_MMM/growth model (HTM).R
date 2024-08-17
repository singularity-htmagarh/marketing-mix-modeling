
storeWeekFinal <- read.csv("C:/Users/hthapa/OneDrive - Epsilon/Projects/Wingstop MMM/RawData/StoreWeekDataset.csv", stringsAsFactors = FALSE)

storeMonth<-sqldf('
                 SELECT
                 StoreNo
                 , marketStatus
                 , DMA_NAME
                 , month
                 , year
                 , max(quintile2) as quintile2
                 , max(quintile3) as quintile3
                 , max(quintile4) as quintile4
                 , max(quintile5) as quintile5
                 , avg(samplingEvent) as samplingEvent
                 , min(date) as WeekOf
                 , max(ageOfStoreInWeeks) as ageOfStoreInWeeks
                 , max(newStore30weeks) as newStore30weeks
                 , max(DMAStoreCount) as DMAStoreCount
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
                 
                 , sum(TotalItemSales) as TotalItemSales
                 , sum(totalDelivery) as totalDelivery
                 
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
                 storeWeekFinal as s
                 GROUP BY
                 StoreNo
                 ,marketStatus
                 ,DMA_NAME
                 ,month
                 ,year                 
                 ;')



nationalMonth<-storeMonth%>%
  group_by(month, year)%>%
  summarise(TransactionCount = sum(TransactionCount, na.rm = TRUE), storeWeeks = n_distinct(StoreNo), adSpend = sum(AdSpend), totalSales = sum(TotalItemSales, na.rm = TRUE)) %>%
  ungroup()

nationalMonth$avgSales<-nationalMonth$totalSales/nationalMonth$storeWeeks
nationalMonth$avgTransactionCount<-nationalMonth$TransactionCount/nationalMonth$storeWeeks
nationalMonth$avgTransactionSize<-nationalMonth$totalSales/nationalMonth$TransactionCount
nationalMonth<-nationalMonth[order(nationalMonth$year, nationalMonth$month), ]

nationalMonth<- nationalMonth %>%
  mutate(lastMonthAdSpend = dplyr::lag(adSpend, n = 1, default = NA), lastMonthSales = dplyr::lag(totalSales, n = 1, default = NA))

nationalMonth$growthRate<-(nationalMonth$totalSales - nationalMonth$lastMonthSales)/nationalMonth$lastMonthSales


nationalMonth<-nationalMonth[order(nationalMonth$year, nationalMonth$month), ]
nationalMonth<-nationalMonth[!is.na(nationalMonth$month),]
nationalWeekTS <- ts(nationalMonth$avgSales, frequency = 12)
stl_national = stl(nationalWeekTS, "periodic")
plot(stl_national)

p<-ggplot(nationalMonth, aes(x = adSpend, y = avgSales)) + geom_point()
p

nationalWeekTS <- ts(nationalMonth$growthRate[!is.na(nationalMonth$growthRate)], frequency = 12)
stl_national = stl(nationalWeekTS, "periodic")
plot(stl_national)

p<-ggplot(nationalMonth, aes(x = adSpend, y = avgSales)) + geom_point()
p


storeMonth<-storeMonth[order(storeMonth$year, storeMonth$month), ]
storeMonth<- storeMonth %>%
  group_by(StoreNo)%>%
  mutate(lastMonthAdSpend = dplyr::lag(AdSpend, n = 1, default = NA), lastMonthSales = dplyr::lag(TotalItemSales, n = 1, default = NA))

storeMonth$growthRate<-(storeMonth$TotalItemSales - storeMonth$lastMonthSales)/storeMonth$lastMonthSales

storeMonth<-storeMonth[storeMonth$growthRate<1, ]

storeMonth$marketStatusCore<-ifelse(storeMonth$marketStatus == "Core", 1, 0)
storeMonth$marketStatusDeveloping<-ifelse(storeMonth$marketStatus == "Developing", 1, 0)
storeMonth$marketStatusEmerging<-ifelse(storeMonth$marketStatus == "Emerging", 1, 0)




storeMonth<-storeMonth[order(storeMonth$StoreNo,storeMonth$week), ]
mediaVars<- c("OLOSocialSpend", "OLOSearchSpend" , "OLODisplaySpend", "OLVSocialSpend", "OLVNationalSpend", "DigitalSpend", "tvspend", "AdSpend","localsponsorsSpend","localsocialSpend","localdigitalSpend","localradioSpend","localoohSpend","localotherSpend","localtvSpend","localmobileSpend", "localAdSpend")

##truncate extreme media spend

for (med in mediaVars){
  eval(parse(text = paste0("storeMonth$",med,"Truncated<-storeMonth$",med)))
  eval(parse(text = paste0("storeMonth$",med,"[storeMonth$",med," > mean(storeMonth$",med,") + 2 * sd(storeMonth$",med,")]<-mean(storeMonth$",med,") + 2 * sd(storeMonth$",med,")")))
}


mediaVars<- c("OLOSocialSpend", "OLOSearchSpend" , "OLODisplaySpend", "OLVSocialSpend", "OLVNationalSpend", "DigitalSpend", "tvspend", "AdSpend",
              "localsponsorsSpend","localsocialSpend","localdigitalSpend","localradioSpend","localoohSpend","localotherSpend","localtvSpend","localmobileSpend", "localAdSpend",
              "OLOSocialSpendTruncated", "OLOSearchSpendTruncated" , "OLODisplaySpendTruncated", "OLVSocialSpendTruncated", "OLVNationalSpendTruncated", "DigitalSpendTruncated", "tvspendTruncated", "AdSpendTruncated",
              "localsponsorsSpendTruncated","localsocialSpendTruncated","localdigitalSpendTruncated","localradioSpendTruncated","localoohSpendTruncated","localotherSpendTruncated","localtvSpendTruncated","localmobileSpendTruncated", "localAdSpendTruncated")
for (med in mediaVars){
  for (i in 1:8){
    text<-paste0("storeMonth<- storeMonth %>%
                 group_by(StoreNo)%>%
                 mutate(",med, "lag", i, " = dplyr::lag(", med, ", n = ", i, ", default = NA))")
    eval(parse(text = text))
  }
}

storeMonth$marketStatusCore<-ifelse(storeMonth$marketStatus == "Core", 1, 0)
storeMonth$marketStatusDeveloping<-ifelse(storeMonth$marketStatus == "Developing", 1, 0)
storeMonth$marketStatusEmerging<-ifelse(storeMonth$marketStatus == "Emerging", 1, 0)

for (med in mediaVars){
  text<-paste0("storeMonth$", med, "Discounted<- storeMonth$", med, "+
               storeMonth$", med,"lag1 * 0.75+ 
               storeMonth$", med,"lag2 * 0.5+ 
               storeMonth$", med,"lag3 * 0.25 + 
               storeMonth$", med,"lag4 * 0.1 + 
               storeMonth$", med,"lag5 * 0.1 + 
               storeMonth$", med,"lag6 * 0.05 + 
               storeMonth$", med,"lag7 * 0.05 + 
               storeMonth$", med,"lag8 * 0.05  ")
  print(text)             
  eval(parse(text = text))
  
}


Salesfit<-lm((storeMonth$growthRate)~
               storeMonth$ageOfStoreInWeeks+
               storeMonth$newStore30weeks + 
               storeMonth$ageOfStoreInWeeks*storeMonth$newStore30weeks+
               storeMonth$quintile2 + 
               storeMonth$quintile3 + 
               storeMonth$quintile4 + 
               storeMonth$quintile5 + 

               storeMonth$samplingEvent+

               storeMonth$OverallSatisfaction+
               #storeWeek$OSATBin40+
               storeMonth$wingstops_within_3_miles+
               storeMonth$Buffalo_Wild_Wings_within_3_miles+
               storeMonth$Chick_Fil_A_within_3_miles+
               storeMonth$Chipotle_within_3_miles+
               storeMonth$KFC_within_3_miles+
               storeMonth$Panda_Express_within_3_miles+
               storeMonth$Pizza_Hut_within_3_miles+
               storeMonth$Taco_Bell_within_3_miles+
               storeMonth$Whataburger_within_3_miles+
               storeMonth$Dominos_within_3_miles+
               storeMonth$hotDay_float+
               storeMonth$coldDay_float+
               storeMonth$spring_float+
               storeMonth$fall_float+
               storeMonth$winter_float+
               storeMonth$four_twenty+
               storeMonth$march_madness_float+
               

               storeMonth$superbowl+
              
               storeMonth$christmas+
               storeMonth$easter+
               storeMonth$thanksgiving+
               storeMonth$marketStatusEmerging+
               storeMonth$marketStatusDeveloping+
               storeMonth$householdsPerStore+
               
               storeMonth$OLOSocialSpendTruncated + 
               #storeMonth$OLOSocialSpendTruncatedlag1 + 
               storeMonth$OLOSearchSpendTruncated +
               #storeMonth$OLOSearchSpendTruncatedlag1 +
               storeMonth$OLODisplaySpendTruncated +
               #storeMonth$OLODisplaySpendTruncatedlag1 +
               storeMonth$OLVSocialSpendTruncated +
               storeMonth$OLVSocialSpendTruncatedlag1 +
               storeMonth$OLVNationalSpendTruncated +
               storeMonth$OLVNationalSpendTruncatedlag1 +
               storeMonth$tvspendTruncated +
               storeMonth$tvspendTruncatedlag1 +
      
               
               storeMonth$localoohSpend +
               storeMonth$localsponsorsSpend +
               storeMonth$localsocialSpend +
               storeMonth$localdigitalSpend +
               storeMonth$localradioSpend +
               storeMonth$localotherSpend+
               storeMonth$localtvSpend +
               storeMonth$localmobileSpend +
               
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
             
             storeMonth$DoorDashDeliveryAvailable +
               storeMonth$marketStatusEmerging * storeMonth$DoorDashDeliveryAvailable +
               storeMonth$marketStatusDeveloping* storeMonth$DoorDashDeliveryAvailable + 
               
               storeMonth$DDDeliveryAvailable +
               storeMonth$marketStatusEmerging * storeMonth$DDDeliveryAvailable +
               storeMonth$marketStatusDeveloping* storeMonth$DDDeliveryAvailable + 
               
               storeMonth$InternalDeliveryAvailable +
               storeMonth$marketStatusEmerging * storeMonth$InternalDeliveryAvailable +
               storeMonth$marketStatusDeveloping* storeMonth$InternalDeliveryAvailable 
)

summary(Salesfit)


####waterfall
WaterfallOutput<-NULL
SalesCoefs<-coef(Salesfit)
for (var in 1:length(names(SalesCoefs))){
  if (var == 1){
    outValueFirst52 <- SalesCoefs[1] * nrow(First52Weeks)
    OutValueLast52 <- SalesCoefs[1] * nrow(Last52Weeks)
    OutValueJanMay2018 <- SalesCoefs[1] * nrow(JanMay2018)
    OutValueJanMay2019 <- SalesCoefs[1] * nrow(JanMay2019)
    fullContribution<-SalesCoefs[1] * nrow(storeMonth)
    WaterfallOutput<-rbind(WaterfallOutput, c("Intercept", SalesCoefs[1], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019, fullContribution))
    
  } else{
    thisVar<-gsub("storeMonth\\$","",  names(SalesCoefs)[var])
    theseVars<-str_split_fixed(thisVar, ":", 2)
    if (theseVars[ , 2] == ""){
      outValueFirst52 <- SalesCoefs[var] * sum(First52Weeks[ , thisVar])
      OutValueLast52 <- SalesCoefs[var] * sum(Last52Weeks[ , thisVar])
      OutValueJanMay2018 <- SalesCoefs[var] * sum(JanMay2018[ , thisVar])
      OutValueJanMay2019 <- SalesCoefs[var] * sum(JanMay2019[ , thisVar])
      fullContribution<-SalesCoefs[var] *  sum(storeMonth[ , thisVar], na.rm = TRUE)
    } else {
      outValueFirst52 <- SalesCoefs[var] * sum(First52Weeks[ , theseVars[ , 1]] * First52Weeks[ , theseVars[ , 2]])
      OutValueLast52 <- SalesCoefs[var] * sum(Last52Weeks[ , theseVars[ , 1]] * Last52Weeks[ , theseVars[ , 2]])
      OutValueJanMay2018 <- SalesCoefs[var] * sum(JanMay2018[ , theseVars[ , 1]] * JanMay2018[ , theseVars[ , 2]])
      OutValueJanMay2019 <- SalesCoefs[var] * sum(JanMay2019[ , theseVars[ , 1]] * JanMay2019[ , theseVars[ , 2]])
      fullContribution <- SalesCoefs[var] * sum(storeMonth[ , theseVars[ , 1]] * storeMonth[ , theseVars[ , 2]])
    }
    
    
    WaterfallOutput<-rbind(WaterfallOutput, c(thisVar, SalesCoefs[var], outValueFirst52, OutValueLast52, OutValueJanMay2018, OutValueJanMay2019, fullContribution))
  }
}
write.csv(WaterfallOutput, "growth waterfall output full contribution.csv")


