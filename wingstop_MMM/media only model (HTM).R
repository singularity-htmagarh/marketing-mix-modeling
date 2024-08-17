

Salesfit<-lm((storeWeek$TotalItemSales)~
               storeWeek$quintile2 + 
               storeWeek$quintile3 + 
               storeWeek$quintile4 + 
               storeWeek$quintile5 + 
               
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
               storeWeek$localmobileSpendDiscounted 
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