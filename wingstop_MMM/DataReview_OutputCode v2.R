
## ====================================================================================
## Store Level Insights
## ====================================================================================

Store52week <- read.table("clipboard", sep="\t", header=TRUE)


Sum52x2017 <-sqldf('SELECT
                   WEEK
                  ,AVG(TotalItemSales) as Avg_Sales
                  ,AVG(TransactionCount) as TransCount
                  ,SUM(TotalItemSales) as Total_Sales
                   FROM storeWeek as sw
                   INNER JOIN Store52week as s52
                   ON sw.StoreNo = s52.Store_No
                   WHERE s52.x2017=1
                   GROUP BY
                   WEEK;')

Sum52x2018 <-sqldf('SELECT
                   WEEK
                  ,AVG(TotalItemSales) as Avg_Sales
                  ,AVG(TransactionCount) as TransCount
                  ,SUM(TotalItemSales) as Total_Sales
                   FROM storeWeek as sw
                   INNER JOIN Store52week as s52
                   ON sw.StoreNo = s52.Store_No
                   WHERE s52.x2018=1
                   GROUP BY
                   WEEK;')

storeDay<-read.csv("store by day dataset UPDATED.csv", stringsAsFactors = FALSE)

## ====================================================================================
## WHERE s52.X2017 = 1
## ====================================================================================

Store52week <- read.table("clipboard", sep="\t", header=TRUE)

colnames(Store52week)[4] = "x2018YTD"
colnames(Store52week)[5] = "x2019YTD"

Sum52x2018YTD <-sqldf('SELECT
                   WEEK
                  ,SUM(TotalItemSales) as TotalItemSales
                   FROM storeWeek as sw
                   INNER JOIN Store52week as s52
                   ON sw.StoreNo = s52.Store_No
                   WHERE s52.x2018YTD=1
                   GROUP BY
                   WEEK;')


NewStoreYEAR <- read.table("clipboard", sep="\t", header=TRUE)

Sum52x2018_New <-sqldf('SELECT
                   WEEK
                  
                    ,AVG(TotalItemSales) as Avg_Sales
                    ,AVG(TransactionCount) as TransCount
                    ,SUM(TotalItemSales) as TotalItemSales
 
                   FROM storeWeek as sw
                   INNER JOIN NewStoreYEAR as ns52
                   
                   ON sw.StoreNo = ns52.Store_No
                   
                   WHERE ns52.x2018 = 1
                   
                   GROUP BY
                   WEEK;')

### =========================================================================================================================

newStore30weeks <-sqldf('SELECT
                 YEAR, week,
                 newStore30weeks
                    ,AVG(TotalItemSales) as Avg_Sales
                    ,AVG(TransactionCount) as TransCount
                    ,SUM(TotalItemSales) as TotalItemSales
               
                   FROM storeWeek as sw
                    where newStore30weeks = 1
                    GROUP BY
                 YEAR, week, 
                 newStore30weeks;')

write.csv(newStore30weeks, "C:/Users/hthapa/OneDrive - Epsilon/Projects/Wingstop MMM/Modeling/Data Review/newStore30weeks.csv")

### =========================================================================================================================
## Delivery Calculation in Waterfall
### =========================================================================================================================

AnyDeliveryWaterfall <- sqldf('SELECT
                 week,
                 year
                    ,AVG(TotalItemSales) as Avg_Sales
                    ,AVG(TransactionCount) as TransCount
                    ,SUM(TotalItemSales) as TotalItemSales
               
                   FROM storeWeek as sw
                    WHERE AnyDeliveryAvailable = 1
                    GROUP BY
                 week,
                 year;')

write.csv(AnyDeliveryWaterfall, "C:/Users/hthapa/OneDrive - Epsilon/Projects/Wingstop MMM/Modeling/Data Review/AnyDeliveryWaterfall.csv")

### =========================================================================================================================


MediaSalesWaterfall <- sqldf('SELECT
                            year,
                            week,
                            sum(11.58763553*(CASE WHEN OLOSocialSpendTruncatedlag1>0 then OLOSocialSpendTruncatedlag1 else 0 end)
                            +-17.13286792*(CASE WHEN OLOSearchSpendTruncated>0 then OLOSearchSpendTruncated else 0 end)
                            +-39.82509719*(CASE WHEN OLOSearchSpendTruncatedlag1>0 then OLOSearchSpendTruncatedlag1 else 0 end)
                            +2.427081419*(CASE WHEN OLODisplaySpendTruncated>0 then OLODisplaySpendTruncated else 0 end)
                            +2.27426034*(CASE WHEN OLODisplaySpendTruncatedlag1>0 then OLODisplaySpendTruncatedlag1 else 0 end)
                            +-1.126792579*(CASE WHEN OLVSocialSpendTruncatedDiscounted>0 then OLVSocialSpendTruncatedDiscounted else 0 end)
                            +1.278054195*(CASE WHEN tvspendTruncatedDiscounted>0 then tvspendTruncatedDiscounted else 0 end)
                            +6.538900338*(CASE WHEN localoohSpendDiscounted>0 then localoohSpendDiscounted else 0 end)
                            +-2.383933937*(CASE WHEN localdigitalSpendDiscounted>0 then localdigitalSpendDiscounted else 0 end)
                            +1.41645168*(CASE WHEN localotherSpendDiscounted>0 then localotherSpendDiscounted else 0 end)
                            +-3.213403975*(CASE WHEN localtvSpendDiscounted>0 then localtvSpendDiscounted else 0 end))
                            as MediaDecomSales
        
                            FROM storeWeek as sw
                            GROUP BY
                            year, week;')

write.csv(MediaSalesWaterfall, "C:/Users/hthapa/OneDrive - Epsilon/Projects/Wingstop MMM/Modeling/Data Review/MediaSalesWaterfall.csv")




