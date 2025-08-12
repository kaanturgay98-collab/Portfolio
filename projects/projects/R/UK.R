#UK

setwd("/Users/FedericoCasorati/Desktop/APM/2024:25/Project work/Data")
library(dplyr)
library(tidyr)

#Import data:
uk.data=read.csv("UnitedKingdom.csv")
#Let's take a look at the different covariates:
str(uk.data)
head(uk.data)

#We have to use Category.Global in order to differentiate among the different products
unique(uk.data$Category.Global)
#I want to know in the uk.data how many observations are there for each category of products
table(uk.data$Category.Local)
table(uk.data$Category.Global)

#Let's see which are the manufacturers (competitors). We are still using the .Global value

unique(uk.data$Manufacturer.Global)
unique(uk.data$Manufacturer.Local)

#These commands help us re-ordinating the data for week, month and year. We want them in chronological order.
uk.data.campari <- uk.data[uk.data$Manufacturer.Global == "Campari Group", ]

uk.data.campari <- uk.data.campari %>%
  arrange(Year, Month, Week)


uk.data.comp<- uk.data[uk.data$Manufacturer.Global != "Campari Group", ]
uk.data.comp <- uk.data.comp %>%
  arrange(Year, Month, Week)

#We notice a problem: the observations of week 52 are categorized as part of month 1. Let's change that and let's 
#re-ordinate the observations again (repeat the previous commands). We assume that this issue comes up because
#the year is made by 52.something weeks, so those days after the 52nd week are considered part of the year after,
#but they are actually part of the year before
uk.data.campari <- uk.data.campari %>%
  mutate(
    Year = ifelse(Week == 52 & Month == 1, Year - 1, Year),
    Month = ifelse(Week == 52 & Month == 1, 12, Month)
  ) %>%
  arrange(Year, Month, Week)

uk.data.comp <- uk.data.comp %>%
  mutate(
    Year = ifelse(Week == 52 & Month == 1, Year - 1, Year),
    Month = ifelse(Week == 52 & Month == 1, 12, Month)
  ) %>%
  arrange(Year, Month, Week)

#Let's solve the null values issue
colSums(uk.data.campari == "null") #for Campari there are no null values in Category.Global
colSums(uk.data.campari == "NA") #for Campari there are no NAs in Category.Global
colSums(uk.data.campari == "") #for Campari there are no null values in Category.Global
colSums(uk.data.campari == "Not Available") #for Campari there are no NAs in Category.Global

colSums(uk.data.comp == "null") #for competitors there are no null values in Category.Global
colSums(uk.data.comp == "") #for competitors there are 68 null values in Category.Global
colSums(uk.data.comp == "NA") #for competitors there are no NAs in Category.Global
colSums(uk.data.comp == "Not Available") #for competitors there are no NAs in Category.Global

#Let's solve the "" issue

#We can notice that the only observations where Category.Global is null, have Category.Local=="TOTAL LIQUEURS & 
#SPECIALITIES" and "TOTAL RUM"
unique(uk.data.comp$Category.Local[uk.data.comp$Category.Global==""])

#So, we can only focus on those two and assign the correspondent Category.Global
uk.data.comp <- uk.data.comp %>%
  mutate(
    Category.Global = if_else(
      Category.Global == "", 
      case_when(
        Category.Local == "TOTAL LIQUEURS & SPECIALITIES" ~ "Liqueur",
        Category.Local == "TOTAL RUM" ~ "Rum",
        TRUE ~ NA_character_ 
      ),
      Category.Global
    )
  )


colSums(uk.data.comp == "") #now for competitors there are 0 null values in Category.Global


#Let's see now which are the categories of products that Campari sells in UK
unique(uk.data.campari$Category.Global)
table(uk.data.campari$Category.Global)

#Now let's group for product type.
uk.data_ape.campari <- uk.data.campari[uk.data.campari$Category.Global == "Aperitif", ]
uk.data_brandy.campari <- uk.data.campari[uk.data.campari$Category.Global == "Brandy & Cognac", ]
uk.data_gin.campari <- uk.data.campari[uk.data.campari$Category.Global == "Gin & Genever", ]
uk.data_hb.campari <- uk.data.campari[uk.data.campari$Category.Global == "Herbal Bitter", ]
uk.data_lq.campari <- uk.data.campari[uk.data.campari$Category.Global=="Liqueur",]
uk.data_rum.campari <- uk.data.campari[uk.data.campari$Category.Global == "Rum", ]
uk.data_teq.campari <- uk.data.campari[uk.data.campari$Category.Global == "Tequila & Mezcal", ]
uk.data_whisky.campari <- uk.data.campari[uk.data.campari$Category.Global=="Whisky",]
#Same for competitors
uk.data_ape.comp <- uk.data.comp[uk.data.comp$Category.Global == "Aperitif", ]
uk.data_brandy.comp <- uk.data.comp[uk.data.comp$Category.Global == "Brandy & Cognac", ]
uk.data_gin.comp <- uk.data.comp[uk.data.comp$Category.Global == "Gin & Genever", ]
uk.data_hb.comp <- uk.data.comp[uk.data.comp$Category.Global == "Herbal Bitter", ]
uk.data_lq.comp <- uk.data.comp[uk.data.comp$Category.Global=="Liqueur",]
uk.data_rum.comp <- uk.data.comp[uk.data.comp$Category.Global == "Rum", ]
uk.data_teq.comp <- uk.data.comp[uk.data.comp$Category.Global == "Tequila & Mezcal", ]
uk.data_whisky.comp <- uk.data.comp[uk.data.comp$Category.Global=="Whisky",]

#Let's create some datasets with the sales values for every product (for both Campari and competitors)

campari_ape_summary_uk<- uk.data_ape.campari %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_ape_summary_uk <- uk.data_ape.comp %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_brandy_summary_uk<- uk.data_brandy.campari %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_brandy_summary_uk <- uk.data_brandy.comp %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_gin_summary_uk <- uk.data_gin.campari %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_gin_summary_uk <- uk.data_gin.comp %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_hb_summary_uk <- uk.data_hb.campari %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

#campari_hb_summary_uk$X2020=0
#campari_hb_summary_uk$X2022=0
#campari_hb_summary_uk$X2024=0
#campari_hb_summary_uk <- campari_hb_summary_uk[, order(names(campari_hb_summary_uk))]

comp_hb_summary_uk <- uk.data_hb.comp %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_lq_summary_uk <- uk.data_lq.campari %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_lq_summary_uk <- uk.data_lq.comp %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_rum_summary_uk <- uk.data_rum.campari %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_rum_summary_uk <- uk.data_rum.comp %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_teq_summary_uk <- uk.data_teq.campari %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_teq_summary_uk <- uk.data_teq.comp %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_whisky_summary_uk <- uk.data_whisky.campari %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_whisky_summary_uk <- uk.data_whisky.comp %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")


#Now let's merge them into two datasets (one for Campari, one for competitors)
sales_campari_uk=rbind(campari_ape_summary_uk, campari_brandy_summary_uk, campari_gin_summary_uk, campari_hb_summary_uk, 
                       campari_lq_summary_uk, campari_rum_summary_uk, campari_teq_summary_uk, campari_whisky_summary_uk)
sales_comp_uk=rbind(comp_ape_summary_uk, comp_brandy_summary_uk, comp_gin_summary_uk, comp_hb_summary_uk, comp_lq_summary_uk, 
                    comp_rum_summary_uk, comp_teq_summary_uk, comp_whisky_summary_uk)

#To make it more tidy, let's create the column for product name
sales_campari_uk$Name_Product=c("Aperitif", "Brandy", "Gin", "Herbal Bitter", "Liqueurs and Spirits", "Rum", "Tequila", "Whisky")
sales_comp_uk$Name_Product=c("Aperitif", "Brandy", "Gin", "Herbal Bitter", "Liqueurs and Spirits", "Rum", "Tequila", "Whisky")

#Let's put the column with the names as the first one
sales_campari_uk <- sales_campari_uk %>%
  select(ncol(sales_campari_uk), everything())
sales_comp_uk <- sales_comp_uk %>%
  select(ncol(sales_comp_uk), everything())

# We can extract years and sales data
years_uk <- as.numeric(sub("X", "", colnames(sales_campari_uk)))  
sales_data_uk_campari <- as.matrix(sales_campari_uk)  

years_uk <- as.numeric(sub("X", "", colnames(sales_comp_uk)))  
sales_data_uk_comp <- as.matrix(sales_comp_uk)  

colori_uk = rainbow(length(unique(sales_comp_uk$Name_Product)))

#We can plot some charts that help us visualize which are the most important products for Campari
x11()
par(mfrow=c(1,2))
matplot(years_uk, t(sales_data_uk_campari), type = "l", lty = 1, lwd = 2, col = colori_uk, 
        xlab = "Year", ylab = "Sales Value", main = "United Kingdom Campari Sales")

# Add a legend
legend("topright", legend = sales_campari_uk$Name_Product, col = colori_uk, lty = 1, lwd = 2)

matplot(years_uk, t(sales_data_uk_comp), type = "l", lty = 1, lwd = 2, col = colori_uk, 
        xlab = "Year", ylab = "Sales Value", main = "United Kingdom Competitors Sales")

# Add a legend
legend("topright", legend = sales_comp_uk$Name_Product, col = colori_uk, lty = 1, lwd = 2)

#Brandy and Aperitif are the top 2 products for Campari in terms of sales.
#Whisky and Gin the two most sold products among competitors, so the ones with the highest demand: Campari could
#penetrate those markets and become a big player.

#Now let's do a graphical vertical analysis of the revenues per product with barplots

x11()
par(mfrow=c(1,2))
barplot(sales_data_uk_campari[,-1], col = colori_uk, main="Campari Vertical Analysis United Kingdom")
legend("topright",legend = sales_campari_uk$Name_Product, col = colori_uk, lty = 1, lwd = 2, cex = 0.5)
barplot(sales_data_uk_comp[,-1], col = colori_uk, main="Competitors Vertical Analysis United Kingdom")
legend("topright",legend = sales_comp_uk$Name_Product, col = colori_uk, lty = 1, lwd = 2, cex = 0.5)

#This chart is similar to the previous one but it only shows last year's sales
#----------------------------------------------------------------------------------------------------------------


#Now let's re-create the big dataset (it will be useful for PowerBI dashboard, since we will need the three datasets altogether)
uk.data=rbind(uk.data.campari, uk.data.comp)
uk.data$Country<-"United Kingdom"


#In order to see the competition in a certain market, let's calculate the market shares and the concentration
market_share_uk <- uk.data %>%
  group_by(Manufacturer.Global) %>%        
  summarize(Total.Sales = sum(Sales.Value, na.rm = TRUE)) %>%  
  mutate(Market.Share = (Total.Sales / sum(Total.Sales)) * 100,
         Market.Share = format(round(Market.Share, 4), nsmall = 4))%>%  
  arrange(desc(Market.Share))

market_share_uk$Market.Share=as.numeric(market_share_uk$Market.Share)

#We try to calculate the C4 but we don't consider Private Label and Minor Manufacturers.
#Minor Manufacturers are smaller than the ones that are listed, so they can't be among the biggest competitors.
#Since C4 aims at determining the concentration among the biggest branded manufacturers, we decide to exclude 
#Private Labels, since it may also be the aggregated result of smaller manufacturers supplying for different supermarkets 
sum(market_share_uk$Market.Share[c(1,2,4,5)])

#Here we notice a C4 of 55.416%. We suppose none of the Minor Manufacturers are in the top10 of competitors

calculate_market_share <- function(uk.data) {
  uk.data %>%
    group_by(Category.Global, Manufacturer.Global) %>%
    summarise(Total_Sales = sum(Sales.Value, na.rm = TRUE), .groups = "drop") %>%
    group_by(Category.Global) %>%
    mutate(Market_Share_Percentage = (Total_Sales / sum(Total_Sales)) * 100,
           Market_Share_Percentage = format(round(Market_Share_Percentage, 4), nsmall = 4))%>%
    arrange(Category.Global, desc(Market_Share_Percentage)) # Sorted by category and market share
}

market_share_result_uk <- calculate_market_share(uk.data)


#Campari is the leader in the Aperitif segment, and the runner-up in the Brandy segment. Whisky is the least
#concentrated market among the others.

