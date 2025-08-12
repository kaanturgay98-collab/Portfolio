
#Italy 

setwd("/Users/FedericoCasorati/Desktop/APM/2024:25/Project work/Data")
library(dplyr)
library(tidyr)
it.data=read.csv("Italy.csv")
str(it.data)
head(it.data)

colSums(is.na(it.data)) #this command allows me to find the number of NAs in each covariate
colSums(is.null(it.data))

#We realize that we have a lot of NAs in Sales.Value.No.Promo, so we need to fix the dataset. We look at Germany.csv and UK.csv to analyze
#when a period is defined as "no-promo", based on the value of sales baseline.

uk.data=read.csv("UnitedKingdom.csv")
ger.data=read.csv("Germany.csv")


#let's keep only the observations of the months when there are no promotions
uk.no.promo <- uk.data %>%
  filter(Sales.Value.No.Promo > 0) %>%
  select(Sales.Value, Sales.Value.No.Promo, Sales.Value.Baseline)

ger.no.promo <- ger.data %>%
  filter(Sales.Value.No.Promo > 0) %>%
  select(Sales.Value, Sales.Value.No.Promo, Sales.Value.Baseline)

#now let's calculate the ratios 

uk.no.promo$Ratio1 <- round(uk.no.promo$Sales.Value.Baseline / uk.no.promo$Sales.Value, 2)
uk.no.promo$Ratio2 <- round(uk.no.promo$Sales.Value.No.Promo / uk.no.promo$Sales.Value, 2)

ger.no.promo$Ratio1 <- round(ger.no.promo$Sales.Value.Baseline / ger.no.promo$Sales.Value, 2)
ger.no.promo$Ratio2 <- round(ger.no.promo$Sales.Value.No.Promo / ger.no.promo$Sales.Value, 2)

#let's plot the ratios so that we know where to fix the threshold (Sales.Baseline/Sales.Value)
#we see that some ratios are higher than 1, which doesn't make sense since it's difficult that
#the sales without promotion would be higher than the total sales, so we remove those observations

ratios_filtered_ger <- ger.no.promo$Ratio1[ger.no.promo$Ratio1 >= 0 & ger.no.promo$Ratio1 < 1]
ratios_filtered_uk <- uk.no.promo$Ratio1[uk.no.promo$Ratio1 >= 0 & uk.no.promo$Ratio1 <= 1]

# Plot the histograms
x11()
par(mfrow=c(1,2))
hist(ratios_filtered_ger, breaks = 50, col = "lightblue", border = "black",
     main = "Germany Ratios (0 to 1)",
     xlab = "Ratio1", ylab = "Frequency",
     xlim = c(0, 1))
hist(ratios_filtered_uk, breaks = 50, col = "lightblue", border = "black",
     main = "United Kingdom Ratios (0 to 1)",
     xlab = "Ratio1", ylab = "Frequency",
     xlim = c(0, 1))

#The UK results are not that useful, since we would have to put the threshold at 1.
#We can already see a trend with the German data and we can already figure out where to put the threshold.

#Now let's calculate the mode, just to be sure and to have a precise number

calculate_mode <- function(x, exclude_value = 1) {
  filtered_x <- x[x != exclude_value]
  uniq_x <- unique(filtered_x)  # Get unique values
  uniq_x[which.max(tabulate(match(filtered_x, uniq_x)))]  # Find the value with the highest frequency
}

# Apply the mode function to the Ratio1 column
mode_ratio1_ger <- calculate_mode(ger.no.promo$Ratio1, exclude_value = 1)

# Print the mode
print(paste("The mode of Ratio1 is:", mode_ratio1_ger))

# Apply the mode function to the Ratio1 column
mode_ratio1_uk <- calculate_mode(uk.no.promo$Ratio1)

# Print the mode
print(paste("The mode of Ratio1 is:", mode_ratio1_uk))

#0.86 seems more reasonable than 0.99, so we are going to put 0.86 as a threshold

it.data$Sales.Value.No.Promo <- ifelse(it.data$Sales.Value.Baseline / it.data$Sales.Value >= 0.86,
                                   it.data$Sales.Value, 0)
it.data$Sales.Volume.No.Promo <- ifelse(it.data$Sales.Value.Baseline / it.data$Sales.Value >= 0.86,
                                       it.data$Sales.Volume, 0)









#These commands help us re-ordinating the data for week, month and year. We want them in chronological order.
it.data.campari <- it.data[it.data$Manufacturer.Global == "Campari Group", ]

it.data.campari <- it.data.campari %>%
  arrange(Year, Month, Week)


it.data.comp<- it.data[it.data$Manufacturer.Global != "Campari Group", ]
it.data.comp <- it.data.comp %>%
  arrange(Year, Month, Week)

#We notice a problem: the observations of week 52 are categorized as part of month 1. Let's change that and let's 
#re-ordinate the observations again (repeat the previous commands). We assume that this issue comes up because
#the year is made by 52.something weeks, so those days after the 52nd week are considered part of the year after,
#but they are actually part of the year before

it.data.campari <- it.data.campari %>%
  mutate(
    Year = ifelse(Week == 52 & Month == 1, Year - 1, Year),
    Month = ifelse(Week == 52 & Month == 1, 12, Month)
  ) %>%
  arrange(Year, Month, Week)

it.data.comp <- it.data.comp %>%
  mutate(
    Year = ifelse(Week == 52 & Month == 1, Year - 1, Year),
    Month = ifelse(Week == 52 & Month == 1, 12, Month)
  ) %>%
  arrange(Year, Month, Week)



#Let's solve the null values issue, if there is one
colSums(it.data.campari == "null") #for Campari there are no null values in Category.Global/Manufacturer.Global
colSums(it.data.campari == "") #for Campari there are no null values in Category.Global/Manufacturer.Global
colSums(it.data.campari == "NA") #for Campari there are no NAs in Category.Global/Manufacturer.Global
colSums(it.data.campari == "Not Available") #for Campari there are no NA values in Category.Global/Manufacturer.Global

colSums(it.data.campari == "null") #for competitors there are no null values in Category.Global/Manufacturer.Global
colSums(it.data.comp == "") #for competitors there are 1894 null values in Category.Global/Manufacturer.Global
colSums(it.data.comp == "NA") #for competitors there are no NAs in Category.Global/Manufacturer.Global
colSums(it.data.comp == "Not Available") #for competitors there are no NAs in Category.Global/Manufacturer.Global

#Category global doesn't have empty data in the Italian dataset, unlike in the Germany and UK ones

#The only issue is with Manufacturer.Global that has some "" values.

#We need to solve this issue. 
#Since we need the competitors' data just to compare Campari with the biggest comeptitors, let's focus only on
#the biggest competitors.

#The biggest competitors are the following: 
#"Diageo", "Pernod Ricard", "Bacardi", "Distilleria Caffo", "Stock", "Montenegro"        
#"Dilmoor", "F.lli Branca", "Valdo", "Berlucchi"        


#Let's create a dataset with the observations with "" in the Manufacturer.Global covariate

it.data.comp.null<-it.data.comp[it.data.comp$Manufacturer.Global=="", ]

#Let's see which variable to use to solve the "" issue in Manufacturer.Global
table(it.data.comp.null$Manufacturer.Local) #we can use Manufacturer.Local, that leaves us with 0 null values

#With this command we isolate the observations that have a null value in Manufacturer.Global.
it.data.comp %>%
  filter(Manufacturer.Global == "") %>%
  distinct(Manufacturer.Local) %>%
  pull(Manufacturer.Local)

# "DILMOOR", "JAGDSCHLOSS", "LEKKERLAND EXPRES", "PABS & RICHARZ", "IL SICANO", "S SPITZ", "QUEEN MARGOT", "GAMAR SRL"         
#"LOSITO E GUARINI", "PRIVATE LABEL" 


#Among these, the only manufacturer that is among the biggest competitors is Dilmoor.

it.data.comp$Manufacturer.Global[it.data.comp$Manufacturer.Local=="DILMOOR" & it.data.comp$Manufacturer.Global==""]<-"Dilmoor"

#Now the problem is solved, since we can see with 
colSums(it.data.comp=="") 
#that the null values for Manufacturer.Global were reduced to 1556, so the observations for the big company 
#have a non null value in this covariate.


#Now let's group for product type.
it.data_campari.anis <- it.data.campari[it.data.campari$Category.Global == "Aniseed", ]
it.data_campari.ape <- it.data.campari[it.data.campari$Category.Global == "Aperitif", ]
it.data_campari.brandy<-it.data.campari[it.data.campari$Category.Global == "Brandy & Cognac",]
it.data_campari.cane <- it.data.campari[it.data.campari$Category.Global == "Cane", ]
it.data_campari.champ <- it.data.campari[it.data.campari$Category.Global == "Champagne", ]
it.data_campari.gin <- it.data.campari[it.data.campari$Category.Global == "Gin & Genever", ]
it.data.campari.hb<-it.data.campari[it.data.campari$Category.Global == "Herbal Bitter",]
it.data.campari.lq<-it.data.campari[it.data.campari$Category.Global == "Liqueur",]
it.data.campari.os<-it.data.campari[it.data.campari$Category.Global == "Spirits - Others",]
it.data_campari.rum <- it.data.campari[it.data.campari$Category.Global == "Rum", ]
it.data_campari.sw <- it.data.campari[it.data.campari$Category.Global == "Sparkling Wine", ]
it.data_campari.teq <- it.data.campari[it.data.campari$Category.Global == "Tequila & Mezcal", ]
it.data_campari.vdk <- it.data.campari[it.data.campari$Category.Global == "Vodka", ]
it.data_campari.whisky <- it.data.campari[it.data.campari$Category.Global == "Whisky", ]

#Same for competitors
it.data_comp.anis <- it.data.comp[it.data.comp$Category.Global == "Aniseed", ]
it.data_comp.ape <- it.data.comp[it.data.comp$Category.Global == "Aperitif", ]
it.data_comp.brandy<-it.data.comp[it.data.comp$Category.Global == "Brandy & Cognac",]
it.data_comp.cane <- it.data.comp[it.data.comp$Category.Global == "Cane", ]
it.data_comp.champ <- it.data.comp[it.data.comp$Category.Global == "Champagne", ]
it.data_comp.gin <- it.data.comp[it.data.comp$Category.Global == "Gin & Genever", ]
it.data.comp.hb<-it.data.comp[it.data.comp$Category.Global == "Herbal Bitter",]
it.data.comp.lq<-it.data.comp[it.data.comp$Category.Global == "Liqueur",]
it.data.comp.os<-it.data.comp[it.data.comp$Category.Global == "Spirits - Others",]
it.data_comp.rum <- it.data.comp[it.data.comp$Category.Global == "Rum", ]
it.data_comp.sw <- it.data.comp[it.data.comp$Category.Global == "Sparkling Wine", ]
it.data_comp.teq <- it.data.comp[it.data.comp$Category.Global == "Tequila & Mezcal", ]
it.data_comp.vdk <- it.data.comp[it.data.comp$Category.Global == "Vodka", ]
it.data_comp.whisky <- it.data.comp[it.data.comp$Category.Global == "Whisky", ]

library(dplyr)
library(tidyr)

#Let's create some datasets with the sales values for every product (for both Campari and competitors)

campari_anis_summary_it<- it.data_campari.anis %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_anis_summary_it <- it.data_comp.anis %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_ape_summary_it<- it.data_campari.ape %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_ape_summary_it <- it.data_comp.ape %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_brandy_summary_it<- it.data_campari.brandy %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_brandy_summary_it <- it.data_comp.brandy %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X") 

campari_cane_summary_it<- it.data_campari.cane %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_cane_summary_it<- it.data_comp.cane %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_champ_summary_it <- it.data_campari.champ %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X") #it's equal to 0 for Campari, so we remove it also for competitors

comp_champ_summary_it <- it.data_comp.champ %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_gin_summary_it <- it.data_campari.gin %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_gin_summary_it <- it.data_comp.gin %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_hb_summary_it <- it.data.campari.hb %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_hb_summary_it <- it.data.comp.hb %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_lq_summary_it <- it.data.campari.lq %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_lq_summary_it <- it.data.comp.lq %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_os_summary_it <- it.data.campari.os %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_os_summary_it <- it.data.comp.os %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_rum_summary_it <- it.data_campari.rum %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_rum_summary_it <- it.data_comp.rum %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_sw_summary_it <- it.data_campari.sw %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_sw_summary_it <- it.data_comp.sw %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_teq_summary_it <- it.data_campari.teq %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_teq_summary_it <- it.data_comp.teq %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_vdk_summary_it <- it.data_campari.vdk %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_vdk_summary_it <- it.data_comp.vdk %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_whisky_summary_it <- it.data_campari.whisky %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_whisky_summary_it <- it.data_comp.whisky %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")


#Now let's merge them into two datasets (one for Campari, one for competitors)
sales_campari_it=rbind(campari_anis_summary_it, campari_ape_summary_it, campari_brandy_summary_it, campari_cane_summary_it, 
                       campari_gin_summary_it, campari_hb_summary_it, campari_lq_summary_it, campari_os_summary_it, 
                       campari_rum_summary_it, campari_sw_summary_it, campari_teq_summary_it, campari_vdk_summary_it, campari_whisky_summary_it)
sales_comp_it=rbind(comp_anis_summary_it, comp_ape_summary_it, comp_brandy_summary_it, comp_cane_summary_it, 
                    comp_gin_summary_it, comp_hb_summary_it, comp_lq_summary_it, comp_os_summary_it, 
                    comp_rum_summary_it, comp_sw_summary_it, comp_teq_summary_it, comp_vdk_summary_it, comp_whisky_summary_it)

#To make it more tidy, let's create the column for product name
sales_campari_it$Name_Product=c("Aniseed", "Aperitif", "Brandy", "Cane", "Gin", "Herbal Bitters", "Liqueurs and Spirits", "Spirits - Others", "Rum", "Sparkling Wines", "Tequila", "Vodka", "Whisky")
sales_comp_it$Name_Product=c("Aniseed", "Aperitif", "Brandy", "Cane", "Gin", "Herbal Bitters", "Liqueurs and Spirits", "Spirits - Others", "Rum", "Sparkling Wines", "Tequila", "Vodka", "Whisky")

#Let's put the column with the names as the first one
sales_campari_it <- sales_campari_it %>%
  select(ncol(sales_campari_it), everything())
sales_comp_it <- sales_comp_it %>%
  select(ncol(sales_comp_it), everything())



# Extract years and sales data
years_it <- as.numeric(sub("X", "", colnames(sales_campari_it)))  # Remove "X" from column names and convert to numeric
sales_data_it_campari <- as.matrix(sales_campari_it)  # Convert data to matrix for plotting

years_it <- as.numeric(sub("X", "", colnames(sales_comp_it)))  # Remove "X" from column names and convert to numeric
sales_data_it_comp <- as.matrix(sales_comp_it)  # Convert data to matrix for plotting

colori_it = rainbow(length(unique(sales_comp_it$Name_Product)))

x11()
par(mfrow=c(1,2))
matplot(years_it, t(sales_data_it_campari), type = "l", lty = 1, lwd = 2, col = colori_it, 
        xlab = "Year", ylab = "Sales Value", main = "Sales Campari Italy")

# Add a legend
legend("topright", legend = sales_campari_it$Name_Product, col = colori_it, lty = 1, lwd = 2)

matplot(years_it, t(sales_data_it_comp), type = "l", lty = 1, lwd = 2, col =colori_it, 
        xlab = "Year", ylab = "Sales Value", main = "Sales Competitors Italy")

# Add a legend
legend("topright", legend = sales_comp_it$Name_Product, col = colori_it, lty = 1, lwd = 2)

#The top products in Italy for Campari are Aperitif and Gin. 
#The first two most important products to monitor are Sparkling Wines and Liqueurs since they are the ones with the highest potential of growth.

#Now let's do a graphical vertical analysis of the revenues per product with barplots

x11()
par(mfrow=c(1,2))
barplot(sales_data_it_campari[,-1], col = colori_it, main = "Vertical Analysis Campari Italy")
legend("topright",legend = sales_campari_it$Name_Product, col = colori_it, lty = 1, lwd = 2, cex = 0.5)
barplot(sales_data_it_comp[,-1], col = colori_it, main = "Vertical Analysis Competitors Italy")
legend("topright",legend = sales_comp_it$Name_Product, col = colori_it, lty = 1, lwd = 2, cex = 0.5)


#Let's bind the two datasets and then the three datasets of the two different countries (this will serve us for PowerBI dashboard)
it.data=rbind(it.data.campari, it.data.comp)
it.data$Country<-"Italy"

#In order to see which markets we can penetrate, we should determine the market share for each product
market_share_it <- it.data %>%
  group_by(Manufacturer.Global) %>%        
  summarize(Total.Sales = sum(Sales.Value, na.rm = TRUE)) %>%  
  mutate(Market.Share = (Total.Sales / sum(Total.Sales)) * 100,
         Market.Share = format(round(Market.Share, 4), nsmall = 4))%>%  
  arrange(desc(Market.Share))
market_share_it$Market.Share=as.numeric(market_share_it$Market.Share)


sum(market_share_it$Market.Share[c(4,5,6,7)])
#C4=15.0086. We suppose none of the Minor Manufacturers are in the top10 of competitors.
#Since C4 aims at determining the concentration among the biggest branded manufacturers, we decide to exclude 
#Private Labels, since it may also be the aggregated result of smaller manufacturers supplying for different supermarkets 

#Function to calculate the market share for each product (which is the market with the highest potential of growth)
calculate_market_share_it <- function(it.data) {
  it.data %>%
    group_by(Category.Global, Manufacturer.Global) %>%
    summarise(Total_Sales = sum(Sales.Value, na.rm = TRUE), .groups = "drop") %>%
    group_by(Category.Global) %>%
    mutate(Market_Share_Percentage = (Total_Sales / sum(Total_Sales)) * 100,
           Market_Share_Percentage = format(round(Market_Share_Percentage, 4), nsmall = 4))%>%
    arrange(Category.Global, desc(Market_Share_Percentage)) # Sorted by category and market share
}

market_share_result_it <- calculate_market_share_it(it.data)

#Sparkling wines is the market with the lowest concentration. Then Whisky and Liqueur.

#!!!!!!!I can do this only after I run the code for the three nations
factsales=rbind(ger.data, it.data, uk.data)

#These commands' purpose is to export the dataset 
write.csv(factsales, "Factsales.csv")
saveRDS(factsales, "Factsales.rds")

