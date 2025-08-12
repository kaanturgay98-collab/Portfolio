#Germany 

setwd("/Users/FedericoCasorati/Desktop/APM/2024:25/Project work/Data")
library(dplyr)
library(tidyr)
ger.data=read.csv("Germany.csv")
str(ger.data)
head(ger.data)

#Sub Channel: total market and split per store format and regions.
             #the initial code is the code of the product (with DE in the end). LEH+DM indicates
             #that it's sold in grocery stores and drug stores. 1 indicates it's the first covariate(?)
             #LEH+DM in the end remarks the channels through which it is sold.
#Category Local: categories for the specific country
#Sub Category Local: additional details on categories segmentation for the specific country
#Sub Category Detail Local: additional details on categories segmentation for the specific country
#Manufacturer Local: Manufacturer name for the specific country
#Brand Local: Brand name for the specific country
#Product Local: Product name for the specific country
#Category Global: Categories at a global scale (i.e. across specific countries)
#Sub Category Global: additional details on categories segmentation at a global scale
#Sub Category Detail Global: additional details on categories segmentation at a global scale
#Manufacturer Global Manufacturer at a global scale
#Brand Global: Brand at a global scale
#Sub Brand Global: Additional detail on brand at a global scale
#!!!!Size
#Year: 2021-2024
#Month: 1-12
#Week: 1-53. 53 because is due to a misalignment between Provider Calendar and Full Year
#Sales Volume: Total Volume (liters) sold in a specific period
#Sales Volume No Promo: Volume (liters) sold in a specific period without any promo
#Sales Volume Baseline: Volume (liters) sold in a specific period, without considering the impact of promo. Slightly different from the ‘No Promo’ measure
#Sales Value: Total Value (€) sold in a specific period
#Sales Value No Promo: Value (€) sold in a specific period without any promo
#Sales Value Baseline: Value (€) sold in a specific period, without considering the impact of promo. Slightly different from the ‘No Promo’ measure
#Weighted Distribution: Weight of the stores that are selling the brand in the week. Maximum is 100
#Promo: sales made during a promotion activity (when the product had a cut price or extra visibility)
#No promo: opposite
#Baseline: represents the sales that would occur without any promotional support.It's an estimate of the natural or regular demand for the product.
#Incremental: Refers to the additional sales that occur because of promotional activities (e.g., price cuts, extra visibility).These sales are over and above the baseline and are directly attributed to promotional efforts.


#We have to use Category.Global in order to differentiate among the different products
unique(ger.data$Category.Global)
#I want to know in the uk.data how many observations are there for each category of products
table(ger.data$Category.Local) #to find how many observations in each category local
table(ger.data$Category.Local)

#Let's rename this covariate, otherwise it's different from the .Local counterpart
ger.data <- ger.data %>%
  rename(Manufacturer.Global = Manufactur.Global)

#Let's see which are all the companies selling spirits that are included in the dataset
unique(ger.data$Manufacturer.Global)
unique(ger.data$Manufacturer.Local)


#These commands help us re-ordinating the data for week, month and year. We want them in chronological order.
ger.data.campari <- ger.data[ger.data$Manufacturer.Global == "Campari Group", ]

ger.data.campari <- ger.data.campari %>%
  arrange(Year, Month, Week)

ger.data.comp<- ger.data[ger.data$Manufacturer.Global != "Campari Group", ]

#We notice a problem: the observations of week 52 are categorized as part of month 1. Let's change that and let's 
#re-ordinate the observations again (repeat the previous commands). We assume that this issue comes up because
#the year is made by 52.something weeks, so those days after the 52nd week are considered part of the year after,
#but they are actually part of the year before
ger.data.campari <- ger.data.campari %>%
  mutate(
    Year = ifelse(Week == 52 & Month == 1, Year - 1, Year),
    Month = ifelse(Week == 52 & Month == 1, 12, Month)
  ) %>%
  arrange(Year, Month, Week)

#There is an issue because months 10, 11 and 12 are before 5. This is because Month is a character, not a number
ger.data.campari$Month <- as.numeric(ger.data.campari$Month)

#Now let's do the same for the competitors
#This will change the month and year if week=52 and will reorder the dataset
ger.data.comp <- ger.data.comp %>%
  mutate(
    Year = ifelse(Week == 52 & Month == 1, Year - 1, Year),
    Month = ifelse(Week == 52 & Month == 1, 12, Month)
  ) %>%
  arrange(Year, Month, Week)

#We notice some null values and NAs in Category.Global
#Let's solve the null values issue
colSums(ger.data.campari == "null") #for Campari there are no null values in Category.Global
colSums(ger.data.campari == "") #for Campari there are no null values in Category.Global
colSums(ger.data.campari == "NA") #for Campari there are no NAs in Category.Global
colSums(ger.data.campari == "Not Available") #for Campari there are no NAs in Category.Global

colSums(ger.data.comp == "null") #for competitors there are no null values in Category.Global
colSums(ger.data.comp == "") #for competitors there are 677 null values in Manufacturer.Global
colSums(ger.data.comp == "NA") #for competitors there are no NAs in Category.Global
colSums(ger.data.comp == "Not Available") #for competitors there are  1,957,045 NAs in Category.Global, so it's symptomatic

#Category.Global has some null values, let's solve this.
unique(ger.data.comp$Category.Local[ger.data.comp$Category.Global=="null"])

#This command fixes the problem and assigns a "Category.Global" value to all the observations that had a null value for this covariate
ger.data.comp <- ger.data.comp %>%
  mutate(
    Category.Global = if_else(
      Category.Global == "null", 
      case_when(
      Category.Local == "GIN U GENEVER" ~ "Gin & Genever",
      Category.Local == "RUM INKL ZUSATZ" ~ "Rum",
      Category.Local == "WHISKY INKL ZUSATZ" ~ "Whisky",
      Category.Local == "WODKA INKL ZUSATZ" ~ "Vodka",
      Category.Local == "LIKOERE" ~ "Liqueur",
      Category.Local == "KRAEUTERLIKOER" ~ "Herbal Bitter",
      Category.Local == "ANIS SPIRITUOSE" ~ "Aniseed",
      Category.Local == "RTD (SPIRIT BASED)" ~ "RTD & RTS Spirits",
      Category.Local == "APERITIF U WERMUT" ~ "Aperitif",
      Category.Local == "KLARE SPIRITUOSEN INKL ZUSATZ" ~ "Spirits - Others",
      Category.Local == "OBSTBREANDE" ~ "Brandy & Cognac",
      Category.Local == "ALKOHOLISCHE GETRAENKE" ~ "Other Categories",
      TRUE ~ NA_character_ 
    ),
    Category.Global
  )
  )

table(ger.data.comp$Category.Global) #now for competitors there are 0 null values in Category.Global
#We can do an analysis by product that is 100% accurate

#Now, since we need to analyze the competitors as well, let's look for any NAs and null values in this covariate

#We need to solve this issue. 
#Since we need the competitors' data just to compare Campari with the biggest comeptitors, let's focus only on
#the biggest competitors.

#The biggest competitors are the following: 
#"Pernod Ricard", "Diageo", "Brown Forman", "Nordbrand Nordhausen", "Henkell Freixenet", "Bacardi", "Beam Suntory", 
#"Mast Jagermeister", "Berentzen", "Diversa"


#Let's see which variable to use to solve the "Not Available" and the "" issue in Manufacturer.Global
table(ger.data.comp.null$Manufacturer.Local) #for the null values, we can use Manufacturer.Local, that leaves us with
                                             #only 52 null values (not that big of a deal out of almost 4 million observations)


#Let's start from the null values.
#With this command we isolate the observations that have a null value in Manufacturer.Global.
ger.data.comp %>%
  filter(Manufacturer.Global == "null") %>%
  distinct(Manufacturer.Local) %>%
  pull(Manufacturer.Local)

ger.data.comp %>%
  filter(Manufacturer.Global == "") %>%
  distinct(Manufacturer.Local) %>%
  pull(Manufacturer.Local) 
#We will take care of "" later

#Among these manufacturers, there are no top manufacturers

#Now let's solve the bigger issue: the over 1 million observations wiht Manufacturer.Global=="Not Available".
#Now we want to determine all the Brand.Global values.
target_manufacturers <- c(
  "Pernod Ricard", "Diageo", "Brown Forman", "Nordbrand Nordhausen", "Henkell Freixenet", 
  "Bacardi", "Beam Suntory", "Mast Jagermeister", "Berentzen", "Diversa")

#What we want to do is the following: we assume a 1 to many relationship between Manufacturer.Global and Brand.Global
#since each manufacturer owns a group of brands. 
#With the following command, we create a dataset where we list all the Brand.Global of a specific Manufacturer.Global
manufacturer_brand_dataset <- ger.data.comp %>%
  filter(
    Manufacturer.Global %in% target_manufacturers,
    !grepl("^Minor Manufacturers", Brand.Global),
    !grepl("^Minor Brands", Brand.Global)
  ) %>%
  select(Manufacturer.Global, Brand.Global) %>% 
  distinct() %>% 
  arrange(Manufacturer.Global, Brand.Global)
#We don't include Minor Manufacturers and Minor Brands because they are names that can be referred to more than one manufacturer.

#Let's solve the =="" problem too.
#With this command, we change the value of Manufacturer.Global in the dataset where it was "Not Available" or ="", only
#for the top10 competitors of Campari
ger.data.comp.na <- ger.data.comp %>%
  filter(Manufacturer.Global == "Not Available" | Manufacturer.Global == "") %>%
  left_join(manufacturer_brand_dataset, by = "Brand.Global", suffix = c("", ".updated")) %>%
  mutate(
    Manufacturer.Global = if_else(
      !is.na(Manufacturer.Global.updated),
      Manufacturer.Global.updated,
      Manufacturer.Global
    )
  ) %>%
  select(-Manufacturer.Global.updated)

#Here we isolate the observations where Manufacturer.Global!="Not Available" and we bind it with the updated version
#of the observations where Manufacturer.Global=="Not Available"
ger.data.comp <- ger.data.comp %>%
  filter(Manufacturer.Global != "Not Available" & Manufacturer.Global != "") %>%
  bind_rows(ger.data.comp.na)

colSums(ger.data.comp == "Not Available") 
colSums(ger.data.comp=="")

#We solved the problem with the "Not Available" issue since we can see that the number of NA observations decreased.
#For "" problem, there are no observations that belong to one of the top competitors.




#Now let's create a dataset for each product:
ger.data_campari.anis <- ger.data.campari[ger.data.campari$Category.Global == "Aniseed", ]
ger.data_campari.ape <- ger.data.campari[ger.data.campari$Category.Global == "Aperitif", ]
ger.data_campari.brandy<-ger.data.campari[ger.data.campari$Category.Global == "Brandy & Cognac",]
ger.data_campari.cane <- ger.data.campari[ger.data.campari$Category.Global == "Cane", ]
ger.data_campari.gin <- ger.data.campari[ger.data.campari$Category.Global == "Gin & Genever", ]
ger.data.campari.hb<-ger.data.campari[ger.data.campari$Category.Global == "Herbal Bitter",]
ger.data.campari.lq<-ger.data.campari[ger.data.campari$Category.Global == "Liqueur",]
ger.data_campari.rum <- ger.data.campari[ger.data.campari$Category.Global == "Rum", ]
ger.data_campari.sw <- ger.data.campari[ger.data.campari$Category.Global == "Sparkling Wine", ]
ger.data_campari.vdk <- ger.data.campari[ger.data.campari$Category.Global == "Vodka", ]
ger.data_campari.teq <- ger.data.campari[ger.data.campari$Category.Global == "Tequila & Mezcal", ]
ger.data_campari.whisky <- ger.data.campari[ger.data.campari$Category.Global == "Whisky", ]

#Same for competitors
ger.data_comp.anis <- ger.data.comp[ger.data.comp$Category.Global == "Aniseed", ]
ger.data_comp.ape <- ger.data.comp[ger.data.comp$Category.Global == "Aperitif", ]
ger.data_comp.brandy <- ger.data.comp[ger.data.comp$Category.Global == "Brandy & Cognac", ]
ger.data_comp.cane <- ger.data.comp[ger.data.comp$Category.Global == "Cane", ]
ger.data_comp.gin <- ger.data.comp[ger.data.comp$Category.Global == "Gin & Genever", ]
ger.data.comp.hb<-ger.data.comp[ger.data.comp$Category.Global == "Herbal Bitter",]
ger.data.comp.lq<-ger.data.comp[ger.data.comp$Category.Global == "Liqueur",]
ger.data_comp.rum <- ger.data.comp[ger.data.comp$Category.Global == "Rum", ]
ger.data_comp.sw <- ger.data.comp[ger.data.comp$Category.Global == "Sparkling Wine", ]
ger.data_comp.vdk <- ger.data.comp[ger.data.comp$Category.Global == "Vodka", ]
ger.data_comp.teq <- ger.data.comp[ger.data.comp$Category.Global == "Tequila & Mezcal", ]
ger.data_comp.whisky <- ger.data.comp[ger.data.comp$Category.Global == "Whisky", ]

#Let's create some datasets with the sales values for every product (for both Campari and competitors)

campari_anis_summary_ger<- ger.data_campari.anis %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_anis_summary_ger<- ger.data_comp.anis %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_ape_summary_ger<- ger.data_campari.ape %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_ape_summary_ger<- ger.data_comp.ape %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_brandy_summary_ger<- ger.data_campari.brandy %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_brandy_summary_ger <- ger.data_comp.brandy %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_cane_summary_ger<- ger.data_campari.cane %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_cane_summary_ger <- ger.data_comp.cane %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_gin_summary_ger <- ger.data_campari.gin %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_gin_summary_ger <- ger.data_comp.gin %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_hb_summary_ger<- ger.data.campari.hb %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X") 

comp_hb_summary_ger <- ger.data.comp.hb %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_lq_summary_ger <- ger.data.campari.lq %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_lq_summary_ger <- ger.data.comp.lq %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_rum_summary_ger <- ger.data_campari.rum %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_rum_summary_ger <- ger.data_comp.rum %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_sw_summary_ger <- ger.data_campari.sw %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_sw_summary_ger <- ger.data_comp.sw %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_teq_summary_ger <- ger.data_campari.teq %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_teq_summary_ger <- ger.data_comp.teq %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_vdk_summary_ger <- ger.data_campari.vdk %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_vdk_summary_ger <- ger.data_comp.vdk %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

campari_whisky_summary_ger <- ger.data_campari.whisky %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")

comp_whisky_summary_ger <- ger.data_comp.whisky %>%
  group_by(Year) %>%
  summarize(Sales.Value = sum(Sales.Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = Year, values_from = Sales.Value, names_prefix = "X")


#Now let's merge them into two datasets (one for Campari, one for competitors)
sales_campari_ger=rbind(campari_anis_summary_ger, campari_ape_summary_ger, campari_brandy_summary_ger, campari_cane_summary_ger, 
                        campari_gin_summary_ger, campari_hb_summary_ger, campari_lq_summary_ger, campari_rum_summary_ger, campari_sw_summary_ger, 
                        campari_teq_summary_ger, campari_vdk_summary_ger, campari_whisky_summary_ger)
sales_comp_ger=rbind(comp_anis_summary_ger, comp_ape_summary_ger, comp_brandy_summary_ger, comp_cane_summary_ger, 
                     comp_gin_summary_ger, comp_hb_summary_ger, comp_lq_summary_ger, comp_rum_summary_ger, comp_sw_summary_ger, 
                     comp_teq_summary_ger, comp_vdk_summary_ger, comp_whisky_summary_ger)

#To make it more tidy, let's create the column for product name
sales_campari_ger$Name_Product=c("Aniseed", "Aperitif", "Brandy", "Cane", "Gin", "Herbal Bitters", "Liqueurs and Spirits", "Rum", "Sparkling Wines", "Tequila", "Vodka", "Whisky")
sales_comp_ger$Name_Product=c("Aniseed", "Aperitif", "Brandy", "Cane", "Gin", "Herbal Bitters", "Liqueurs and Spirits", "Rum", "Sparkling Wines", "Tequila", "Vodka", "Whisky")

#Let's put the column with the names as the first one
sales_campari_ger <- sales_campari_ger %>%
  select(ncol(sales_campari_ger), everything())
sales_comp_ger <- sales_comp_ger %>%
  select(ncol(sales_comp_ger), everything())
sales_comp_ger<-sales_comp_ger[,-7]

# Extract years and sales data
years <- as.numeric(sub("X", "", colnames(sales_campari_ger)))  # Remove "X" from column names and convert to numeric
sales_data_ger_campari <- as.matrix(sales_campari_ger)  # Convert data to matrix for plotting

years <- as.numeric(sub("X", "", colnames(sales_comp_ger)))  # Remove "X" from column names and convert to numeric
sales_data_ger_comp <- as.matrix(sales_comp_ger)  # Convert data to matrix for plotting


# Plot using matplot

colori_ger = rainbow(length(unique(sales_comp_ger$Name_Product)))

x11()
par(mfrow=c(1,2))
matplot(years, t(sales_data_ger_campari), type = "l", lty = 1, lwd = 2, col = colori_ger, 
        xlab = "Year", ylab = "Sales Value", main = "Sales Campari Germany")

# Add a legend
legend("topright", legend = sales_comp_ger$Name_Product, col = colori_ger, lty = 1, lwd = 2)

matplot(years, t(sales_data_ger_comp), type = "l", lty = 1, lwd = 2, col =colori_ger, 
        xlab = "Year", ylab = "Sales Value", main = "Sales Competitors Germany")

# Add a legend
legend("topright", legend = sales_comp_ger$Name_Product, col = colori_ger, lty = 1, lwd = 2)


#The top products are Aperitif and Whisky for Campari. For competitors it's Sparkling Wines and Whsiky.

#Now let's do a graphical vertical analysis of the revenues per product with barplots

x11()
par(mfrow=c(1,2))
barplot(sales_data_ger_campari[,-1], col = colori_ger, main = "Vertical Analysis Campari Germany")
legend("topright",legend = sales_campari_ger$Name_Product, col = colori_ger, lty = 1, lwd = 2, cex = 0.5)
barplot(sales_data_ger_comp[,-1], col = colori_ger, main = "Vertical Analysis Competitors Germany")
legend("topright",legend = sales_comp_ger$Name_Product, col = colori_ger, lty = 1, lwd = 2, cex = 0.5)
#We can see that it's the same chart but only focuses on last year.


#We want to bind the datasets in order to have a single dataset.
ger.data=rbind(ger.data.campari, ger.data.comp)
ger.data$Country<-"Germany"

#Let's calculate the market share
market_share_ger <- ger.data %>%
  group_by(Manufacturer.Global) %>%        
  summarize(Total.Sales = sum(Sales.Value, na.rm = TRUE)) %>%  
  mutate(Market.Share = (Total.Sales / sum(Total.Sales)) * 100,
         Market.Share = format(round(Market.Share, 4), nsmall = 4))%>%  
  arrange(desc(Market.Share))
market_share_ger$Market.Share=as.numeric(market_share_ger$Market.Share)

sum(market_share_ger$Market.Share[c(3,4,6,8)])
#C4=16.9569. We didn't include Not Available value because we know they belong to 
#manufacturers that are not in the top 10, so for sure they wouldn't enter the requirements
#for the C4. We suppose none of the Minor Manufacturers are in the top10 of competitors.
#Since C4 aims at determining the concentration among the biggest branded manufacturers, we decide to exclude 
#Private Labels, since it may also be the aggregated result of smaller manufacturers supplying for different supermarkets 

calculate_market_share_ger <- function(ger.data) {
  ger.data %>%
    group_by(Category.Global, Manufacturer.Global) %>%
    summarise(Total_Sales = sum(Sales.Value, na.rm = TRUE), .groups = "drop") %>%
    group_by(Category.Global) %>%
    mutate(Market_Share_Percentage = (Total_Sales / sum(Total_Sales)) * 100,
           Market_Share_Percentage = format(round(Market_Share_Percentage, 4), nsmall = 4))%>%
    arrange(Category.Global, desc(Market_Share_Percentage)) # Sorted by category and market share
}

market_share_result_ger <- calculate_market_share_ger(ger.data)


#Campari is the leader in the Aperitif segment. Sparkling wine is a very little concentrated market.

