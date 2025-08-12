#Mentions UK

install.packages("readxl")
library(readxl)

setwd("/Users/FedericoCasorati/Desktop/APM/2024:25/Project work/Data/Mentions/UK")

ment.uk.campari.2021=read_xlsx("UK-Allcategories 2021.xlsx")
ment.uk.campari.2021=ment.uk.campari.2021[-c(1:2),]
ment.uk.campari.2022=read_xlsx("UK-Allcategories 2022.xlsx")
ment.uk.campari.2022=ment.uk.campari.2022[-c(1:2),]
ment.uk.campari.2023=read_xlsx("UK-Allcategories 2023.xlsx")
ment.uk.campari.2023=ment.uk.campari.2023[-c(1:2),]
ment.uk.campari.2024=read_xlsx("UK-Allcategories 2024 2024_01 2024_08.xlsx")
ment.uk.campari.2024=ment.uk.campari.2024[-c(1:2),]

ment.uk.all.2021=read_xlsx("UK-AllCategoriesCOMPETITORS 2021.xlsx")
ment.uk.all.2021=ment.uk.all.2021[-c(1:2),]
ment.uk.all.2022=read_xlsx("UK-AllCategoriesCOMPETITORS 2022.xlsx")
ment.uk.all.2022=ment.uk.all.2022[-c(1:2),]
ment.uk.all.2023=read_xlsx("UK-AllCategoriesCOMPETITORS 2023.xlsx")
ment.uk.all.2023=ment.uk.all.2023[-c(1:2),]
ment.uk.all.2024=read_xlsx("UK-AllCategoriesCOMPETITORS 2024 2024_01 2024_08.xlsx")
ment.uk.all.2024=ment.uk.all.2024[-c(1:2),]

colnames(ment.uk.campari.2021)<-c("Brand", "Mentions", "Earned.Mentions", "Owned.Mentions", "Reach", "Country",
                              "Positive.Mentions", "Neutral.Mentions",  "Negative.Mentions", "Source",  "Created.Time")
colnames(ment.uk.campari.2022)<-colnames(ment.uk.campari.2021)
colnames(ment.uk.campari.2023)<-colnames(ment.uk.campari.2021)
colnames(ment.uk.campari.2024)<-colnames(ment.uk.campari.2021)

colnames(ment.uk.all.2021)<-c("Brand", "Mentions", "Positive Mentions", "Neutral Mentions", "Negative Mentions", 
                              "Country (Message)", "Source", "Created Time")
colnames(ment.uk.all.2022)<-colnames(ment.uk.all.2021)
colnames(ment.uk.all.2023)<-colnames(ment.uk.all.2021)
colnames(ment.uk.all.2024)<-colnames(ment.uk.all.2021)


ment.uk.campari=rbind(ment.uk.campari.2021, ment.uk.campari.2022, ment.uk.campari.2023, ment.uk.campari.2024)
ment.uk.all=rbind(ment.uk.all.2021, ment.uk.all.2022, ment.uk.all.2023, ment.uk.all.2024)

library(lubridate)

ment.uk.campari <- ment.uk.campari %>%
  mutate(
    Start.Date = sub(" - .*", "", Created.Time), 
    Day = day(mdy(Start.Date)),
    Month = month(mdy(Start.Date)), 
    Year = year(mdy(Start.Date))          
  )

ment.uk.all <- ment.uk.all %>%
  mutate(
    Start.Date = sub(" - .*", "", `Created Time`),  
    Day = day(mdy(Start.Date)),
    Month = month(mdy(Start.Date)), 
    Year = year(mdy(Start.Date))          
  )

ment.uk.campari <- ment.uk.campari %>%
  mutate(
    Start.Date = as.Date(paste(Year, Month, Day, sep = "-")),  
    Week = isoweek(Start.Date),       
    Week = ifelse(Week == 53 & month(Start.Date) == 12, 52, Week),
    Year.Week = paste(year(Start.Date), Week, sep = "-")      
  )

ment.uk.all <- ment.uk.all %>%
  mutate(
    Start.Date = as.Date(paste(Year, Month, Day, sep = "-")),  
    Week = isoweek(Start.Date),      
    Week = ifelse(Week == 53 & month(Start.Date) == 12, 52, Week),
    Year.Week = paste(year(Start.Date), Week, sep = "-")      
  )

#Now we have two datasets for Campari and compettiors with the columns with Day, Week, Month and Year, 
#so we can compare them with the Sell Out data

#Let's transform the mentions data from character into numeric
ment.uk.campari$Mentions=as.numeric(ment.uk.campari$Mentions)
ment.uk.all$Mentions=as.numeric(ment.uk.all$Mentions)

ment.uk.campari$Earned.Mentions=as.numeric(ment.uk.campari$Earned.Mentions)

ment.uk.campari$Owned.Mentions=as.numeric(ment.uk.campari$Owned.Mentions)

ment.uk.campari$Reach=as.numeric(ment.uk.campari$Reach)

ment.uk.campari$Positive.Mentions=as.numeric(ment.uk.campari$Positive.Mentions)
ment.uk.all$'Positive Mentions'=as.numeric(ment.uk.all$'Positive Mentions')

ment.uk.campari$Neutral.Mentions=as.numeric(ment.uk.campari$Neutral.Mentions)
ment.uk.all$'Neutral Mentions'=as.numeric(ment.uk.all$'Neutral Mentions')

ment.uk.campari$Negative.Mentions=as.numeric(ment.uk.campari$Negative.Mentions)
ment.uk.all$'Negative Mentions'=as.numeric(ment.uk.all$'Negative Mentions')




#-------------------------------------
#Now after this to make the dataset good for PowerBI


unique(ment.uk.campari$Brand)










#Let's write a function that gives us the reaches for the bubble chart. It allows us to see the log of the reaches
#for a specific period for a certain Source
ment.uk.campari <- ment.uk.campari %>%
  group_by(Source, Year, Week) %>%
  mutate(LogOfSumReach = if_else(sum(Reach) != 0, log(sum(Reach)), 0)) %>%
  ungroup()


library(openxlsx)
write.xlsx(ment.uk.campari, file = "Mentions_UK_Campari.xlsx")
write.csv(ment.uk.campari, file = "Mentions_UK_Campari.csv")


ment_uk_summary <- ment.uk.campari %>%
  group_by(Source, Year) %>%
  summarize(
    TotalMentions = sum(Mentions, na.rm = TRUE),
    PositiveMentions = sum(Positive.Mentions, na.rm = TRUE),
    NeutralMentions = sum(Neutral.Mentions, na.rm = TRUE),
    NegativeMentions = sum(Negative.Mentions, na.rm = TRUE),
    EarnedMentions = sum(Earned.Mentions, na.rm = TRUE),
    OwnedMentions = sum(Owned.Mentions, na.rm = TRUE)
  ) %>%
  mutate(
    PositiveRatio = PositiveMentions / TotalMentions,
    NeutralRatio = NeutralMentions / TotalMentions,
    NegativeRatio = NegativeMentions / TotalMentions,
    EarnedRatio = EarnedMentions / TotalMentions,
    OwnedRatio = OwnedMentions / TotalMentions
  )

write.xlsx(ment_uk_summary, file = "Summary_Mentions_UnitedKingdom.xlsx")




































































































































































































































































































































































































































































































































