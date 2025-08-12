#Mentions Italy

install.packages("readxl")
library(readxl)

setwd("/Users/FedericoCasorati/Desktop/APM/2024:25/Project work/Data/Mentions/Italy")

ment.it.campari.2021=read_xlsx("Italy-Allcategories 2021.xlsx")

ment.it.campari.2022=read_xlsx("Italy-Allcategories 2022.xlsx")

ment.it.campari.2023=read_xlsx("Italy-Allcategories 2023.xlsx")

ment.it.campari.2024=read_xlsx("ITALY-Allcategories 2024_01 2024_08.xlsx")


ment.it.all.2021=read_xlsx("ITALY-AllCategoriesCOMPETITORS 2021.xlsx")

ment.it.all.2022=read_xlsx("ITALY-AllCategoriesCOMPETITORS 2022.xlsx")

ment.it.all.2023=read_xlsx("ITALY-AllCategoriesCOMPETITORS 2023.xlsx")

ment.it.all.2024=read_xlsx("ITALY-AllCategoriesCOMPETITORS 2024_01 2024_08.xlsx")


colnames(ment.it.campari.2021)<-c("Brand", "Mentions", "Earned.Mentions", "Owned.Mentions", "Reach", "Country",
                                  "Positive.Mentions", "Neutral.Mentions",  "Negative.Mentions", "Source",  "Created.Time")
colnames(ment.it.campari.2022)<-colnames(ment.it.campari.2021)
colnames(ment.it.campari.2023)<-colnames(ment.it.campari.2021)
colnames(ment.it.campari.2024)<-colnames(ment.it.campari.2021)

colnames(ment.it.all.2021)<-c("Brand", "Mentions", "Positive.Mentions", "Neutral.Mentions", "Negative.Mentions", 
                              "Country", "Source", "Created.Time")
colnames(ment.it.all.2022)<-colnames(ment.it.all.2021)
colnames(ment.it.all.2023)<-colnames(ment.it.all.2021)
colnames(ment.it.all.2024)<-colnames(ment.it.all.2021)


ment.it.campari=rbind(ment.it.campari.2021, ment.it.campari.2022, ment.it.campari.2023, ment.it.campari.2024)
ment.it.all=rbind(ment.it.all.2021, ment.it.all.2022, ment.it.all.2023, ment.it.all.2024)

library(lubridate)

ment.it.campari <- ment.it.campari %>%
  mutate(
    Start.Date = sub(" - .*", "", Created.Time), 
    Day = day(mdy(Start.Date)),
    Month = month(mdy(Start.Date)), 
    Year = year(mdy(Start.Date))          
  )

ment.it.all <- ment.it.all %>%
  mutate(
    Start.Date = sub(" - .*", "", Created.Time),  
    Day = day(mdy(Start.Date)),
    Month = month(mdy(Start.Date)), 
    Year = year(mdy(Start.Date))          
  )

ment.it.campari <- ment.it.campari %>%
  mutate(
    Start.Date = as.Date(paste(Year, Month, Day, sep = "-")),  
    Week = isoweek(Start.Date),       
    Week = ifelse(Week == 53 & month(Start.Date) == 12, 52, Week),
    Year.Week = paste(year(Start.Date), Week, sep = "-")      
  )

ment.it.all <- ment.it.all %>%
  mutate(
    Start.Date = as.Date(paste(Year, Month, Day, sep = "-")),  
    Week = isoweek(Start.Date),      
    Week = ifelse(Week == 53 & month(Start.Date) == 12, 52, Week),
    Year.Week = paste(year(Start.Date), Week, sep = "-")      
  )

#Now we have two datasets for Campari and compettiors with the columns with Day, Week, Month and Year, 
#so we can compare them with the Sell Out data

#Let's transform the mentions data from character into numeric
ment.it.campari$Mentions=as.numeric(ment.it.campari$Mentions)
ment.it.all$Mentions=as.numeric(ment.it.all$Mentions)

ment.it.campari$Earned.Mentions=as.numeric(ment.it.campari$Earned.Mentions)

ment.it.campari$Owned.Mentions=as.numeric(ment.it.campari$Owned.Mentions)

ment.it.campari$Reach=as.numeric(ment.it.campari$Reach)

ment.it.campari$Positive.Mentions=as.numeric(ment.it.campari$Positive.Mentions)
ment.it.all$Positive.Mentions=as.numeric(ment.it.all$Positive.Mentions)

ment.it.campari$Neutral.Mentions=as.numeric(ment.it.campari$Neutral.Mentions)
ment.it.all$Neutral.Mentions=as.numeric(ment.it.all$Neutral.Mentions)

ment.it.campari$Negative.Mentions=as.numeric(ment.it.campari$Negative.Mentions)
ment.it.all$Negative.Mentions=as.numeric(ment.it.all$Negative.Mentions)


#Let's write a function that gives us the reaches for the bubble chart. It allows us to see the log of the reaches
#for a specific period for a certain Source
ment.it.campari <- ment.it.campari %>%
  group_by(Source, Year, Week) %>%
  mutate(LogOfSumReach = if_else(sum(Reach) != 0, log(sum(Reach)), 0)) %>%
  ungroup()


library(openxlsx)
write.xlsx(ment.it.campari, file = "Mentions_Italy_Campari.xlsx")
write.csv(ment.it.campari, file = "Mentions_Italy_Campari.csv")


ment_it_summary <- ment.it.campari %>%
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

write.xlsx(ment_it_summary, file = "Summary_Mentions_Italy.xlsx")









































































































































































































































































































































































































































































































































