#Mentions UK

install.packages("readxl")
library(readxl)

setwd("/Users/FedericoCasorati/Desktop/APM/2024:25/Project work/Data/Mentions/Germany")

ment.ger.campari.2021=read_xlsx("Germany-Allcategories 2021.xlsx")
ment.ger.campari.2021=ment.ger.campari.2021[-c(1:2),]
ment.ger.campari.2022=read.csv("Germany-Allcategories 2022.csv")

ment.ger.campari.2023=read_xlsx("Germany-Allcategories 2023.xlsx")
ment.ger.campari.2023=ment.ger.campari.2023[-c(1:2),]
ment.ger.campari.2024=read_xlsx("Germany-Allcategories 2024_01 2024_08.xlsx")
ment.ger.campari.2024=ment.ger.campari.2024[-c(1:2),]

ment.ger.all.2021=read_xlsx("Germany-AllCategoriesCOMPETITORS 2021.xlsx")
ment.ger.all.2021=ment.ger.all.2021[-c(1:2),]
ment.ger.all.2022=read_xlsx("Germany-AllCategoriesCOMPETITORS 2022.xlsx")
ment.ger.all.2022=ment.ger.all.2022[-c(1:2),]
ment.ger.all.2023=read_xlsx("Germany-AllCategoriesCOMPETITORS 2023.xlsx")
ment.ger.all.2023=ment.ger.all.2023[-c(1:2),]
ment.ger.all.2024=read_xlsx("Germany-AllCategoriesCOMPETITORS 2024_01 2024_08.xlsx")
ment.ger.all.2024=ment.ger.all.2024[-c(1:2),]

colnames(ment.ger.campari.2021)<-c("Brand", "Mentions", "Earned.Mentions", "Owned.Mentions", "Reach", "Country",
                                  "Positive.Mentions", "Neutral.Mentions",  "Negative.Mentions", "Source",  "Created.Time")
colnames(ment.ger.campari.2022)<-colnames(ment.ger.campari.2021)
colnames(ment.ger.campari.2023)<-colnames(ment.ger.campari.2021)
colnames(ment.ger.campari.2024)<-colnames(ment.ger.campari.2021)

colnames(ment.ger.all.2021)<-c("Brand", "Mentions", "Positive.Mentions", "Neutral.Mentions", "Negative.Mentions", 
                              "Country", "Source", "Created.Time")
colnames(ment.ger.all.2022)<-colnames(ment.ger.all.2021)
colnames(ment.ger.all.2023)<-colnames(ment.ger.all.2021)
colnames(ment.ger.all.2024)<-colnames(ment.ger.all.2021)


ment.ger.campari=rbind(ment.ger.campari.2021, ment.ger.campari.2022, ment.ger.campari.2023, ment.ger.campari.2024)
ment.ger.all=rbind(ment.ger.all.2021, ment.ger.all.2022, ment.ger.all.2023, ment.ger.all.2024)

library(lubridate)

ment.ger.campari <- ment.ger.campari %>%
  mutate(
    Start.Date = sub(" - .*", "", Created.Time), 
    Day = day(mdy(Start.Date)),
    Month = month(mdy(Start.Date)), 
    Year = year(mdy(Start.Date))          
  )

ment.ger.all <- ment.ger.all %>%
  mutate(
    Start.Date = sub(" - .*", "", Created.Time),  
    Day = day(mdy(Start.Date)),
    Month = month(mdy(Start.Date)), 
    Year = year(mdy(Start.Date))          
  )

ment.ger.campari <- ment.ger.campari %>%
  mutate(
    Start.Date = as.Date(paste(Year, Month, Day, sep = "-")),  
    Week = isoweek(Start.Date),       
    Week = ifelse(Week == 53 & month(Start.Date) == 12, 52, Week),
    Year.Week = paste(year(Start.Date), Week, sep = "-")      
  )

ment.ger.all <- ment.ger.all %>%
  mutate(
    Start.Date = as.Date(paste(Year, Month, Day, sep = "-")),  
    Week = isoweek(Start.Date),      
    Week = ifelse(Week == 53 & month(Start.Date) == 12, 52, Week),
    Year.Week = paste(year(Start.Date), Week, sep = "-")      
  )

#Now we have two datasets for Campari and compettiors with the columns with Day, Week, Month and Year, 
#so we can compare them with the Sell Out data


#Let's transform the mentions data from character into numeric
ment.ger.campari$Mentions=as.numeric(ment.ger.campari$Mentions)
ment.ger.all$Mentions=as.numeric(ment.ger.all$Mentions)

ment.ger.campari$Earned.Mentions=as.numeric(ment.ger.campari$Earned.Mentions)

ment.ger.campari$Owned.Mentions=as.numeric(ment.ger.campari$Owned.Mentions)

ment.ger.campari$Reach=as.numeric(ment.ger.campari$Reach)

ment.ger.campari$Positive.Mentions=as.numeric(ment.ger.campari$Positive.Mentions)
ment.ger.all$Positive.Mentions=as.numeric(ment.ger.all$Positive.Mentions)

ment.ger.campari$Neutral.Mentions=as.numeric(ment.ger.campari$Neutral.Mentions)
ment.ger.all$Neutral.Mentions=as.numeric(ment.ger.all$Neutral.Mentions)

ment.ger.campari$Negative.Mentions=as.numeric(ment.ger.campari$Negative.Mentions)
ment.ger.all$Negative.Mentions=as.numeric(ment.ger.all$Negative.Mentions)


#Let's write a function that gives us the reaches for the bubble chart. It allows us to see the log of the reaches
#for a specific period for a certain Source
ment.ger.campari <- ment.ger.campari %>%
  group_by(Source, Year, Week) %>%
  mutate(LogOfSumReach = if_else(sum(Reach) != 0, log(sum(Reach)), 0)) %>%
  ungroup()


library(openxlsx)
write.xlsx(ment.ger.campari, file = "Mentions_Germany_Campari.xlsx")
write.csv(ment.ger.campari, file = "Mentions_Germany_Campari.csv")




ment_ger_summary <- ment.ger.campari %>%
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

write.xlsx(ment_ger_summary, file = "Summary_Mentions_Germany.xlsx")








































































































