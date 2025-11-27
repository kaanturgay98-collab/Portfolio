library(dplyr)
library(lubridate)
library(tidyr)      
library(stringr)   


getwd()
setwd("C:\\Users\\anastasiia_yakobson\\Documents\\POLIMI\\2-1\\marketing analytics\\wetransfer_type-b_2024-11-18_0819\\Type B")


#read data
B_dataset <- read.csv("C:\\Users\\anastasiia_yakobson\\Documents\\POLIMI\\2-1\\marketing analytics\\wetransfer_type-b_2024-11-18_0819\\Type B\\Dataset_CRM_TypeB.csv", stringsAsFactors = FALSE)
View(B_dataset)
str(B_dataset)
nrow(B_dataset)
colSums(is.na(B_dataset)) # Show NA in dataset


############################### convert the data ############################### 

B_dataset_clean <- mutate(B_dataset, ticket_id = as.integer(ticket_id)) # Convert ticket_id to int
sum(B_dataset_clean$ticket_id<=0)
sum(duplicated(B_dataset_clean$ticket_id))

sum(B_dataset_clean$id_customer <=0)
B_dataset_clean <- mutate(B_dataset_clean, date = as.Date(date,format = "%Y-%m-%d")) # Convert date to date
sum(B_dataset_clean$store_id <=0)
B_dataset_clean$store_type <- as.factor(B_dataset_clean$store_type)

B_dataset_clean$PL_gross_sales <- str_replace(B_dataset_clean$PL_gross_sales, ",", ".")
B_dataset_clean$PL_gross_sales <- as.numeric(B_dataset_clean$PL_gross_sales)

B_dataset_clean <- mutate(B_dataset_clean, number_items_other = as.integer(number_items_other))
B_dataset_clean <- mutate(B_dataset_clean, number_item_PL = as.integer(number_item_PL))


# 3. Count NA, zeros, and negative values for each column
total_elements <- sapply(B_dataset_clean, length)
na_counts <- sapply(B_dataset_clean, function(x) sum(is.na(x)))
zero_counts <- sapply(B_dataset_clean, function(x) if (is.numeric(x)) sum(x == 0, na.rm = TRUE) else 0)
negative_counts <- sapply(B_dataset_clean, function(x) if (is.numeric(x)) sum(x < 0, na.rm = TRUE) else 0)

# 4. Calculate percentages
na_percentage <- (na_counts / total_elements) * 100
zero_percentage <- (zero_counts / total_elements) * 100
negative_percentage <- (negative_counts / total_elements) * 100

# Create a summary table
summary_table <- data.frame(
  Column = names(B_dataset_clean),
  Total_Elements = total_elements,
  NA_Counts = na_counts,
  NA_Percentage = na_percentage,
  Zero_Counts = zero_counts,
  Zero_Percentage = zero_percentage,
  Negative_Counts = negative_counts,
  Negative_Percentage = negative_percentage
)
View(summary_table)



#After tranformation Null => NA. Change NA to 0
B_dataset_clean <- B_dataset_clean %>%
  mutate(
    PL_gross_sales = ifelse(is.na(PL_gross_sales), 0, as.numeric(PL_gross_sales)),
    number_items_other = ifelse(is.na(number_items_other), 0, as.integer(number_items_other)),
    number_item_PL = ifelse(is.na(number_item_PL), 0, as.integer(number_item_PL))
  )

#Check that we have PL sales only when we have PL item
filtered_data <- B_dataset_clean %>%
  filter((PL_gross_sales == 0 & number_item_PL != 0) | 
           (PL_gross_sales != 0 & number_item_PL == 0))
head(filtered_data, 10)

B_dataset_clean$gross_sales <- B_dataset_clean$gross_sales / 100.0
B_dataset_clean$gross_sales <- as.numeric(as.character(B_dataset_clean$gross_sales))
head(B_dataset_clean$gross_sales)
str(B_dataset_clean)

B_dataset_clean$number_items_other <- as.integer(B_dataset_clean$number_items_other)
B_dataset_clean$number_item_PL <- as.integer(B_dataset_clean$number_item_PL)
str(B_dataset_clean)

colSums(is.na(B_dataset_clean)) # Show NA in dataset

duplicates <- duplicated(B_dataset_clean)  # Show duplicates if any
print(sum(duplicates))
View(B_dataset_clean)

#### CALCULATING EXPENSES AND INTEGRATING INTO THE ORIGINAL DATASET ####

# Here we are analyzing data by customer, grouping the data by customer and date
# to create "receipts".


# Create the "Receipts" table
# Group the data by Customer_id and DATE to calculate:
# - The total daily expense per customer.
# - The number of transactions made by a customer on a given day.
receipts <- aggregate(PL_gross_sales, net_sales, gross_sales, number_items_other, number_item_PL ~ id_customer + date, data = B_dataset_clean, FUN = function(x) {
  list(
    daily_total = sum(x, na.rm = TRUE), 
    transactions = length(x)
  )
})


receipts <- B_dataset_clean %>%
  group_by(id_customer, date) %>%
  summarize(
    daily_PL_gross_sales = sum(PL_gross_sales, na.rm = TRUE),
    daily_net_sales = sum(net_sales, na.rm = TRUE),
    daily_gross_sales = sum(gross_sales, na.rm = TRUE),
    daily_number_items_other = sum(number_items_other, na.rm = TRUE),
    daily_number_item_PL = sum(number_item_PL, na.rm = TRUE),
    transactions = n()
  ) %>%
  ungroup()

View(receipts)



############################### clean the sales data ############################### 

# Check net_sales < gross_sales
filtered_rows <- B_dataset_clean %>%
  filter(net_sales > gross_sales)
print(filtered_rows)
count(filtered_rows)
count(filtered_rows)/nrow(B_dataset)
# Delete net_sales > gross_sales
B_dataset_clean <- B_dataset_clean %>%
  filter(!(net_sales > gross_sales)) 

# Check PL_gross_sales <  gross_sales
filtered_rows <- B_dataset_clean %>%
  filter(PL_gross_sales > gross_sales)
print(filtered_rows)
count(filtered_rows)/nrow(B_dataset)
# Delete PL_gross_sales > gross_sales
B_dataset_clean <- B_dataset_clean %>%
  filter(!(PL_gross_sales > gross_sales))

# Delete net_sales < 0.1
B_dataset_clean <- B_dataset_clean %>%
  filter(!(net_sales < 0.1)) 

## Not sure about this one. It is strange data but can we delete it? What is the reason?
## Delete gross_sales < 0.5
##B_dataset_clean <- B_dataset_clean %>%
##  filter(!(gross_sales < 0.5)) 


# Check that at least one item is bought: number_items_other + number_item_PL !=0
filtered_data <- B_dataset_clean %>%
  filter((number_items_other + number_item_PL) == 0)
head(filtered_data, 10)

############################### number_items_other ############################### 

boxplot(B_dataset_clean$number_items_other,
        main = "number_items_other",
        ylab = "number_items_other",
        col = "lightgreen")



############################### number_items_PL ############################### 

boxplot(B_dataset_clean$number_items_PL,
        main = "number_items_PL",
        ylab = "number_items_PL",
        col = "lightgreen")

############################### gross_sales ############################### 

boxplot(log1p(B_dataset_clean$gross_sales),
        main = "Log-Transformed Boxplot of Gross Sales",
        ylab = "Log Gross Sales",
        col = "lightgreen")

############################### PL_gross_sales ############################### 

# Boxplot for PL_gross_sales > 0 (it is valid to have ==0)
B_dataset_clean_filtered <- B_dataset_clean %>%
  filter(PL_gross_sales > 0)

# Boxplot
boxplot(log1p(B_dataset_clean_filtered$PL_gross_sales),
        main = "Log-Transformed Boxplot of PL Gross Sales",
        ylab = "Log PL Gross Sales",
        col = "lightgreen")

# Whiskers: log1p(PL_gross_sales) >= 6. Check but not delete
filtered_values_above_6 <- B_dataset_clean_filtered %>%
  filter(log1p(PL_gross_sales) >= 6)
head(filtered_values_above_6)
print(nrow(filtered_values_above_6))


############################### net_sales  ############################### 

boxplot(log1p(B_dataset_clean$net_sales),
        main = "Log-Transformed Boxplot of Net sales",
        ylab = "Log PL Net Sales",
        col = "lightgreen")

# Whiskers: Filter log1p(net_sales) >= 6. Check but not delete
filtered_values_above_6 <- B_dataset_clean %>%
  filter(log1p(net_sales) >= 6)
head(filtered_values_above_6)
print(nrow(filtered_values_above_6))


############################### net_sales and gross_sales difference ############################### 

# Add new column for net_sales and gross_sales difference
B_dataset_clean <- B_dataset_clean %>%
  mutate(diff_sales = abs(net_sales - gross_sales)) 

# Sort
max_diff_rows <- B_dataset_clean %>%
  arrange(desc(diff_sales))
head(max_diff_rows, 30)

boxplot(log1p(B_dataset_clean$diff_sales),
        main = "Boxplot of max_diff_rows",
        ylab = "max_diff",
        col = "lightblue")

# Whiskers: Filter log1p(diff_sales) >= 6. Check but not delete
filtered_values_above_6 <- B_dataset_clean %>%
  filter(log1p(diff_sales) >= 6)
head(filtered_values_above_6)
print(nrow(filtered_values_above_6))


############################### result of data cleaning ############################### 

percent_lost <- 100 * (nrow(B_dataset) - nrow(B_dataset_clean)) / nrow(B_dataset)
print(percent_lost)
str(B_dataset_clean)
colSums(is.na(B_dataset_clean))
View(B_dataset_clean)

write.csv(B_dataset_clean, "B_dataset_clean_v1.csv", row.names = FALSE)

