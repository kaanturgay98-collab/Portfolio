#libraries 
install.packages("partykit")
install.packages("survminer")
install.packages("fastDummies")
install.packages("gt")


library(dplyr) 
library(readxl)
library(tidyr) 
library(fastDummies) 
library(partykit) 
library(survival) 
library(survminer) 

rfm_analysis <- read_excel("rfm_analysis_final1.xlsx")
dataB=read.csv("B_dataset_clean_v1.csv") ##or read initial dataset and add cleaning code 

str(dataB)
str(rfm_analysis)
# CREATING DATASET FOR CHURN ANALYSIS

# Group by customer and summarize key metrics
last_date <- max(dataB$date)  # Find the most recent date in the dataset

customer_info <- dataB %>%
  group_by(id_customer) %>%
  summarise(
    total_orders = n(),  # Total number of orders per customer
    different_stores = n_distinct(store_id),  # Number of unique stores visited
    main_store_type = store_type[which.max(table(store_type))],  # Most frequently visited store type
    tenure_days = as.numeric(difftime(last_date, min(date), units = "days")),  # Number of days since the first order
    
    # Total sales across categories
    total_PL_sales = sum(PL_gross_sales, na.rm = TRUE), 
    total_net_sales = sum(net_sales, na.rm = TRUE),
    total_gross_sales = sum(gross_sales, na.rm = TRUE),
    
    # Total number of items purchased in each category
    total_items_other = sum(number_items_other, na.rm = TRUE),
    total_item_PL = sum(number_item_PL, na.rm = TRUE),
    
    # Average sales values
    avg_gross_sales = mean(gross_sales, trim = 0.1, na.rm = TRUE),  # Trimmed mean to handle outliers
    avg_items_other = ceiling(mean(number_items_other, trim = 0.1, na.rm = TRUE)),  # Average number of other items
    avg_items_PL = ceiling(mean(number_item_PL, trim = 0.1, na.rm = TRUE)),  # Average number of PL items
    
    .groups = "drop"
  )

# Check the structure of the resulting dataset
str(customer_info)

# Join customer_info with RFM analysis results
data_churn <- customer_info %>%
  left_join(
    rfm_analysis %>%
      select(id_customer, Avg_Interpurchase_Time, R),  # Keep only relevant columns
    by = "id_customer"
  )

# Correlation check
# Select only numeric columns from the dataset
numeric_data <- data_churn %>%
  select(where(is.numeric), -id_customer)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")
print(correlation_matrix)

# Identify pairs of variables with high correlation (> 0.75 or < -0.75)
high_correlation <- which(abs(correlation_matrix) > 0.75 & abs(correlation_matrix) < 1, arr.ind = TRUE)

# Create a data frame with highly correlated variable pairs
correlated_pairs <- data.frame(
  Variable1 = rownames(correlation_matrix)[high_correlation[, 1]],
  Variable2 = colnames(correlation_matrix)[high_correlation[, 2]],
  Correlation = correlation_matrix[high_correlation]
)
print(correlated_pairs)

# Cleaning the dataset by removing highly correlated variables
data_churn <- data_churn %>%
  select(
    -total_PL_sales,
    -total_net_sales,
    -total_items_other,
    -total_item_PL,
    -avg_items_other
  )

# Remove rows with missing values (NAs)
data_churn <- data_churn %>%
  na.omit()

# Add a binary churn column
# Customers with R = "L" (low recency) are considered churners (1)
data_churn <- data_churn %>%
  mutate(
    churn = if_else(R == "L", 1, 0)
  )

# Remove the R column
data_churn <- data_churn %>%
  select(
    -R
  )

# Transform categorical variables into dummy variables
data_churn_dummies <- data_churn %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(
    main_store_type = ifelse(main_store_type == "superstore", 1, 0)  # 1 for superstore, 0 for iperstore
  )

View(data_churn_dummies)

# Count the number of churn and non-churn customers
churn_count <- data_churn_dummies %>%
  group_by(churn) %>%
  summarise(
    Count = n(),  # Count the number of customers in each churn group
    Percentage = n() / nrow(data_churn_dummies) * 100  # Calculate percentage
  )

# Print the result
print(churn_count)

unique(rfm_analysis$Cluster)

################################################################################
####Analysis of private labels in loyalty mechanisms####
################################################################################

# LOAD NECESSARY LIBRARIES
library(dplyr)
library(ggplot2)
# STEP 1: MERGE DATASETS
# Combine data_churn_dummies and rfm_analysis for analysis
# This step merges customer data with RFM analysis data to provide a comprehensive dataset for further exploration. 
# It ensures that both private label metrics (PL) and cluster information are included for each customer.
merged_data <- data_churn_dummies %>%
  left_join(rfm_analysis %>% select(id_customer, Cluster, Monetary_PL, PL_sales_contribution), by = "id_customer")

# STEP 2: ANALYZE PRIVATE LABELS (PL) BY CHURN
# This step calculates average PL sales, items, and contribution for churners and non-churners.
# It helps identify the significance of private labels for retaining or losing customers.
pl_by_churn <- merged_data %>%
  group_by(churn) %>%
  summarise(
    Avg_Monetary_PL = mean(Monetary_PL, na.rm = TRUE),  # Average PL sales
    Avg_PL_Items = mean(avg_items_PL, na.rm = TRUE),   # Average number of PL items
    Avg_PL_Contribution = mean(PL_sales_contribution, na.rm = TRUE),  # Average PL contribution
    Count = n()  # Number of customers
  )
print("Private Labels by Churn")
print(pl_by_churn)

# STEP 3: ANALYZE PRIVATE LABELS (PL) BY RFM CLUSTER
# This step calculates similar metrics as in step 2 but grouped by RFM clusters instead of churn.
# It provides insights into how private label performance varies across different customer segments.
pl_by_cluster <- merged_data %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Monetary_PL = mean(Monetary_PL, na.rm = TRUE),  # Average PL sales
    Avg_PL_Items = mean(avg_items_PL, na.rm = TRUE),   # Average number of PL items
    Avg_PL_Contribution = mean(PL_sales_contribution, na.rm = TRUE),  # Average PL contribution
    Count = n()  # Number of customers
  )
print("Private Labels by RFM Cluster")
print(pl_by_cluster)

# STEP 4: VISUALIZE PRIVATE LABELS BY CHURN
# This visualization compares the average PL monetary value between churners and non-churners.
# It highlights the monetary impact of private labels on customer retention.
ggplot(pl_by_churn, aes(x = factor(churn), y = Avg_Monetary_PL, fill = factor(churn))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average PL Monetary Value by Churn", x = "Churn (0 = No, 1 = Yes)", y = "Average PL Monetary Value") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()

# STEP 5: VISUALIZE PRIVATE LABELS BY RFM CLUSTER
# This visualization ranks clusters by their average monetary PL value to identify high-performing clusters.
# It helps prioritize clusters for private label strategies.
ggplot(pl_by_cluster, aes(x = reorder(Cluster, -Avg_Monetary_PL), y = Avg_Monetary_PL, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average PL Monetary Value by RFM Cluster", x = "RFM Cluster", y = "Average PL Monetary Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# STEP 6: DISTRIBUTION OF CHURNERS ACROSS RFM CLUSTERS
# This step calculates the distribution of churners and non-churners across RFM clusters.
# It identifies which clusters are more prone to churn, helping to target retention efforts.
churn_by_cluster <- merged_data %>%
  group_by(Cluster, churn) %>%
  summarise(
    Count = n(), 
    Avg_Monetary_PL = mean(Monetary_PL, na.rm = TRUE),
    Avg_PL_Contribution = mean(PL_sales_contribution, na.rm = TRUE),
    Avg_PL_Items = mean(avg_items_PL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Percentage = Count / sum(Count) * 100)
print("Churn Distribution by RFM Cluster")
print(churn_by_cluster)

# STEP 7: CONTRIBUTION OF PL TO TOTAL SALES BY CHURN AND CLUSTER
# This step calculates the contribution of private labels to total sales by churn status and cluster.
# It helps understand the importance of PL sales across different customer segments.
pl_contribution <- merged_data %>%
  group_by(churn, Cluster) %>%
  summarise(
    Total_PL_Contribution = sum(Monetary_PL, na.rm = TRUE) / sum(total_gross_sales, na.rm = TRUE) * 100,
    Avg_PL_Contribution = mean(PL_sales_contribution, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  )
print("PL Contribution to Total Sales by Churn and Cluster")
print(pl_contribution)

# Calculate PL contribution to total sales for churners and non-churners
pl_sales_contribution <- merged_data %>%
  group_by(churn) %>%
  summarise(
    Total_PL_Sales = sum(Monetary_PL, na.rm = TRUE),      # Total private label sales
    Total_Sales = sum(total_gross_sales, na.rm = TRUE),   # Total gross sales
    PL_Contribution = (Total_PL_Sales / Total_Sales) * 100 # Contribution of PL to total sales in percentage
  )

# Print the results
print("PL Contribution to Total Sales by Churn")
print(pl_sales_contribution)


# STEP 8: IDENTIFYING OPPORTUNITIES IN "AT RISK" CLUSTERS
# Focuses on "To Reactivate" and "About to Sleep" clusters to identify key opportunities for retention.
# These clusters are critical as they combine risk and potential for high private label sales.
at_risk_clusters <- churn_by_cluster %>%
  filter(Cluster %in% c("To Reactivate", "About to Sleep")) %>%
  arrange(desc(Avg_Monetary_PL))
print("Opportunities in At Risk Clusters")
print(at_risk_clusters)

# Filter for "About to Sleep" and "To Reactivate" clusters
at_risk_churn_analysis <- merged_data %>%
  filter(Cluster %in% c("About to Sleep", "To Reactivate")) %>%
  group_by(Cluster, churn) %>%
  summarise(
    Count = n(),                                # Number of customers in each churn category
    Total_Count = sum(n()),                     # Total customers in the cluster
    Churn_Percentage = (Count / Total_Count) * 100, # Percentage of churners or non-churners
    .groups = "drop"
  )

# Print the results
print("Churn Analysis for 'About to Sleep' and 'To Reactivate'")
print(at_risk_churn_analysis)


# STEP 9: VISUALIZATIONS
# 9.1 Distribution of churners across clusters
# Highlights where churners are most concentrated across RFM clusters.
ggplot(churn_by_cluster, aes(x = reorder(Cluster, -Count), y = Count, fill = factor(churn))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Churners Across RFM Clusters", x = "RFM Cluster", y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), name = "Churn (0 = No, 1 = Yes)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 9.2 Contribution of PL to Total Sales by Cluster
# Visualizes the PL contribution for each RFM cluster, comparing churners and non-churners.
ggplot(pl_contribution, aes(x = reorder(Cluster, -Total_PL_Contribution), y = Total_PL_Contribution, fill = factor(churn))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "PL Contribution to Total Sales by Cluster", x = "RFM Cluster", y = "PL Contribution (%)") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), name = "Churn (0 = No, 1 = Yes)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 9.3 Opportunities in At Risk Clusters
# Highlights the monetary potential of private labels in "at-risk" clusters.
ggplot(at_risk_clusters, aes(x = reorder(Cluster, -Avg_Monetary_PL), y = Avg_Monetary_PL, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Opportunities in At Risk Clusters", x = "RFM Cluster", y = "Average Monetary PL Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


################################################################################
####sum up insights#####
################################################################################
# Merge data for analysis
merged_data <- data_churn_dummies %>%
  left_join(rfm_analysis %>% select(id_customer, Cluster, Monetary_PL, PL_sales_contribution), by = "id_customer")

# 1. Private Labels as a Loyalty Driver: Churn vs Non-Churn
pl_by_churn <- merged_data %>%
  group_by(churn) %>%
  summarise(
    Avg_Monetary_PL = mean(Monetary_PL, na.rm = TRUE),  # Average PL spend
    PL_Contribution = sum(Monetary_PL, na.rm = TRUE) / sum(total_gross_sales, na.rm = TRUE) * 100,  # PL contribution to total sales
    Count = n()  # Number of customers
  )

print("PL as a Loyalty Driver (Churn vs Non-Churn)")
print(pl_by_churn)

# 2. Cluster-Specific Findings: Champions and Loyal Clusters
pl_by_cluster <- merged_data %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Monetary_PL = mean(Monetary_PL, na.rm = TRUE),  # Average PL spend
    Avg_PL_Contribution = mean(PL_sales_contribution, na.rm = TRUE),  # Average PL contribution
    Avg_PL_Items = mean(avg_items_PL, na.rm = TRUE),  # Average PL items purchased
    Count = n()  # Number of customers
  ) %>%
  arrange(desc(Avg_Monetary_PL))

print("PL Engagement by RFM Cluster")
print(pl_by_cluster)

# 3. Insights for At-Risk Clusters: To Reactivate and About to Sleep
at_risk_clusters <- merged_data %>%
  filter(Cluster %in% c("To Reactivate", "About to Sleep")) %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Monetary_PL = mean(Monetary_PL, na.rm = TRUE),
    Avg_PL_Contribution = mean(PL_sales_contribution, na.rm = TRUE),
    Avg_PL_Items = mean(avg_items_PL, na.rm = TRUE),
    Count = n()
  )

print("Insights for At-Risk Clusters")
print(at_risk_clusters)

# 4. Unknown Churn Cluster: Highlight Potential
unknown_churn <- merged_data %>%
  filter(Cluster == "Unknown") %>%
  group_by(churn) %>%
  summarise(
    Avg_Monetary_PL = mean(Monetary_PL, na.rm = TRUE),
    PL_Contribution = sum(Monetary_PL, na.rm = TRUE) / sum(total_gross_sales, na.rm = TRUE) * 100,
    Avg_PL_Items = mean(avg_items_PL, na.rm = TRUE),
    Count = n()
  )

print("Insights for Unknown Churn Cluster")
print(unknown_churn)






















