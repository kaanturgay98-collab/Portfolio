###############################################################################
# LIBRARIES
###############################################################################
install.packages("partykit")
install.packages("survminer")
install.packages("fastDummies")

library(dplyr) 
library(readxl)
library(tidyr) 
library(fastDummies) 
library(partykit) 
library(survival) 
library(survminer)
library(xgboost)
library(pROC)
library(pdp)
library(ggplot2)
library(data.table)

###############################################################################
# DATA IMPORT AND PREPARATION
###############################################################################
rfm_analysis <- read_excel("rfm_analysis.xlsx")
dataB = read.csv("B_dataset_clean_v1.csv")  # or read the initial dataset and add cleaning code 

# CREATING DATASET FOR CHURN ANALYSIS

# Create a dataset with customer information
last_date <- max(dataB$date)

customer_info <- dataB %>%
  group_by(id_customer) %>%
  summarise(
    total_orders = n(),  # Number of orders
    different_stores = n_distinct(store_id),  # Number of unique stores
    main_store_type = store_type[which.max(table(store_type))],  # Primary store type
    tenure_days = as.numeric(difftime(last_date, min(date), units = "days")),  # Customer tenure duration
    
    # Sales totals
    total_PL_sales = sum(PL_gross_sales, na.rm = TRUE), 
    # total_net_sales = sum(net_sales, na.rm = TRUE),
    total_gross_sales = sum(gross_sales, na.rm = TRUE),
    
    # Total items by category
    total_items_other = sum(number_items_other, na.rm = TRUE),
    total_item_PL = sum(number_item_PL, na.rm = TRUE),
    
    # Average sales
    avg_gross_sales = mean(gross_sales, trim = 0.1, na.rm = TRUE),  # Trimmed mean to reduce outliers
    avg_items_other = ceiling(mean(number_items_other, trim = 0.1, na.rm = TRUE)),
    avg_items_PL = ceiling(mean(number_item_PL, trim = 0.1, na.rm = TRUE)),
    
    .groups = "drop"
  )

# Inspect the resulting dataset
str(customer_info)

# Merge customer info with RFM analysis results
data_churn_full <- customer_info %>%
  left_join(
    rfm_analysis %>%
      select(id_customer, Avg_Interpurchase_Time, R),
    by = "id_customer"
  )

# Add a binary churn column
data_churn_full <- data_churn_full %>%
  mutate(
    churn = if_else(R == "L", 1, 0)
  ) %>%
  select(-R)

# Check and remove NAs
summary(data_churn_full)
anyNA(data_churn_full)  # Check for missing values
data_churn_full <- data_churn_full %>% na.omit()
View(data_churn_full)

###############################################################################
# CORRELATION CHECK
###############################################################################
numeric_data <- data_churn_full %>%
  select(where(is.numeric), -id_customer)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Correlation with churn
churn_correlation <- correlation_matrix["churn", , drop = FALSE]
print(churn_correlation)  # No high correlation with churn

# Identify highly correlated variable pairs (> 0.75 or < -0.75)
high_correlation <- which(abs(correlation_matrix) > 0.75 & abs(correlation_matrix) < 1, arr.ind = TRUE)
correlated_pairs <- data.frame(
  Variable1 = rownames(correlation_matrix)[high_correlation[, 1]],
  Variable2 = colnames(correlation_matrix)[high_correlation[, 2]],
  Correlation = correlation_matrix[high_correlation]
)
print(correlated_pairs)

data_churn <- data_churn_full %>%
  select(
    -total_gross_sales,
    -total_PL_sales,
    -total_items_other,
    -avg_items_other
  )
cor_matrix <- cor(data_churn %>% select(-c(id_customer, main_store_type, churn)), use = "complete.obs")
print(cor_matrix)  # No more correlation

View(data_churn)

###############################################################################
# DUMMY VARIABLES FOR CATEGORICAL
###############################################################################

data_churn_dummies <- data_churn %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(
    main_store_type = ifelse(main_store_type == "superstore", 1, 0)  # 1 for superstore, 0 for iperstore
  )

# View the transformed dataset
View(data_churn_dummies)

###############################################################################
# TRAIN-TEST SPLIT
###############################################################################
set.seed(123) 
sample_indices <- sample(c(TRUE, FALSE), nrow(data_churn_dummies), replace = TRUE, prob = c(0.7, 0.3))
train <- data_churn_dummies[sample_indices, ]
test <- data_churn_dummies[!sample_indices, ]

# Checking proportions between classes
churn_counts <- table(data_churn_dummies$churn)
churn_proportions <- prop.table(churn_counts)

churn_proportions
#         0         1 
# 0.8811212 0.1188788 

# The dataset is too unbalanced between classes
# Dataset balancing techniques will be used before each analysis 

###############################################################################
# LOGISTIC REGRESSION WITH UNDERSAMPLING
###############################################################################

# 1. Random Undersampling on the Training Set

# Separate majority (churn = 0) and minority (churn = 1) classes
train_majority <- train[train$churn == 0, ]
train_minority <- train[train$churn == 1, ]

# Randomly sample from the majority class to match the minority class size
set.seed(123)  # for reproducibility
train_majority_undersampled <- train_majority[sample(
  nrow(train_majority),
  nrow(train_minority),
  replace = FALSE
), ]

# Combine undersampled majority with minority
train_undersampled <- rbind(train_minority, train_majority_undersampled)

# 2. Fit the Logistic Regression Model on the Undersampled Training Set
logit_model_undersampled <- glm(
  churn ~ . - id_customer,
  data   = train_undersampled,
  family = binomial
)

summary(logit_model_undersampled)

# Calculate (pseudo) R-squared for logistic regression
r2Log <- function(model) {
  s <- summary(model)
  1 - (s$deviance / s$null.deviance)
}

r2_undersampled <- r2Log(logit_model_undersampled)
cat("Pseudo R2:", r2_undersampled, "\n")  #R2: 0.2257618

# 3. Predict on the Test Set and Evaluate
test$churn <- as.character(test$churn)
test$churn[test$churn == "No"]  <- "0"
test$churn[test$churn == "Yes"] <- "1"
test$churn <- as.numeric(test$churn)

# Get predicted probabilities from the undersampled model
fitted_prob <- predict(logit_model_undersampled, newdata = test, type = "response")

# Classify using threshold = 0.5
fitted_class <- ifelse(fitted_prob > 0.5, 1, 0)

# 4. Confusion Matrix & Metrics
tab <- table(Predicted = fitted_class, Actual = test$churn)
cat("\nConfusion Matrix:\n")
print(tab)

# Extract elements from confusion matrix
TP <- tab["1", "1"]  # True Positives
TN <- tab["0", "0"]  # True Negatives
FP <- tab["1", "0"]  # False Positives
FN <- tab["0", "1"]  # False Negatives

# Safety check for missing cells
if (is.na(TP)) TP <- 0
if (is.na(TN)) TN <- 0
if (is.na(FP)) FP <- 0
if (is.na(FN)) FN <- 0

# Accuracy
accuracy_logistic_undersampled <- (TP + TN) / sum(tab)

# Precision
precision <- if ((TP + FP) == 0) NA else TP / (TP + FP)

# Recall (Sensitivity, TPR)
recall <- if ((TP + FN) == 0) NA else TP / (TP + FN)

# F1 Score
f1_score <- if (is.na(precision) | is.na(recall) | (precision + recall == 0)) {
  NA 
} else {
  2 * (precision * recall) / (precision + recall)
}

# Print metrics
cat("\nMetrics:\n")
cat("Accuracy :", accuracy_logistic_undersampled, "\n") # Accuracy : 0.6789718 
cat("Precision:", precision, "\n")                      # Precision: 0.2531034   
cat("Recall   :", recall, "\n")                         # Recall   : 0.80131 
cat("F1 Score :", f1_score, "\n")                       # F1 score : 0.384696

# CONCLUSIONS
# Low Accuracy and Precision, another model could perform better

###############################################################################
# DECISION TREE
###############################################################################

# 1. Detect Smaller Class vs. Larger Class in 'train' and Perform Undersampling
train$churn <- as.character(train$churn)
test$churn  <- as.character(test$churn)

num_zero <- sum(train$churn == "0", na.rm = TRUE)
num_one  <- sum(train$churn == "1", na.rm = TRUE)

if (num_zero <= num_one) {
  minority_label <- "0"
  majority_label <- "1"
} else {
  minority_label <- "1"
  majority_label <- "0"
}

train_minority <- train[train$churn == minority_label, ]
train_majority <- train[train$churn == majority_label, ]

if (nrow(train_majority) > nrow(train_minority)) {
  set.seed(123)  
  train_majority_undersampled <- train_majority[
    sample(
      x       = nrow(train_majority),
      size    = nrow(train_minority),
      replace = FALSE
    ),
  ]
  train_balanced <- rbind(train_minority, train_majority_undersampled)
} else {
  train_balanced <- train
}

# 2. Convert churn to Factor in both train & test
train_balanced$churn <- factor(train_balanced$churn, levels = c("0","1"))
test$churn           <- factor(test$churn,           levels = c("0","1"))

# 3. Fit ctree Model on the Balanced Data
ctree_model <- ctree(
  churn ~ . - id_customer,
  data = train_balanced
)
plot(ctree_model)

# 4. Predict on the Original Test Set
test_no_id <- test %>% select(-id_customer)

# By default, predict() returns class labels
predicted_classes <- predict(ctree_model, newdata = test_no_id)

# 5. Confusion Matrix & Metrics
conf_matrix <- table(
  Predicted = predicted_classes,
  Actual    = test_no_id$churn
)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

TP <- conf_matrix["1", "1"]
FP <- conf_matrix["1", "0"]
FN <- conf_matrix["0", "1"]
precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)

cat("\nAccuracy :", accuracy,      # Accuracy : 0.7497949 
    "\nPrecision:", precision,     # Precision: 0.2928377
    "\nRecall   :", recall, "\n")  # Recall   : 0.7052402 

# CONCLUSIONS
# Performing better in terms of Accuracy and Precision, but much lower Recall
# Attempt with another model

###############################################################################
# METHOD 3. XGBoost WITH UNDERSAMPLING
###############################################################################

# 1. Handle Imbalance by Undersampling
train$churn <- as.character(train$churn)
test$churn  <- as.character(test$churn)

minority_label <- ifelse(sum(train$churn == "0") <= sum(train$churn == "1"), "0", "1")
majority_label <- ifelse(minority_label == "0", "1", "0")

train_minority <- train[train$churn == minority_label, ]
train_majority <- train[train$churn == majority_label, ]

if (nrow(train_majority) > nrow(train_minority)) {
  set.seed(123)
  train_majority_undersampled <- train_majority[sample(seq_len(nrow(train_majority)), nrow(train_minority), replace = FALSE), ]
  train_balanced <- rbind(train_minority, train_majority_undersampled)
} else {
  train_balanced <- train
}

# 2. Convert churn back to numeric for XGBoost
train_balanced$churn <- as.numeric(train_balanced$churn)
test$churn           <- as.numeric(test$churn)

# 3. Prepare Matrices for XGBoost (Remove id_customer)
train_no_id <- train_balanced %>% select(-id_customer)
test_no_id  <- test %>% select(-id_customer)

train_matrix <- model.matrix(churn ~ . - 1, data = train_no_id)
train_labels <- train_no_id$churn
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)

# 4. Train XGBoost Model
set.seed(123)
xgb_model <- xgboost(data = dtrain, max.depth = 5, nrounds = 200, objective = "binary:logistic", verbose = 0)

# 5. Predict on Test Set
test_matrix <- model.matrix(churn ~ . - 1, data = test_no_id)
pred_probs <- predict(xgb_model, newdata = test_matrix)

# 6. Confusion Matrix & Metrics
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)
conf_matrix_xgb <- table(Predicted = pred_classes, Actual = test_no_id$churn)

cat("Confusion Matrix:\n", print(conf_matrix_xgb), "\n")

accuracy_xgb <- sum(diag(conf_matrix_xgb)) / sum(conf_matrix_xgb)
TP <- conf_matrix_xgb["1", "1"]
FP <- conf_matrix_xgb["1", "0"]
FN <- conf_matrix_xgb["0", "1"]

precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)

cat("\nAccuracy:", accuracy_xgb, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

# 7. ROC & AUC
roc_curve <- roc(test_no_id$churn, pred_probs)
plot(roc_curve, col = "blue", main = "ROC Curve for XGBoost Model")
cat("AUC:", auc(roc_curve), "\n")

# 8. Variable Importance
importance <- xgb.importance(model = xgb_model)
print(importance)
xgb.plot.importance(importance, top_n = 10, main = "Top 10 Important Variables")

# 9. SHAP Value Calculation
shap_values <- predict(xgb_model, newdata = test_matrix, predcontrib = TRUE)
shap_values_df <- as.data.table(shap_values)[, -"BIAS", with = FALSE]
colnames(shap_values_df) <- colnames(test_matrix)

# 10. SHAP Value Summary
shap_summary <- shap_values_df[, lapply(.SD, function(x) list(mean = mean(x), max = max(x), min = min(x)))]
shap_summary

# 11. SHAP Dependence Plots
library(ggplot2)

# Define a function for creating SHAP dependence plots
create_shap_plot <- function(feature, shap_values_df, test_matrix) {
  shap_dep_data <- data.table(Feature = test_matrix[[feature]], SHAP_Value = shap_values_df[[feature]])
  ggplot(shap_dep_data, aes(x = Feature, y = SHAP_Value)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", color = "blue", se = FALSE) +
    theme_minimal() +
    labs(title = paste("SHAP Dependence Plot for", feature), x = feature, y = "SHAP Value")
}

# Create SHAP dependence plots for the specified features
plots <- list(
  create_shap_plot("total_orders", shap_values_df, test_matrix),
  create_shap_plot("Avg_Interpurchase_Time", shap_values_df, test_matrix),
  create_shap_plot("avg_gross_sales", shap_values_df, test_matrix),
  create_shap_plot("total_item_PL", shap_values_df, test_matrix)
)

# Arrange the plots in a 2x2 grid
library(gridExtra)
grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], ncol = 2, nrow = 2)


# CONCLUSIONS
# Accuracy and Precision > Decision Tree
# Recall < Logistic Regression, but only by 0,03

# This model is the best one in terms of overall performances

# MOST IMPORTANT VARIABLES IN PREDICTING CHURN
#   1. Total Orders                 -> overall negative effect on churn prob
#   2. Average Interpurchase Time   -> overall positive effect on churn prob
#   3. Average Gross Sales          -> ambiguous
#   4. Total Items PL               -> overall negative effect on churn prob
#   5. Tenure days                  -> overall positive effect on churn prob

# Tenure days could be used as a time variable to see how the probability of 
# churning changes over time depending on the other four through a survival analysis

###############################################################################
# METHOD 4. SURVIVAL ANALYSIS
###############################################################################

# Left join with rfm_analysis to add the "F" column
data_survival <- data_churn_dummies %>%
  left_join(rfm_analysis %>% select(id_customer, F), by = "id_customer")

# Transform "F" into a dummy variable
data_survival$F <- recode(data_survival$F, "L" = 1, "M" = 2, "H" = 3)

# Function to categorize into low, medium, and high
categorize <- function(values, breaks, labels) {
  cut(values, breaks = breaks, labels = labels, include.lowest = TRUE)
}

# Categorize Avg_Interpurchase_Time using predefined bins
data_survival$Avg_Interpurchase_Time <- cut(
  data_survival$Avg_Interpurchase_Time,
  breaks = c(-Inf, 5, 10, Inf),
  labels = c(1, 2, 3),
  include.lowest = TRUE
)

# Define quantile-based bins for avg_gross_sales and total_item_PL
avg_gross_sales_bins <- quantile(data_survival$avg_gross_sales, probs = c(0, 0.25, 0.75, 1), na.rm = TRUE)
total_item_PL_bins <- quantile(data_survival$total_item_PL, probs = c(0, 0.25, 0.75, 1), na.rm = TRUE)

# Categorize avg_gross_sales
data_survival$avg_gross_sales <- cut(
  data_survival$avg_gross_sales,
  breaks = avg_gross_sales_bins,
  labels = c(1, 2, 3),
  include.lowest = TRUE
)

# Categorize total_item_PL
data_survival$total_item_PL <- cut(
  data_survival$total_item_PL,
  breaks = total_item_PL_bins,
  labels = c(1, 2, 3),
  include.lowest = TRUE
)

# Display the final dataset
head(data_survival)

# Create the Surv object
surv_object <- Surv(time = data_churn_dummies$tenure_days, event = data_churn_dummies$churn)

# Survival analysis and visualization for total_orders
km_fit_group_orders <- survfit(surv_object ~ F, data = data_survival)
ggsurvplot(
  km_fit_group_orders, conf.int = TRUE,
  xlab = "Tenure (Days)",
  ylab = "Probability of surviving by total_orders",
  title = "Survival Curve for Churn Based on Total Orders",
  xlim = c(100, 370),  
  ylim = c(0.25, 1)
)

# Survival analysis and visualization for Avg_Interpurchase_Time
km_fit_group_interpurchase <- survfit(surv_object ~ Avg_Interpurchase_Time, data = data_survival)
ggsurvplot(
  km_fit_group_interpurchase, conf.int = TRUE,
  xlab = "Tenure (Days)",
  ylab = "Probability of surviving by Avg_Interpurchase_Time",
  title = "Survival Curve for Churn Based on Avg Interpurchase Time",
  xlim = c(100, 370),  
  ylim = c(0.25, 1)
)

# Survival analysis and visualization for avg_gross_sales
km_fit_group_gross_sales <- survfit(surv_object ~ avg_gross_sales, data = data_survival)
ggsurvplot(
  km_fit_group_gross_sales, conf.int = TRUE,
  xlab = "Tenure (Days)",
  ylab = "Probability of surviving by avg_gross_sales",
  title = "Survival Curve for Churn Based on Average Gross Sales",
  xlim = c(100, 370),  
  ylim = c(0.25, 1)
)

# Survival analysis and visualization for total_item_PL
km_fit_group_item_PL <- survfit(surv_object ~ total_item_PL, data = data_survival)
ggsurvplot(
  km_fit_group_item_PL, conf.int = TRUE,
  xlab = "Tenure (Days)",
  ylab = "Probability of surviving by total_item_PL",
  title = "Survival Curve for Churn Based on Total Items Purchased (PL)",
  xlim = c(100, 370),  
  ylim = c(0.25, 1)
)
