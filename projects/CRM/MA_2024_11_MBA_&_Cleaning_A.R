##############################################################################
# 1) IMPORT AND INITIAL EXPLORATION
##############################################################################

data_A <- read.csv("dataset_CRM_typeA.csv")

names(data_A)
str(data_A)

##############################################################################
# 2) DATA CLEANING: GENERIC CLEANING FUNCTION
##############################################################################

clean_data <- function(dataset) {
  # Replace "NULL" with NA in all columns
  dataset <- data.frame(lapply(dataset, function(x) {
    x[x == "NULL"] <- NA
    return(x)
  }))
  
  # Convert columns to numeric or date formats where appropriate
  dataset <- data.frame(lapply(dataset, function(x) {
    # If column contains only numbers, commas, or dots, convert to numeric (replace commas with dots)
    if (all(grepl("^[0-9,.]+$", na.omit(x)))) {
      return(as.numeric(gsub(",", ".", x)))
    }
    # If column values match YYYY-MM-DD, convert to Date
    if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", na.omit(x)))) {
      return(as.Date(x))
    }
    # Otherwise, keep the original format
    return(x)
  }))
  
  # Count NA, zeros (for numeric columns), and negative values (for numeric columns)
  total_elements <- sapply(dataset, length)
  na_counts <- sapply(dataset, function(x) sum(is.na(x)))
  zero_counts <- sapply(dataset, function(x) if (is.numeric(x)) sum(x == 0, na.rm = TRUE) else 0)
  negative_counts <- sapply(dataset, function(x) if (is.numeric(x)) sum(x < 0, na.rm = TRUE) else 0)
  
  # Compute percentages for each count
  na_percentage <- (na_counts / total_elements) * 100
  zero_percentage <- (zero_counts / total_elements) * 100
  negative_percentage <- (negative_counts / total_elements) * 100
  
  # Create a summary table
  summary_table <- data.frame(
    Colonna = names(dataset),
    Totale_Elementi = total_elements,
    NA_Counts = na_counts,
    Percentuale_NA = na_percentage,
    Zero_Counts = zero_counts,
    Percentuale_Zeri = zero_percentage,
    Negative_Counts = negative_counts,
    Percentuale_Valori_Negativi = negative_percentage
  )
  
  return(list(cleaned_data = dataset, summary_table = summary_table))
}

##############################################################################
# 3) APPLY THE CLEANING FUNCTION TO data_A
##############################################################################

result_A <- clean_data(data_A)
data_A_cleaned <- result_A$cleaned_data
summary_A <- result_A$summary_table

print("Tabella di riepilogo per data_A:")
View(summary_A)

##############################################################################
# 4) IDENTIFY INCONSISTENT ROWS
##############################################################################

inconsistent_rows <- data_A_cleaned[
  data_A_cleaned$UNITS == 0 & !is.na(data_A_cleaned$price.per.unit), 
]

print(paste("Numero di righe inconsistenti trovate:", nrow(inconsistent_rows)))
View(inconsistent_rows)

##############################################################################
# 5) REMOVE INCONSISTENT ROWS
##############################################################################

print(paste("Numero di righe prima della rimozione:", nrow(data_A)))
data_A <- data_A[!(data_A$UNITS == 0 & !is.na(data_A$price.per.unit)), ]
print(paste("Numero di righe dopo la rimozione:", nrow(data_A)))

##############################################################################
# 6) IMPUTE BRAND AND SUB.BRAND BASED ON Item_id
##############################################################################

impute_na_with_item_id <- function(dataset, column_name, fallback_value = "SENZA MARCA") {
  # Save original column values for later analysis
  dataset[[paste0("Original_", column_name)]] <- dataset[[column_name]]
  
  unique_values_per_item <- aggregate(dataset[[column_name]] ~ dataset$Item_id, 
                                      data = dataset, 
                                      function(x) {
                                        unique_values <- unique(na.omit(x))
                                        return(unique_values)
                                      })
  
  colnames(unique_values_per_item) <- c("Item_id", "Unique_Values")
  value_lookup <- setNames(unique_values_per_item$Unique_Values, unique_values_per_item$Item_id)
  
  dataset[[column_name]] <- apply(dataset, 1, function(row) {
    item_id <- as.character(row["Item_id"])
    value <- row[column_name]
    
    if (is.na(value)) {
      if (item_id %in% names(value_lookup)) {
        unique_values <- sort(unlist(value_lookup[[item_id]]))
        if (length(unique_values) == 0) {
          return(fallback_value)
        } else if (length(unique_values) == 1) {
          return(unique_values[1])
        } else {
          return(unique_values[1])
        }
      } else {
        return(fallback_value)
      }
    } else {
      return(value)
    }
  })
  
  return(dataset)
}

data_A <- impute_na_with_item_id(data_A, "BRAND")
data_A <- impute_na_with_item_id(data_A, "SUB.BRAND")

##############################################################################
# 7) ANALYZE CHANGES MADE BY IMPUTATION
##############################################################################

brand_changes <- data_A[data_A$Original_BRAND != data_A$BRAND & !is.na(data_A$Original_BRAND), ]
subbrand_changes <- data_A[data_A$Original_SUB.BRAND != data_A$SUB.BRAND & !is.na(data_A$Original_SUB.BRAND), ]

brand_changes_count <- nrow(brand_changes)
subbrand_changes_count <- nrow(subbrand_changes)

changes_summary <- data.frame(
  Colonna = c("BRAND", "SUB.BRAND"),
  Modifiche = c(brand_changes_count, subbrand_changes_count)
)

##############################################################################
# 8) REMOVE AUXILIARY "ORIGINAL" COLUMNS
##############################################################################

data_A <- data_A[, !colnames(data_A) %in% c("Original_BRAND", "Original_SUB.BRAND")]

##############################################################################
# 9) CALCULATE TOTAL SPEND AND CREATE "Scontrini" TABLE
##############################################################################

data_A$Totale_Spesa <- with(data_A, ifelse(!is.na(price.per.unit), price.per.unit * UNITS, price.per.kg * KG))

scontrini <- aggregate(Totale_Spesa ~ Customer_id + DATE, data = data_A, FUN = function(x) {
  list(
    totale_giornaliero = sum(x, na.rm = TRUE), 
    transazioni = length(x)
  )
})

scontrini <- do.call(data.frame, scontrini)
colnames(scontrini) <- c("Customer_id", "DATE", "Totale_Spesa_Scontrino", "Numero_Transazioni_Scontrino")
scontrini$Totale_Spesa_Scontrino <- as.numeric(scontrini$Totale_Spesa_Scontrino)

data_A <- merge(data_A, scontrini, by = c("Customer_id", "DATE"), all.x = TRUE)
unique(data_A$Numero_Transazioni_Scontrino)
data_A$Numero_Transazioni_Scontrino <- as.numeric(unlist(data_A$Numero_Transazioni_Scontrino))

##############################################################################
# 9 BIS) FREQUENCY OF EACH CATEGORY ACROSS SCONTRINI
##############################################################################
# This section calculates the percentage of scontrini (based on "Customer_id + DATE")
# in which each unique category appears, then plots these percentages.

# Create a unique scontrino identifier
scontrino_id <- paste(data_A$Customer_id, data_A$DATE, sep = "_")
total_scontrini <- length(unique(scontrino_id))

# Count how many scontrini contain each category
unique_categories <- unique(data_A$CATEGORY)
presenza_categoria <- sapply(unique_categories, function(cat) {
  scontrini_cat <- unique(scontrino_id[data_A$CATEGORY == cat])
  return(length(scontrini_cat))
})

# Calculate percentage per category
percentuale_categoria <- 100 * presenza_categoria / total_scontrini

# Combine into a data frame and sort by descending percentage
category_presence <- data.frame(
  CATEGORY = unique_categories,
  Scontrini_Con_Presenza = presenza_categoria,
  Percentuale_Scontrini = percentuale_categoria
)
category_presence <- category_presence[order(category_presence$Percentuale_Scontrini, 
                                             decreasing = TRUE), ]

print("Percentage of Scontrini that contain each CATEGORY:")
print(category_presence)

# Plot as a bar chart
barplot(
  category_presence$Percentuale_Scontrini,
  names.arg = category_presence$CATEGORY,
  las = 2,
  cex.names = 0.7,
  main = "Percentage of Scontrini with Each Category",
  ylab = "Percentage"
)

##############################################################################
# 10) MARKET BASKET ANALYSIS (MBA) USING Item_id
##############################################################################

library(arules)
library(arulesViz)

data_A_transazioni <- split(data_A$Item_id, paste(data_A$Customer_id, data_A$DATE, sep = "_"))
transazioni <- as(data_A_transazioni, "transactions")

support_values <- c(0.0005, 0.001, 0.005)
confidence_values <- c(0.6, 0.7, 0.8)

results <- data.frame(Support = numeric(), Confidence = numeric(), Num_Rules = integer())

for (supp in support_values) {
  for (conf in confidence_values) {
    regole <- apriori(transazioni, parameter = list(supp = supp, conf = conf))
    num_rules <- length(regole)
    results <- rbind(results, data.frame(Support = supp, Confidence = conf, Num_Rules = num_rules))
  }
}

print(results)

selected_supp <- 0.0005
selected_conf <- 0.8

item_category_map <- unique(data_A[, c('Item_id', 'CATEGORY', 'BRAND')])
item_labels <- paste(item_category_map$Item_id, item_category_map$CATEGORY, item_category_map$BRAND, sep = " - ")
labels <- setNames(as.character(item_labels), as.character(item_category_map$Item_id))
transazioni@itemInfo$labels <- labels[transazioni@itemInfo$labels]

regole_finali <- apriori(transazioni, parameter = list(supp = selected_supp, conf = selected_conf))
summary(regole_finali)

regole_ord <- sort(regole_finali, by = "lift", decreasing = TRUE)
inspect(head(regole_ord, 10))

plot(regole_ord, method = "graph", engine = "htmlwidget")

##############################################################################
# 11) MARKET BASKET ANALYSIS (MBA) USING CATEGORY
##############################################################################

data_A_transazioni_category <- split(data_A$CATEGORY, paste(data_A$Customer_id, data_A$DATE, sep = "_"))
transazioni_category <- as(data_A_transazioni_category, "transactions")

support_values_category <- c(0.02, 0.025, 0.05)
confidence_values_category <- c(0.6, 0.7, 0.8, 0.85)

results_category <- data.frame(Support = numeric(), Confidence = numeric(), Num_Rules = integer())

for (supp in support_values_category) {
  for (conf in confidence_values_category) {
    regole_category <- apriori(transazioni_category, parameter = list(supp = supp, conf = conf))
    num_rules <- length(regole_category)
    results_category <- rbind(results_category, data.frame(Support = supp, Confidence = conf, Num_Rules = num_rules))
  }
}

print(results_category)

selected_supp_category <- 0.025
selected_conf_category <- 0.7

regole_category <- apriori(transazioni_category, parameter = list(supp = selected_supp_category, conf = selected_conf_category))
summary(regole_category)

regole_category_ord <- sort(regole_category, by = "lift", decreasing = TRUE)
inspect(head(regole_category_ord, 10))

plot(regole_category_ord, method = "graph", engine = "htmlwidget")

##############################################################################
# 11 BIS) MBA PER CATEGORY (EXCLUDING "MELE" AND "AGRUMI" AND "BANANE")
##############################################################################
# Exclude categories "MELE", "AGRUMI", and "BANANE", then run MBA by CATEGORY.

data_A_no_mele_agrumi <- subset(data_A, !(CATEGORY %in% c("MELE", "AGRUMI", "BANANE")))

data_A_transazioni_category_no_mele_agrumi <- split(
  data_A_no_mele_agrumi$CATEGORY, 
  paste(data_A_no_mele_agrumi$Customer_id, data_A_no_mele_agrumi$DATE, sep = "_")
)
transazioni_category_no_mele_agrumi <- as(data_A_transazioni_category_no_mele_agrumi, "transactions")

support_values_category_excl <- c(0.02, 0.025, 0.05)
confidence_values_category_excl <- c(0.6, 0.7, 0.8, 0.85)

results_category_excl <- data.frame(Support = numeric(), Confidence = numeric(), Num_Rules = integer())

for (supp in support_values_category_excl) {
  for (conf in confidence_values_category_excl) {
    regole_category_excl <- apriori(
      transazioni_category_no_mele_agrumi, 
      parameter = list(supp = supp, conf = conf)
    )
    num_rules_excl <- length(regole_category_excl)
    results_category_excl <- rbind(
      results_category_excl,
      data.frame(Support = supp, Confidence = conf, Num_Rules = num_rules_excl)
    )
  }
}

cat("\n------ CATEGORY EXCLUDING MELE & AGRUMI: SENSITIVITY ANALYSIS ------\n")
print(results_category_excl)

selected_supp_category_excl <- 0.02
selected_conf_category_excl <- 0.7

regole_category_excl <- apriori(
  transazioni_category_no_mele_agrumi,
  parameter = list(supp = selected_supp_category_excl, conf = selected_conf_category_excl)
)

summary(regole_category_excl)

regole_category_excl_ord <- sort(regole_category_excl, by = "lift", decreasing = TRUE)
inspect(head(regole_category_excl_ord, 10))

plot(regole_category_excl_ord, method = "graph", engine = "htmlwidget")

##############################################################################
# 11 TRIS) MBA PER CATEGORY 
# (EXCLUDING "MELE", "AGRUMI" AND ALSO
# "FORMAGGI FRESCHI", "PANE E SOSTITUTIVI",
# "SALUMI LIBERO SERVIZIO", "PASTA FRESCA")
##############################################################################
# Further exclusion: remove additional categories from data_A_no_mele_agrumi

additional_exclusions <- c(
  "FORMAGGI FRESCHI",
  "PANE E SOSTITUTIVI",
  "SALUMI LIBERO SERVIZIO",
  "PASTA FRESCA"
)

data_A_no_mele_agrumi_others <- subset(
  data_A_no_mele_agrumi, 
  !(CATEGORY %in% additional_exclusions)
)

data_A_transazioni_category_no_mele_agrumi_others <- split(
  data_A_no_mele_agrumi_others$CATEGORY,
  paste(data_A_no_mele_agrumi_others$Customer_id, data_A_no_mele_agrumi_others$DATE, sep = "_")
)
transazioni_category_no_mele_agrumi_others <- as(
  data_A_transazioni_category_no_mele_agrumi_others,
  "transactions"
)

support_values_tris <- c(0.0025, 0.00275, 0.003)
confidence_values_tris <- c(0.7, 0.75, 0.775, 0.85)

results_category_tris <- data.frame(Support = numeric(),
                                    Confidence = numeric(),
                                    Num_Rules = integer())

for (supp in support_values_tris) {
  for (conf in confidence_values_tris) {
    regole_category_tris <- apriori(
      transazioni_category_no_mele_agrumi_others,
      parameter = list(supp = supp, conf = conf)
    )
    num_rules_tris <- length(regole_category_tris)
    results_category_tris <- rbind(
      results_category_tris,
      data.frame(Support = supp, Confidence = conf, Num_Rules = num_rules_tris)
    )
  }
}

cat("\n------ CATEGORY EXCLUDING MELE, AGRUMI, FORMAGGI FRESCHI, PANE, SALUMI L.S., PASTA FRESCA: SENSITIVITY ANALYSIS ------\n")
print(results_category_tris)

selected_supp_category_tris <- 0.00275
selected_conf_category_tris <- 0.775

regole_category_tris_final <- apriori(
  transazioni_category_no_mele_agrumi_others,
  parameter = list(supp = selected_supp_category_tris, 
                   conf = selected_conf_category_tris)
)

summary(regole_category_tris_final)

regole_category_tris_ord <- sort(
  regole_category_tris_final, 
  by = "lift", 
  decreasing = TRUE
)
inspect(head(regole_category_tris_ord, 10))

plot(
  regole_category_tris_ord, 
  method = "graph", 
  engine = "htmlwidget",
  main = "MBA by Category (Excluding Mele, Agrumi, Formaggi Freschi, Pane, Salumi L.S., Pasta Fresca)"
)

##############################################################################
# 11 QUATRIS) MBA PER CATEGORY 
# (EXCLUDING "MELE", "AGRUMI" + 
# "FORMAGGI FRESCHI", "PANE E SOSTITUTIVI",
# "SALUMI LIBERO SERVIZIO", "PASTA FRESCA",
# "ALTRI ORTAGGI", "ALTRE VERDURE",
# "CAVOLI", "POMODORI")
##############################################################################
# Additional exclusions on top of section 11 BIS

additional_exclusions_quatris <- c(
  "FORMAGGI FRESCHI",
  "PANE E SOSTITUTIVI",
  "SALUMI LIBERO SERVIZIO",
  "PASTA FRESCA",
  "ALTRI ORTAGGI",
  "ALTRE VERDURE",
  "UOVA",
  "POMODORI",
  "LATTE"
)

data_A_no_mele_agrumi_others_quatris <- subset(
  data_A_no_mele_agrumi, 
  !(CATEGORY %in% additional_exclusions_quatris)
)

data_A_transazioni_quatris <- split(
  data_A_no_mele_agrumi_others_quatris$CATEGORY,
  paste(
    data_A_no_mele_agrumi_others_quatris$Customer_id, 
    data_A_no_mele_agrumi_others_quatris$DATE,
    sep = "_"
  )
)
transazioni_quatris <- as(data_A_transazioni_quatris, "transactions")

support_values_quatris <- c(0.002, 0.002125, 0.0025)
confidence_values_quatris <- c(0.7, 0.73, 0.725, 0.775, 0.8, 0.85)

results_category_quatris <- data.frame(Support = numeric(),
                                       Confidence = numeric(),
                                       Num_Rules = integer())

for (supp in support_values_quatris) {
  for (conf in confidence_values_quatris) {
    regole_quatris <- apriori(
      transazioni_quatris,
      parameter = list(supp = supp, conf = conf)
    )
    num_rules_quatris <- length(regole_quatris)
    results_category_quatris <- rbind(
      results_category_quatris,
      data.frame(Support = supp, Confidence = conf, Num_Rules = num_rules_quatris)
    )
  }
}

cat("\n------ CATEGORY EXCLUDING MELE, AGRUMI, FORMAGGI FRESCHI, PANE, SALUMI L.S., PASTA FRESCA, ORTAGGI/VERDURE/CAVOLI/POMODORI: SENSITIVITY ANALYSIS ------\n")
print(results_category_quatris)

selected_supp_quatris <- 0.002125
selected_conf_quatris <- 0.73

regole_category_quatris_final <- apriori(
  transazioni_quatris,
  parameter = list(supp = selected_supp_quatris, conf = selected_conf_quatris)
)

summary(regole_category_quatris_final)

regole_category_quatris_ord <- sort(
  regole_category_quatris_final, 
  by = "lift", 
  decreasing = TRUE
)
inspect(head(regole_category_quatris_ord, 10))

plot(
  regole_category_quatris_ord, 
  method = "graph", 
  engine = "htmlwidget",
  main = "MBA by Category - Excluding Mele, Agrumi, Formaggi Freschi, Pane, Salumi L.S., Pasta Fresca, Altri Ortaggi, Altre Verdure, Cavoli, Pomodori"
)

##############################################################################
# 12) MARKET BASKET ANALYSIS (MBA) USING BRAND
##############################################################################

data_A_transazioni_brand <- split(data_A$BRAND, paste(data_A$Customer_id, data_A$DATE, sep = "_"))
data_A_transazioni_brand <- lapply(data_A_transazioni_brand, function(x) x[!is.na(x)])
transazioni_brand <- as(data_A_transazioni_brand, "transactions")

support_values_brand <- c(0.05, 0.75, 0.1)
confidence_values_brand <- c(0.7, 0.8, 0.9, 0.95)

results_brand <- data.frame(Support = numeric(), Confidence = numeric(), Num_Rules = integer())

for (supp in support_values_brand) {
  for (conf in confidence_values_brand) {
    regole_brand <- apriori(transazioni_brand, parameter = list(supp = supp, conf = conf))
    num_rules <- length(regole_brand)
    results_brand <- rbind(results_brand, data.frame(Support = supp, Confidence = conf, Num_Rules = num_rules))
  }
}

print(results_brand)

selected_supp_brand <- 0.1
selected_conf_brand <- 0.7

regole_brand_finali <- apriori(transazioni_brand, parameter = list(supp = selected_supp_brand, conf = selected_conf_brand))
summary(regole_brand_finali)

regole_brand_ord <- sort(regole_brand_finali, by = "lift", decreasing = TRUE)
inspect(head(regole_brand_ord, 10))

library(arulesViz)
plot(regole_brand_ord, method = "graph", engine = "htmlwidget")

##############################################################################
# 13) MARKET BASKET ANALYSIS ON CATEGORY, SPLIT BY WEEKDAY vs. WEEKEND
##############################################################################

if (!require("lubridate")) {
  install.packages("lubridate", dependencies = TRUE)
  library(lubridate)
}

data_A$DATE <- as.Date(as.character(data_A$DATE), format = "%Y%m%d")

# Determine the day of the week and classify as weekday or weekend
data_A$day_of_week <- wday(data_A$DATE, label = TRUE, week_start = 1)
data_A$day_type <- ifelse(data_A$day_of_week %in% c("Sat", "Sun"), "weekend", "weekday")

data_A_weekday <- subset(data_A, day_type == "weekday")
data_A_weekend <- subset(data_A, day_type == "weekend")

library(arules)

data_A_weekday_list <- split(
  data_A_weekday$CATEGORY,
  paste(data_A_weekday$Customer_id, data_A_weekday$DATE, sep = "_")
)
transazioni_weekday <- as(data_A_weekday_list, "transactions")

data_A_weekend_list <- split(
  data_A_weekend$CATEGORY,
  paste(data_A_weekend$Customer_id, data_A_weekend$DATE, sep = "_")
)
transazioni_weekend <- as(data_A_weekend_list, "transactions")

support_values_weekday    <- c(0.02, 0.025, 0.05)
confidence_values_weekday <- c(0.6, 0.7, 0.8, 0.85)

results_weekday <- data.frame(Support = numeric(),
                              Confidence = numeric(),
                              Num_Rules = integer())

for (supp in support_values_weekday) {
  for (conf in confidence_values_weekday) {
    rules_test <- apriori(
      transazioni_weekday,
      parameter = list(supp = supp, conf = conf)
    )
    results_weekday <- rbind(
      results_weekday,
      data.frame(Support = supp, Confidence = conf, Num_Rules = length(rules_test))
    )
  }
}

cat("\n------ WEEKDAY (CATEGORY) SENSITIVITY ANALYSIS ------\n")
print(results_weekday)

support_values_weekend    <- c(0.02, 0.025, 0.05)
confidence_values_weekend <- c(0.6, 0.7, 0.8, 0.85)

results_weekend <- data.frame(Support = numeric(),
                              Confidence = numeric(),
                              Num_Rules = integer())

for (supp in support_values_weekend) {
  for (conf in confidence_values_weekend) {
    rules_test <- apriori(
      transazioni_weekend,
      parameter = list(supp = supp, conf = conf)
    )
    results_weekend <- rbind(
      results_weekend,
      data.frame(Support = supp, Confidence = conf, Num_Rules = length(rules_test))
    )
  }
}

cat("\n------ WEEKEND (CATEGORY) SENSITIVITY ANALYSIS ------\n")
print(results_weekend)

best_supp_weekday <- 0.02
best_conf_weekday <- 0.7

best_supp_weekend <- 0.025
best_conf_weekend <- 0.7

rules_weekday_final <- apriori(
  transazioni_weekday,
  parameter = list(supp = best_supp_weekday, conf = best_conf_weekday)
)

rules_weekend_final <- apriori(
  transazioni_weekend,
  parameter = list(supp = best_supp_weekend, conf = best_conf_weekend)
)

library(arulesViz)

cat("\n------ FINAL WEEKDAY (CATEGORY) RULES ------\n")
summary(rules_weekday_final)
inspect(head(sort(rules_weekday_final, by = "lift"), 10))
plot(rules_weekday_final, method = "graph", engine = "htmlwidget",
     main = "Weekday CATEGORY Rules")

cat("\n------ FINAL WEEKEND (CATEGORY) RULES ------\n")
summary(rules_weekend_final)
inspect(head(sort(rules_weekend_final, by = "lift"), 10))
plot(rules_weekend_final, method = "graph", engine = "htmlwidget",
     main = "Weekend CATEGORY Rules")

##############################################################################
# END OF SCRIPT
##############################################################################
