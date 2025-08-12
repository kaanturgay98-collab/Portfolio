colori_it <- rainbow(length(sales_comp_it$Name_Product)) # Generate distinct colors for lines

x11()
pdf("Sales_Competitors_Italy.pdf", width = 8, height = 6)
# Create the plot
matplot(
  years_it, 
  t(sales_data_it_comp), 
  type = "l", 
  lty = 1, 
  lwd = 3,  # Thicker lines for visibility
  col = colori_it, 
  xlab = "Year", 
  ylab = "Sales Value (€)", 
  main = "Sales Competitors Italy",
  xaxt = "n",  # Suppress x-axis to customize it
  yaxt = "n",  # Suppress y-axis to customize it
  bty = "n"    # Remove box around the plot
)


# Customize axes
axis(1, at = years_it, labels = years_it, col.axis = "darkgray", las = 1, cex.axis = 0.9)  # X-axis
axis(2, las = 2, col.axis = "darkgray", cex.axis = 0.9)                                   # Y-axis

# Add a grid for better readability
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

# Add a legend with a better layout
legend(
  "topright", 
  legend = sales_comp_it$Name_Product, 
  col = colori_it, 
  lty = 1, 
  lwd = 3, 
  cex = 0.8,             # Smaller legend text size
  bty = "n",             # No box around the legend
  title = "Products"     # Add a legend title
)
dev.off()

x11()
pdf("Sales_Campari_UK.pdf", width = 8, height = 6)
# United Kingdom Campari Sales
matplot(
  years_uk, 
  t(sales_data_uk_campari), 
  type = "l", 
  lty = 1, 
  lwd = 3, 
  col = colori_uk, 
  xlab = "Year", 
  ylab = "Sales Value (€)", 
  main = "United Kingdom Campari Sales",
  xaxt = "n", 
  yaxt = "n", 
  bty = "n"
)
axis(1, at = years_uk, labels = years_uk, col.axis = "darkgray", las = 1, cex.axis = 0.9)
axis(2, las = 2, col.axis = "darkgray", cex.axis = 0.9)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
dev.off()


pdf("Sales_Competitors_UK.pdf", width = 8, height = 6)
# United Kingdom Competitors Sales
matplot(
  years_uk, 
  t(sales_data_uk_comp), 
  type = "l", 
  lty = 1, 
  lwd = 3, 
  col = colori_uk, 
  xlab = "Year", 
  ylab = "Sales Value (€)", 
  main = "United Kingdom Competitors Sales",
  xaxt = "n", 
  yaxt = "n", 
  bty = "n"
)
axis(1, at = years_uk, labels = years_uk, col.axis = "darkgray", las = 1, cex.axis = 0.9)
axis(2, las = 2, col.axis = "darkgray", cex.axis = 0.9)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
legend(
  "topright", 
  legend = sales_comp_uk$Name_Product, 
  col = colori_uk, 
  lty = 1, 
  lwd = 3, 
  cex = 0.8, 
  bty = "n", 
  title = "Products"
)
dev.off()

pdf("Sales_Campari_Germany.pdf", width = 8, height = 6)
# Sales Campari Germany
matplot(
  years, 
  t(sales_data_ger_campari), 
  type = "l", 
  lty = 1, 
  lwd = 3, 
  col = colori_ger, 
  xlab = "Year", 
  ylab = "Sales Value (€)", 
  main = "Sales Campari Germany",
  xaxt = "n", 
  yaxt = "n", 
  bty = "n"
)
axis(1, at = years, labels = years, col.axis = "darkgray", las = 1, cex.axis = 0.9)
axis(2, las = 2, col.axis = "darkgray", cex.axis = 0.9)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

dev.off()

pdf("Sales_Competitors_Germany.pdf", width = 8, height = 6)
# Sales Competitors Germany
matplot(
  years, 
  t(sales_data_ger_comp), 
  type = "l", 
  lty = 1, 
  lwd = 3, 
  col = colori_ger, 
  xlab = "Year", 
  ylab = "Sales Value (€)", 
  main = "Sales Competitors Germany",
  xaxt = "n", 
  yaxt = "n", 
  bty = "n"
)
axis(1, at = years, labels = years, col.axis = "darkgray", las = 1, cex.axis = 0.9)
axis(2, las = 2, col.axis = "darkgray", cex.axis = 0.9)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
legend(
  "topright", 
  legend = sales_comp_ger$Name_Product, 
  col = colori_ger, 
  lty = 1, 
  lwd = 3, 
  cex = 0.8, 
  bty = "n", 
  title = "Products"
)
dev.off()

pdf("Sales_Campari_Italy.pdf", width = 8, height = 6)
# Sales Campari Italy
matplot(
  years_it, 
  t(sales_data_it_campari), 
  type = "l", 
  lty = 1, 
  lwd = 3, 
  col = colori_it, 
  xlab = "Year", 
  ylab = "Sales Value (€)", 
  main = "Sales Campari Italy",
  xaxt = "n", 
  yaxt = "n", 
  bty = "n"
)
axis(1, at = years_it, labels = years_it, col.axis = "darkgray", las = 1, cex.axis = 0.9)
axis(2, las = 2, col.axis = "darkgray", cex.axis = 0.9)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

dev.off()


# Improved Histogram Command
pdf("Germany_Ratios_Histogram.pdf", width = 8, height = 6)
  # Set up the plotting area

hist(
  ratios_filtered_ger,
  breaks = 50,
  col = "#4F81BD",  # Use a more modern color
  border = "white",  # White borders for a cleaner look
  main = "Germany Ratios",
  xlab = "Sales.Value.Baseline/Sales.Value",
  ylab = "Frequency",
  xlim = c(0, 1),
  cex.main = 1.5,  # Larger title text
  cex.lab = 1.2,   # Larger axis label text
  cex.axis = 1.1,  # Larger axis tick text
  las = 1          # Horizontal axis labels for better readability
)

# Add gridlines for better readability
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# Optional: Add a vertical line for the mean or a specific threshold
abline(v = mode_ratio1_ger, col = "red", lwd = 2, lty = 2)

dev.off()



















































































































































































































































































































