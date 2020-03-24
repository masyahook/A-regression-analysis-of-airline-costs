library(dplyr)
library(ggplot2)
library(GGally) 
library(pheatmap) 

###### Reading the data

# Define the column names of our dataset
col.names <- c('Airline', "Length","Speed","Time",
                       "Population","Cost","Revenue","Load_Factor",
                       "Capacity","Total_Assets","Funds", "Adjusted_Assets")

# Read the raw data
raw_data <- read.fwf("/Users/MaximKryukov/Documents/GitHub/A-regression-analysis-of-airline-costs/airline_costs.dat", 
                widths=c(13, -12, 3, -5, 3, -4, 4, -3, 5, -3, 5, -4, 4, -3, 5, -3, 5, -1, 7, -2, 6, -1, 7), 
                col.names=col.names,  row.names = 1)

# We will trim the spaces in the rownames (airline names) at the edges
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
rownames(raw_data) <- trim(rownames(raw_data))

# We will reorder the raw data in a way, that "Cost" column would be in the end
raw_data <- raw_data[,c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 5)]

###### Explanatory Data Analysis

# Let us create an additional matrix, where all features would be log-normalized except for load factor
# This was done in the original article
log_data <- raw_data
for (i in 1:ncol(log_data)) {
  if (i != 6) {
    log_data[, i] = log(log_data[, i])
  }
}

# Repeat the same plots
ggpairs(log_data) + theme(text = element_text(size = 8), 
                                             axis.text = element_text(size = rel(0.45)))

cormat <- cor(log_data)
pheatmap(cormat, display_numbers = 1) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Maybe plot some more plots, just focusing on significant features?

###### Linear regression

# Create a formula for linear regression
cost.formula <- Cost ~ Length + Speed + Time + Population + 
  Load_Factor + Capacity + Funds + Adjusted_Assets

# Fit the linear regression model to the features (both in raw and log version of the dataset)
raw_lm <- lm(cost.formula, data = raw_data)
log_lm <- lm(cost.formula, data = log_data)

summary(raw_lm)  # Why in raw_lm described significant features are inded significant
summary(log_lm)  # but in log_lm these features are not so significant

### Then we assess only the log-pretransformed model

# Plotting a QQ-plot
plot(log_lm, which=2, cex=1.2)

# Plotting the residuals plot (should look random)
log_lm.resid <- residuals(log_lm)
plot(log_lm.resid)

# Plotting the Cook's distance (showing the points that are influential - skew the model)
plot(log_lm, which=4)

# Plotting the Residuals vs Fitted plot (still need to get an idea of what it means)
plot(log_lm, which=1)

### Maybe use only significant features?
### Still discuss should we use Revenue feature or not?

significant_cost.formula <- Cost ~ Time + Load_Factor + Capacity

final_lm <- lm(significant_cost.formula, data = log_data)

summary(final_lm)
# Looks good?
