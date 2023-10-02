# Exploratory-Data-Analysis-
Exploratory Data Analysis | Dataset Sample Store | The Sparks Foundation
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
data <- read.csv("SampleSuperstore.csv")

# Display the first few rows of the dataset
head(data)

# Check for missing values
sum(is.na(data))

# Remove unnecessary columns
data <- data [, c("Ship.Mode", "Segment", "Country", "Region", "City", "Category", "Sub.Category", "Sales", "Quantity", "Discount", "Profit")]

# Check for duplicate rows
data <- unique(data)

# Check summary statistics
summary(data)

# Visualize sales by category
ggplot(data, aes(x = Category, y = Sales, fill = Category)) +
  geom_bar(stat = "summary", fun = "mean") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualize profit by region
ggplot(data, aes(x = Region, y = Profit, fill = Region)) +
  geom_bar(stat = "summary", fun = "mean") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualize profit vs. discount
ggplot(data, aes(x = Discount, y = Profit)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")

# Explore correlations
correlations <- cor(data[, c("Sales", "Quantity", "Discount", "Profit")])
print(correlations)
