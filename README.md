# Exploratory-Data-Analysis-
Exploratory Data Analysis | Dataset Sample Store | The Sparks Foundation
# Install and load necessary packages
install.packages("readr")
install.packages("dplyr")
install.packages("flexdashboard")
install.packages("tidyverse")
install.packages("highcharter")
install.packages("gt")
install.packages("htmltools")
installed.packages("viridisLite")

library(tidyverse)
library(flexdashboard)
library(highcharter)
library(gt)
library(htmltools)
library(viridisLite)
library(readr)
library(dplyr)
data <- read_csv("globalterrorismdb_0718dist.csv")

str(data)

summary(data)

colSums(is.na(data))

terrorism_data <- data[, c("iyear", "imonth", "iday", "country_txt", "region_txt", 
                                     "latitude", "longitude", "attacktype1_txt", 
                                     "targtype1_txt", "gname", "nkill", "nwound")]

# Handling missing values
terrorism_data[is.na(terrorism_data)] <- 0
hot_zones <- terrorism_data %>%
  group_by(country_txt) %>%
  summarize(total_attacks = n()) %>%
  arrange(desc(total_attacks))

# Print hot zones
print(hot_zones)
library(ggplot2)

ggplot(hot_zones, aes(x = reorder(country_txt, total_attacks), y = total_attacks)) +
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Total Attacks") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Visualize attack counts by region
library(ggplot2)
ggplot(terrorism_data, aes(x = region_txt)) +
  geom_bar(fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

# Visualize attack types
ggplot(terrorism_data, aes(x = attacktype1_txt)) +
  geom_bar(fill = "red") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

# Explore correlations
cor(terrorism_data[, c("nkill", "nwound")])
print(cor)

ggplot(hot_zones, aes(x = reorder(country_txt, total_attacks), y = total_attacks)) +
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("Total Attacks") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
