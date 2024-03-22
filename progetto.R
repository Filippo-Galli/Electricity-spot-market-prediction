rm(list = ls())

######################### Library #############################

library(dplyr)
library(ggplot2)

######################### Data Processing #############################

# Load the data from .csv file
df <- read.csv("csv/2024-03-01_to_2024-03-02.csv")

########################## Prezzo-Ora's Boxplot #############################

# Ordering Ora's value
df$Ora <- factor(df$Ora, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))

# Steps to create the gradient boxplot

# Calculate median values of Prezzo for each Ora
medians <- df %>%
  group_by(Ora) %>%
  summarise(median_Prezzo = median(Prezzo)) %>%
  mutate(color_value = rank(median_Prezzo, ties.method = "first")) # Rank based on median

# Normalize the color_value to range from 0 to 1
medians$color_value <- (medians$color_value - min(medians$color_value)) / 
                       (max(medians$color_value) - min(medians$color_value))

# Join the median values back to the original data frame
df_boxplot <- df %>%
  left_join(medians, by = "Ora")

# Create the boxplot with enhanced color differentiation
ggplot(df_boxplot, aes(x = Ora, y = Prezzo, fill = color_value)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of Prezzo by Ora") + 
  labs(x = "Ora", y = "Prezzo") +
  ylim(0, 250) +
  scale_fill_gradient(low = "green", high = "red", limits = c(min(medians$color_value), max(medians$color_value))) + # Green to red gradient
  theme_minimal()
