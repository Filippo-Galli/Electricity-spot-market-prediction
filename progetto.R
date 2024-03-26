rm(list = ls())

######################### Library #############################

library(dplyr)
library(ggplot2)
library(tidyverse)

######################### Data Processing #############################

# Load the data from .csv file
df <- read.csv("csv/2023-01-01_to_2023-12-31.csv")

###ALE

########################## Prezzo-Ora's Boxplot #############################

# Ordering Ora's value
df$Ora <- factor(df$Ora, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))

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

# If you want a single month selected
df_boxplot <- df_boxplot[month(df$Data) == 7,]

# Create the boxplot with enhanced color differentiation
ggplot(df_boxplot, aes(x = Ora, y = Prezzo, fill = color_value)) + 
  geom_boxplot() + 
  geom_text(data = medians, aes(x = Ora, y = median_Prezzo, label = round(median_Prezzo, 2)), 
            size = 3, vjust = -1.5) +
  ggtitle("Boxplot of Prezzo by Ora") + 
  labs(x = "Ora", y = "Prezzo") +
  ylim(0, 250) +
  scale_fill_gradient(low = "green", high = "red", limits = c(min(medians$color_value), max(medians$color_value))) + # Green to red gradient
  theme_minimal()

# Cleaning variables
rm(medians, df_boxplot)

########################## Prezzo-Giorni feriali / festivi's Boxplot #############################

# Create a new data frame for boxplot
df_boxplot <- df

# Add a new column to indicate whether the day is a weekday or weekend
df_boxplot$day <- weekdays(as.Date(df_boxplot$Data))

df_boxplot$day <- ifelse(df_boxplot$day %in% c("Saturday", "Sunday"), "weekend", "weekday") 

df_boxplot$day <- factor(df_boxplot$day, levels = c("weekday", "weekend"))

# Calculate median values of Prezzo for each day [weekday, weekend]
dataMedian <- summarise(group_by(df_boxplot, day), MD = median(Prezzo))


# creating a boxplot
ggplot(df_boxplot,aes(x = day, y = Prezzo)) + 
  geom_boxplot() + 
  theme_minimal() + 
  ggtitle("Boxplot of Prezzo by Giorni feriali / festivi") +
  labs(x = "Giorni feriali / festivi", y = "Prezzo") +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_text(data = dataMedian, aes(day, MD, label = MD), 
              position = position_nudge(x = -0.2), size = 5, vjust = -0.8)

# cleaning variables
rm(dataMedian, df_boxplot, medians)

########################## Prezzo Zonale-Ora's Boxplot #############################

ggplot(df, aes(x = Ora, y = PrezzoZonale, fill = Ora)) +
  geom_boxplot() +
  labs(x = "Ora", y = "prezzoZonale") +
  ggtitle("Boxplot of prezzoZonale by Ora")
scale_fill_discrete(name = "Ora")

########################## Prezzo Zonale-Mese's Boxplot #############################

df_boxplot <- df %>%
  mutate(month = month(Data))

ggplot(df_boxplot, aes(x = month, y = PrezzoZonale, fill = month)) +
  geom_boxplot() +
  labs(x = "month", y = "prezzoZonale") +
  ggtitle("Boxplot of prezzoZonale by month")
  scale_fill_discrete(name = "month")

# Cleaning variables
rm(df_boxplot)

########################## Prezzo Zonale-Ora's QQplot #############################

# Create a grid of QQ plots for each hour of the day from 1 to 12
par(mfrow=c(3,4))
for(i in 1:12) {
  qqnorm(df$PrezzoZonale[which(df$Ora==i)])
  qqline(df$PrezzoZonale[which(df$Ora==i)], col = 2, lwd=3)
}

# Create a grid of QQ plots for each hour of the day from 13 to 24
par(mfrow=c(3,4))
for(i in 13:24) {
  qqnorm(df$PrezzoZonale[which(df$Ora==i)])
  qqline(df$PrezzoZonale[which(df$Ora==i)], col = 2)
}

# Cleaning Variables
rm(i)

########################## PrezzoZonale dataframe #############################

df_prezzoZonale <- df
df_prezzoZonale <- subset(df_prezzoZonale, select = -c(Quantita, Prezzo))

# Keep only unique rows based on ID and Group
df_prezzoZonale <- df_prezzoZonale[!duplicated(df_prezzoZonale[, c("Data", "Ora")]), ]

# Extract year, month, day, and hour from the 'Data' column
df_prezzoZonale <- df_prezzoZonale %>%
  mutate(year = year(Data))
df_prezzoZonale <- df_prezzoZonale %>%
  mutate(month = month(Data))
df_prezzoZonale <- df_prezzoZonale %>%
  mutate(day = day(Data))

# Convert columns from character to correct data types
df_prezzoZonale$month <- factor(df_prezzoZonale$month)
df_prezzoZonale$Ora <- factor(df_prezzoZonale$Ora, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))
df_prezzoZonale$day <- factor(df_prezzoZonale$day, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))
df_prezzoZonale$year <- factor(df_prezzoZonale$year)
df_prezzoZonale$PrezzoZonale <- as.numeric(df_prezzoZonale$PrezzoZonale)

#simple analysis of the dataframe containing PrezzoZonale without replicas
tot_mean = mean(df_prezzoZonale$PrezzoZonale)
tot_sigma = sd(df_prezzoZonale$PrezzoZonale)

means_vector_hour <- aggregate(PrezzoZonale ~ Ora, data = df_prezzoZonale, FUN = mean)
means_vector_month <- aggregate(PrezzoZonale ~ month, data = df_prezzoZonale, FUN = mean)
sd_hour <- aggregate(PrezzoZonale ~ Ora, data = df_prezzoZonale, FUN = sd)
sd_month <- aggregate(PrezzoZonale ~ month, data = df_prezzoZonale, FUN = sd)

par(mfrow=c(1,2))
plot(levels(df_prezzoZonale$Ora), sd_hour[,2], xlab = "Ora", ylab = "Standard Deviation")
plot(levels(df_prezzoZonale$month), sd_month[,2], xlab ="Month", ylab = "Standard Deviation")

# Create boxplots for each month
ggplot(df_prezzoZonale, aes(x = month, y = PrezzoZonale, fill = month)) +
  geom_boxplot() +
  labs(x = "month", y = "prezzoZonale") +
  ggtitle("Boxplot of prezzoZonale by month") +
  scale_fill_discrete(name = "month")

# Create boxplots for each hour
ggplot(df_prezzoZonale, aes(x = Ora, y = PrezzoZonale, fill = Ora)) +
  geom_boxplot() +
  labs(x = "hour", y = "prezzoZonale") +
  ggtitle("Boxplot of prezzoZonale by hour") +
  scale_fill_discrete(name = "hour")

# Cleaning variables
rm(means_vector_hour, means_vector_month, sd_hour, sd_month, df_prezzoZonale, tot_mean, tot_sigma)

########################## Matplot months #############################

par(mfrow=c(1,1))

  