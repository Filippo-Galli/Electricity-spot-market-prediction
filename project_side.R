rm(list = ls())

######################### Library #############################

library(dplyr)
library(ggplot2)
library(tidyverse)
library (fda)
library(tidyr)
library(stats)

######################### DataFrame Building #############################

# Load the data from .csv file
df <- read.csv("csv/MGP_2023.csv")
df <- na.omit(df) # remove rows with NA's

# set proper type to column
df$Data <- as.Date(as.character(df$Data), format='%Y%m%d')
df$Ora <- factor(df$Ora, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))
df$ZonaMercato <- factor(df$ZonaMercato)
df$Tipo <- factor(df$Tipo)

# separate OFF and BID df

df.off <- df[which(df$Tipo=='OFF'),1:6]
df.bid <- df[which(df$Tipo=='BID'),1:6]

######################### ZonaMercato inspection #########################

this.df <- df.bid

sort(table(this.df$ZonaMercato), decreasing=T)

zone.full <- "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;" # full pool zone

# some miss
zone.no.cors <- "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;"
zone.no.sici.malt.cala <- c("CALA;CNOR;CSUD;NORD;SARD;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;COUP;MONT;","CNOR;CSUD;NORD;SARD;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;COUP;MONT;")

# full split in half NORD-SUD
zone.nord <- "CNOR;NORD;AUST;CORS;FRAN;SLOV;SVIZ;COUP;"
zone.sud <- "CALA;CSUD;SARD;SICI;SUD;COAC;GREC;MALT;MONT;"

count.ZonaMercato <- count(this.df %>% group_by(ZonaMercato))

zone.category <- case_when(
  count.ZonaMercato$ZonaMercato %in% c(zone.nord,zone.sud) ~ "NORD+SUD",
  count.ZonaMercato$ZonaMercato %in% zone.full ~ "FULL",
  count.ZonaMercato$ZonaMercato %in% zone.no.cors ~ "NOCORS",
  count.ZonaMercato$ZonaMercato %in% zone.no.sici.malt.cala ~ "NOSICIMALTCALA",
  TRUE ~ "OTHER"
)

ggplot(count.ZonaMercato, aes(x = ZonaMercato, y = n, fill=zone.category)) +
  geom_bar(stat="identity",color="black") +
  scale_fill_manual(name="ZonaMercato", values=c("FULL"="green", "NORD+SUD"= "forestgreen", "NOCORS" = "red", "NOSICIMALTCALA"='yellow', "OTHER"="skyblue"), labels=c('Full Pool','No CORS','Split NORD-SUD','No SICI;MALT;CALA','Other')) +
  theme(axis.text.x = element_blank(), legend.position = "top") +
  labs(title = "ZonaMercato", x = "Zones", y = "Count")

zone <- c(zone.full, zone.nord, zone.sud, zone.no.cors, zone.no.sici.malt.cala)

######################### Data loss ######################################

this.df <- df

# group by selected zone and count null values (should be zero for all)
null.prices <- this.df %>%
  filter(ZonaMercato %in% zone) %>%
  group_by(Data, Ora, ZonaMercato) %>%
  summarize(num.nulls = sum(is.na(Prezzo)))

# count for each day&hour the number of entries in which the zone is in the selected zone
zone.day.hour <- this.df %>%
  group_by(Data, Ora) %>%
  summarize(n = sum(ZonaMercato %in% zone))

# select when no specific_zone is present
missing.zone.day.hour <- zone.day.hour %>% filter (n == 0)

# count missing hour per Date
missing.hour <- missing.zone.day.hour %>% count( Data , name = "Missing Hour")

# Convert the 'Data' column to Date type
missing.hour$Data <- as.Date(missing.hour$Data)

# Extract the month from the 'Data' column
missing.hour$Month <- format(missing.hour$Data, "%m")

# Define colors for each month
month_colors <- c(
  "01" = "red",
  "02" = "blue",
  "03" = "green",
  "04" = "orange",
  "05" = "purple",
  "06" = "cyan",
  "07" = "magenta",
  "08" = "yellow",
  "09" = "gray",
  "10" = "darkgreen",
  "11" = "darkblue",
  "12" = "darkred"
)

# Plot the barplot with legend
ggplot(missing.hour, aes(x = Data, y = `Missing Hour`, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(x = " ", y = "Missing Hour", title = " Missing Hour per Day",
       fill = "Month") +
  scale_fill_manual(values = month_colors, 
                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

mean(missing.hour$`Missing Hour`)

# from now on we just one 'zone', so can get rid of column

df <- df[which(df$ZonaMercato %in% zone),-3]
df.off <- df.off[which(df.off$ZonaMercato %in% zone),-3]
df.bid <- df.bid[which(df.bid$ZonaMercato %in% zone),-3]

############################## Boxplot Prezzo:Ora ##############################

this.df <- df.off

# Calculate median values of Prezzo for each Ora
medians <- this.df %>%
  group_by(Ora) %>%
  summarise(median_Prezzo = median(Prezzo)) %>%
  mutate(color_value = rank(median_Prezzo, ties.method = "first")) # Rank based on median

# Join the median values back to the original data frame
df_boxplot <- this.df %>%
  left_join(medians, by = "Ora")

# Create the boxplot with enhanced color differentiation
ggplot(df_boxplot, aes(x = Ora, y = Prezzo, fill = color_value)) + 
  geom_boxplot() + 
  #geom_text(data = medians, aes(x = Ora, y = median_Prezzo, label = round(median_Prezzo, 2)), 
  #         size = 3, vjust = -1.5) +
  ggtitle("Boxplot of Prezzo by Ora - Year 23") + 
  labs(x = "Ora", y = "Prezzo") +
  coord_cartesian(ylim = c(0, 500)) +
  scale_fill_gradient(low = "green", high = "red", limits = c(min(medians$color_value), max(medians$color_value)),
                      breaks = c(min(medians$color_value), max(medians$color_value)),
                      labels = c(min(medians$median_Prezzo), max(medians$median_Prezzo)),
                      guide = guide_colorbar(title = "median_Prezzo")) + # Modifica il titolo della legenda# Green to red gradient
  theme_minimal()

############################## Boxplot Prezzo:Month ##############################

this.df <- df.off
this.df$Month <- as.factor(month(this.df$Data))

# Calculate median values of Prezzo for each Ora
medians <- this.df %>%
  group_by(Month) %>%
  summarise(median_Prezzo = median(Prezzo)) %>%
  mutate(color_value = rank(median_Prezzo, ties.method = "first")) # Rank based on median

# Join the median values back to the original data frame
df_boxplot <- this.df %>%
  left_join(medians, by = "Month")

# Create the boxplot with enhanced color differentiation
ggplot(df_boxplot, aes(x = Month, y = Prezzo, fill = color_value)) + 
  geom_boxplot() + 
  #geom_text(data = medians, aes(x = Ora, y = median_Prezzo, label = round(median_Prezzo, 2)), 
  #         size = 3, vjust = -1.5) +
  ggtitle("Boxplot of Prezzo by Month - Year 23") + 
  labs(x = "Month", y = "Prezzo") +
  coord_cartesian(ylim = c(0, 500)) +
  scale_fill_gradient(low = "green", high = "red", limits = c(min(medians$color_value), max(medians$color_value)),
                      breaks = c(min(medians$color_value), max(medians$color_value)),
                      labels = c(min(medians$median_Prezzo), max(medians$median_Prezzo)),
                      guide = guide_colorbar(title = "median_Prezzo")) + # Modifica il titolo della legenda# Green to red gradient
  theme_minimal()

#############################











