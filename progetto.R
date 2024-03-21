rm(list = ls())

############################ Library #############################

library(parallel)
library(iterators)
library(foreach)
library(doParallel)

library(XML)
library(dplyr)
library("methods")
library(dplyr)

library(ggplot2)

######################### XML to Dataframe ########################

print("Reading XML files...")

# Set the number of cores to use
numCores <- detectCores() - 1 # Leave one core free
registerDoParallel(cores=numCores)

# List all XML files in the directory
file_list <- list.files(path = "data", pattern = "\\.xml$", full.names = TRUE)

# Initialize an empty list to store dataframes
all_dataframes <- list()

# Parallel loop to read XML files and convert to dataframes
all_dataframes <- foreach(i = 1:length(file_list), .packages = "XML") %dopar% {
  result <- xmlToDataFrame(file_list[i])
  return(list(name = file_list[i], data = result))
}

# Manually combine the results into a named list
named_dataframes <- list()
for (res in all_dataframes) {
  named_dataframes[[res$name]] <- res$data
}

# Stop the parallel backend
stopImplicitCluster()

# Extract the 'data' component from each element
dataframes_list <- lapply(all_dataframes, function(x) x$data)

# Combine all dataframes into one using bind_rows
df <- bind_rows(dataframes_list)

# Print or access the results as needed
head(df)

# useless variables cleaning
rm(all_dataframes, file_list, dataframes_list, named_dataframes, res, numCores)

######################### Data Cleaning #############################

print("Data Cleaning...")

# Clean all the rows without all zones
table(df$ZonaMercato)
correct_zone <- c('CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;')
df <- df[!df$ZonaMercato != correct_zone, ]
table(df$ZonaMercato)

# Clean all the demand rows
table(df$Tipo)
correct_tipo <- c('OFF')
df <- df[!df$Tipo != correct_tipo, ]
table(df$Tipo)

# dropping column with only one value
df <- subset(df, select = -c(Tipo, ZonaMercato, Mercato, element))

######################### Data Pre-processing #############################

print("Data Prepocessing...")

# Convert the 'Date' column from character to date
df <- df %>%
 mutate(Data = as.Date(Data, format = "%Y%m%d"))

# Convert the 'Prezzo' column from character to numeric
df <- df %>%
 mutate(Prezzo = as.numeric(Prezzo))

# Convert the 'Quantita' column from character to numeric
df <- df %>%
 mutate(Quantita = as.numeric(Quantita))

# Convert the 'Ora' column from character to factor 
df <- df %>%
 mutate(Ora = as.factor(Ora))

# Convert the 'PrezzoZonale' column from character to numeric

df <- df %>%
 mutate(PrezzoZonale = as.numeric(PrezzoZonale))

summary(df)

# Clean all NA's rows (garbage of xml -> dataframe transformation)
df <- na.omit(df) 

######################### Data Processing #############################

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
