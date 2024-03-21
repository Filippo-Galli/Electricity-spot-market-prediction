############################ Library #############################

library(XML)
library(dplyr)
library("methods")
library(dplyr)

######################### XML to Dataframe ########################

# List all XML files in the directory
file_list <- list.files(path = "data", pattern = "\\.xml$", full.names = TRUE)

# Initialize an empty list to store dataframes
all_dataframes <- list()

# Create a progress bar
pb <- txtProgressBar(min = 0, max = length(file_list), style = 3)

# Loop through each file, read it, and convert to a dataframe
for (i in 1:length(file_list)) {
  result <- xmlToDataFrame(file_list[i])
  all_dataframes[[file_list[i]]] <- result
  # Update the progress bar
  setTxtProgressBar(pb, i)
}

# Close the progress bar
close(pb)

# Combine all dataframes into one
df <- bind_rows(all_dataframes)

# Print or access the results as needed
head(df)

# useless variables cleaning
rm(all_dataframes, file_list, i, result)

######################### Data Cleaning #############################

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

# Convert the 'Date' column from character to date
df <- df %>%
 mutate(Data = as.Date(Data, format = "%Y%m%d"))

# Convert the 'Prezzo' column from character to numeric
df <- df %>%
 mutate(Prezzo = as.numeric(Prezzo))

# Convert the 'Quantita' column from character to numeric
df <- df %>%
 mutate(Quantita = as.numeric(Quantita))

# Convert the 'Ora' column from character to numeric 
df <- df %>%
 mutate(Ora = as.numeric(Ora))

# Convert the 'PrezzoZonale' column from character to numeric

df <- df %>%
 mutate(PrezzoZonale = as.numeric(PrezzoZonale))



summary(df)
