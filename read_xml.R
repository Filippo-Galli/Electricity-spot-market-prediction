library(XML)
library(dplyr)
library("methods")

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
summary(df)
table(df$ZonaMercato)

# Clean all the rows without all zones
df <- df[!df$ZonaMercato != 'CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;',]

table(df$ZonaMercato)
table(df$Tipo)
