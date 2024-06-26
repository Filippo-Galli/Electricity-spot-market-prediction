rm(list = ls())

# Choose if you want all files in data
all_files <- FALSE
# Set Date range to convert
initial_date <- as.Date("2023-01-01")
final_date <- as.Date("2023-12-31")

############################ Library #############################
 
library(parallel)
library(iterators)
library(foreach)
library(doParallel)

library(XML)
library(dplyr)

######################### XML to Dataframe ########################

print("Reading XML files...")

# Set the number of cores to use
numCores <- detectCores() - 1 # Leave one core free
registerDoParallel(cores=numCores)

# Initialize list
file_path <- list()
file_list <- c()

if(all_files){
  # List ALL XML files in the directory
  file_list <- list.files(path = "data", pattern = "\\.xml$", full.names = TRUE)
} else {
  # Setting variables of file_name
  market <- 'MGP'
  dataset <- 'DomandaOfferta'
  date_range <- seq(initial_date, final_date, by = "day")
  
  # Select ONLY files in data_range
  for (i in seq_along(date_range)) {
    date_str <- format(date_range[i],"%Y%m%d")
    file_path <- file.path("data", paste0(date_str, market, dataset, ".xml"))
    
    if (file.exists(file_path)) {
      file_list <- c(file_list,file_path)
    } else {
      cat(paste(file_path, "not found\n"))
    }
  }
}


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

# useless R variables cleaning
rm(all_dataframes, file_list, dataframes_list, named_dataframes, res, numCores)

######################### Data Cleaning #############################

print("Data Cleaning...")

# Clean all the rows without all zones
table(df$ZonaMercato)
correct_zone <- c('CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;')
df <- df[!df$ZonaMercato != correct_zone, ]
#multizones
correct_zones <- c('CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;',
                   'CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;COUP;MONT;',
                   'CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;',
                   'CALA;CNOR;CSUD;NORD;SARD;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;COUP;MONT;')
df_multizone <- df[df$ZonaMercato %in% correct_zones,]
table(df$ZonaMercato)

#split by Tipo
df_multizone_unified_OFF <- subset(df_multizone[df_multizone$Tipo == 'OFF',], select = -c(Tipo, ZonaMercato, Mercato, element))
df_multizone_unified_BID <- subset(df_multizone[df_multizone$Tipo == 'BID',], select = -c(Tipo, ZonaMercato, Mercato, element))

# Clean all the demand rows 
  #table(df$Tipo)
  #correct_tipo <- c('OFF')
  #df <- df[!df$Tipo != correct_tipo, ]
  #table(df$Tipo)

dftot <- df

# dropping column with only one value
df <- subset(df, select = -c(Tipo, ZonaMercato, Mercato, element))
df <- subset(df, select = -c(Mercato, element))

rm(df_multizone)

######################### Data Pre-processing #############################

print("Data Prepocessing...")

#select df to process
df_tot <- df
df <- df_multizone_unified_BID

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

######################### current DF to .CSV #############################
# naming with selected data range and select csv folder to save it into
Tipo <- 'BID' #or BID
file_name_csv <- paste0("csv/",date_range[1],"_to_",date_range[length(date_range)],Tipo,".csv")
write.csv(df, file_name_csv , row.names = FALSE)
write.csv(df, "MGP_2023.csv" , row.names = FALSE)
