rm(list = ls())

######################### Library #############################

library(dplyr)
library(ggplot2)
library(tidyverse)
library (fda)
library(tidyr)
library(stats)

######################### Data Processing #############################

# Load the data from .csv file
df <- read.csv("csv/2023-01-01_to_2023-12-31.csv")

# Ordering Ora's value
df$Ora <- factor(df$Ora, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))
df$Data <- as.Date(df$Data)
df$ZonaMercato <- factor(df$ZonaMercato)

########################## All zones #################################

#for 22 23 specific_zone <- "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;"
#for 21    specific_zone <- "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;MONT;"
specific_zone <- "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;"

count_zona_mercato <- count(df %>% group_by(ZonaMercato))


ggplot(count_zona_mercato, aes(x = ZonaMercato, y = n, fill=ZonaMercato!=specific_zone)) +
  geom_bar(stat="identity",color="black") +
  scale_fill_manual(name="ZonaMercato", values=c("TRUE"="skyblue", "FALSE"="green"), labels=c(specific_zone,"Others")) +
  theme(axis.text.x = element_blank(), legend.position = "top") +
  labs(title = "ZonaMercato", x = "Zones", y = "Count")

######################## Data loss ################################

# null prices of specific_zone per day per hour
null_prices <- df %>%
  filter(ZonaMercato == specific_zone) %>%
  group_by(Data, Ora, ZonaMercato) %>%
  summarize(num_nulls = sum(is.na(Prezzo)))

# how many entries of type specific_ per day per hour
specific_zone_presence <- df %>%
  group_by(Data, Ora) %>%
  summarize(specific_zone_number = sum(ZonaMercato == specific_zone))

# select when no specific_zone is present
no_specific_zone <- specific_zone_presence %>% filter (specific_zone_number == 0)

# count missing hour per Date
missing_hours <- no_specific_zone %>% count( Data , name = "Missing Hour")

# Convert the 'Data' column to Date type
missing_hours$Data <- as.Date(missing_hours$Data)

# Extract the month from the 'Data' column
missing_hours$Month <- format(missing_hours$Data, "%m")

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
ggplot(missing_hours, aes(x = Data, y = `Missing Hour`, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(x = " ", y = "Missing Hour", title = " Missing Hour per Day",
       fill = "Month") +
  scale_fill_manual(values = month_colors, 
                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

########################## Data Loss with clustered zone ##################

# null prices per day per hour
null_prices <- df %>%
  group_by(Data, Ora) %>%
  summarize(num_nulls = sum(is.na(Prezzo)))

# count missing hour per Date
missing_hours <- null_prices %>%
  group_by(Data) %>% summarize( MissingHour = sum(num_nulls))

# Convert the 'Data' column to Date type
missing_hours$Data <- as.Date(missing_hours$Data)

# Extract the month from the 'Data' column
missing_hours$Month <- format(missing_hours$Data, "%m")

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
ggplot(missing_hours, aes(x = Data, y = `MissingHour`, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(x = " ", y = "Missing Hour", title = " Missing Hour per Day",
       fill = "Month") +
  scale_fill_manual(values = month_colors, 
                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



########################## Prezzo-Ora's Boxplot #############################

specific_zone <- "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;"

# If you want a single month selected
#df_boxplot <- df[month(df$Data) == 6,] %>% filter(ZonaMercato == specific_zone)

df_boxplot <- df %>% filter(ZonaMercato == specific_zone)

df_boxplot <- df_boxplot %>% filter(!is.na(Ora))

# Calculate median values of Prezzo for each Ora
medians <- df_boxplot %>%
  group_by(Ora) %>%
  summarise(median_Prezzo = median(Prezzo)) %>%
  mutate(color_value = rank(median_Prezzo, ties.method = "first")) # Rank based on median

# Join the median values back to the original data frame
df_boxplot <- df_boxplot %>%
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
ggplot(df_boxplot, aes(x = day, y = Prezzo)) + 
  geom_boxplot() + 
  theme_minimal() + 
  ggtitle("Boxplot of Prezzo by Giorni feriali / festivi") +
  labs(x = "Giorni feriali / festivi", y = "Prezzo") +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_text(data = dataMedian, aes(day, MD, label = MD), 
            position = position_nudge(x = -0.2), size = 5, vjust = -0.8)

# cleaning variables
rm(dataMedian, df_boxplot)

########################## Prezzo Zonale-Ora's Boxplot #############################

ggplot(df, aes(x = Ora, y = PrezzoZonale, fill = Ora)) +
  geom_boxplot() +
  labs(x = "Ora", y = "prezzoZonale") +
  ggtitle("Boxplot of prezzoZonale by Ora")
scale_fill_discrete(name = "Ora")

########################## Prezzo Zonale-Mese's Boxplot #############################

df_boxplot <- df %>%
  mutate(month = month(Data)) %>%
  mutate(month = factor(month, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))

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

########################## Curve of prezzo by hour normalized #############################

rm(list = ls())

# Parameters
desired_hours <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
day <- c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05", "2023-01-06", "2023-01-07", "2023-01-08", "2023-01-09", "2023-01-10", "2023-01-11", "2023-01-12", "2023-01-13", "2023-01-14", "2023-01-15", "2023-01-16", "2023-01-17", "2023-01-18", "2023-01-19", "2023-01-20", "2023-01-21", "2023-01-22", "2023-01-23", "2023-01-24", "2023-01-25", "2023-01-26", "2023-01-27", "2023-01-28", "2023-01-29", "2023-01-30", "2023-01-31")
hours <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24")
zona_mercato <- c("CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;")

# Load the data from .csv file
df <- read.csv("csv/2023-01-01_to_2023-12-31.csv")

# Dummy dataframe
df_curve <- df[df$Data %in% day & df$Ora %in% hours & df$ZonaMercato %in% zona_mercato, ]
# delete ZonaMercato
df_curve <- df_curve %>% subset(select = c(Data, Ora, Prezzo, Quantita, PrezzoZonale))

# remove variables useless
rm(df, day, hours, zona_mercato)

# calculate the cumulative sum of 'Quantita' for each 'Data' and 'Ora'
df_curve <- df_curve %>%
  group_by(Data, Ora) %>%
  mutate(cum_sum_quantita = cumsum(Quantita),
         cum_sum_quantita_normalized = (cum_sum_quantita - min(cum_sum_quantita)) / (max(cum_sum_quantita) - min(cum_sum_quantita)))

# Create dataframes from the splitting of df_curve by 'Ora' and 'Data'
df_split_by_hour <- split(df_curve, interaction(df_curve$Ora, df_curve$Data))

# delete empty dataframes
df_split_by_hour <- df_split_by_hour[sapply(df_split_by_hour, nrow) > 0]

# Apply the 'stepfun' function to each hour of each day to create a step function object
step_fun_list <- lapply(df_split_by_hour, function(df) {
  df <- df[order(df$cum_sum_quantita_normalized), ] # Sort by 'cum_sum_quantita_normalized'
  y <- c(df$Prezzo, tail(df$Prezzo, n = 1)) # Add the last value to the end, otherwise error message
  stepfun(df$cum_sum_quantita_normalized, y) # create the step function
})

# Function to find indexes of the desired hour in the list of dataframes
find_hour_index <- function(desired_hour, list_of_dfs) {
  # Convert desired_hour to a character for pattern matching
  desired_hour <- as.character(desired_hour)
  
  # Initialize an empty vector to store the indices
  indices <- c()
  
  # Loop through the names of the data frames
  for (i in seq_along(list_of_dfs)) {
    # Extract the hour from the name using a regular expression
    # This pattern matches the first sequence of digits before the dot
    hour <- regmatches(names(list_of_dfs)[i], regexpr("^\\d+", names(list_of_dfs)[i]))
    
    # Check if the extracted hour matches the desired hour
    if (hour == desired_hour) {
      # If a match is found, add the index to the indices vector
      indices <- c(indices, i)
    }
  }
  
  # Return the indices vector
  return(indices)
}

# Initialize an empty data frame to store the results of mean step functions
data <- data.frame()

for (desired_hour in desired_hours){
  
  # Find the index of the desired hour in the list of data frames
  hour_index <- find_hour_index(desired_hour, df_split_by_hour)
  
  # Check if the hour_index is valid
  if (length(hour_index) == 0) {
    print("Desired hour not found in the list of data frames, hour: ", desired_hour)
  }
  
  # Extract the step functions for the desired hour
  # Assuming the step functions are in the same order as the data frames in df_split_by_hour
  step_funs_desired_hour <- list()
  for (i in seq_along(hour_index)) {
    step_funs_desired_hour <- append(step_funs_desired_hour, step_fun_list[[hour_index[i]]])
  }
  
  # Step 2: Find the unique points at which to evaluate the step functions
  unique_points <- unique(unlist(lapply(df_split_by_hour, function(df) df$cum_sum_quantita_normalized)))
  unique_points <- sort(unique_points)
  
  # Step 3: Evaluate the step functions at these points
  values_desired_hour <- sapply(step_funs_desired_hour, function(step_fun) step_fun(unique_points))
  
  # Step 4: Calculate the mean of these values
  mean_values <- rowMeans(values_desired_hour)
  
  # Step 5: Save mean values in a dummy dataframe
  temp <- data.frame(unique_points, mean_values)
  temp$Ora <- desired_hour
  
  # Step 6: Append the dummy dataframe to the main dataframe
  data <- rbind(data, temp)
}

# Cleaning variables
rm(temp, unique_points, mean_values, i, desired_hour, values_desired_hour, step_funs_desired_hour, hour_index)

# Convert the 'Ora' column to a factor with the desired order
data$Ora <- factor(data$Ora, levels = desired_hours)

# Plot using ggplot2
ggplot(data, aes(x = unique_points, y = mean_values, color = Ora)) +
  geom_line() +
  labs(x = "Normalized Cumulative Quantity",
       y = "Mean Prezzo",
       title = "Mean Prezzo by Normalized Cumulative Quantity") +
  theme_minimal()




########################## Curve of prezzo by hour #############################

rm(list = ls())

# Parameters
desired_hours <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
day <- c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05", "2023-01-06", "2023-01-07", "2023-01-08", "2023-01-09", "2023-01-10", "2023-01-11", "2023-01-12", "2023-01-13", "2023-01-14", "2023-01-15", "2023-01-16", "2023-01-17", "2023-01-18", "2023-01-19", "2023-01-20", "2023-01-21", "2023-01-22", "2023-01-23", "2023-01-24", "2023-01-25", "2023-01-26", "2023-01-27", "2023-01-28", "2023-01-29", "2023-01-30", "2023-01-31")
hours <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24")
zona_mercato <- c("CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;")

# Load the data from .csv file
df <- read.csv("csv/2023-01-01_to_2023-12-31.csv")

# Dummy dataframe
df_curve <- df[df$Data %in% day & df$Ora %in% hours & df$ZonaMercato %in% zona_mercato, ]
# delete ZonaMercato
df_curve <- df_curve %>% subset(select = c(Data, Ora, Prezzo, Quantita, PrezzoZonale))
# mean prezzo zonale
mean_prezzo_zonale <- mean(df_curve$PrezzoZonale)

# remove variables useless
rm(df, day, hours, zona_mercato)

# calculate the cumulative sum of 'Quantita' for each 'Data' and 'Ora'
df_curve <- df_curve %>%
  group_by(Data, Ora) %>%
  mutate(cum_sum_quantita = cumsum(Quantita))

# Create dataframes from the splitting of df_curve by 'Ora' and 'Data'
df_split_by_hour <- split(df_curve, interaction(df_curve$Ora, df_curve$Data))

# delete empty dataframes
df_split_by_hour <- df_split_by_hour[sapply(df_split_by_hour, nrow) > 0]

# Apply the 'stepfun' function to each hour of each day to create a step function object
step_fun_list <- lapply(df_split_by_hour, function(df) {
  df <- df[order(df$cum_sum_quantita), ] # Sort by 'cum_sum_quantita_normalized'
  y <- c(df$Prezzo, tail(df$Prezzo, n = 1)) # Add the last value to the end, otherwise error message
  stepfun(df$cum_sum_quantita, y) # create the step function
})

# Function to find indexes of the desired hour in the list of dataframes
find_hour_index <- function(desired_hour, list_of_dfs) {
  # Convert desired_hour to a character for pattern matching
  desired_hour <- as.character(desired_hour)
  
  # Initialize an empty vector to store the indices
  indices <- c()
  
  # Loop through the names of the data frames
  for (i in seq_along(list_of_dfs)) {
    # Extract the hour from the name using a regular expression
    # This pattern matches the first sequence of digits before the dot
    hour <- regmatches(names(list_of_dfs)[i], regexpr("^\\d+", names(list_of_dfs)[i]))
    
    # Check if the extracted hour matches the desired hour
    if (hour == desired_hour) {
      # If a match is found, add the index to the indices vector
      indices <- c(indices, i)
    }
  }
  
  # Return the indices vector
  return(indices)
}

# Initialize an empty data frame to store the results of mean step functions
data <- data.frame()

for (desired_hour in desired_hours){
  
  # Find the index of the desired hour in the list of data frames
  hour_index <- find_hour_index(desired_hour, df_split_by_hour)
  
  # Check if the hour_index is valid
  if (length(hour_index) == 0) {
    print("Desired hour not found in the list of data frames, hour: ", desired_hour)
  }
  
  # Extract the step functions for the desired hour
  # Assuming the step functions are in the same order as the data frames in df_split_by_hour
  step_funs_desired_hour <- list()
  for (i in seq_along(hour_index)) {
    step_funs_desired_hour <- append(step_funs_desired_hour, step_fun_list[[hour_index[i]]])
  }
  
  # Step 2: Find the unique points at which to evaluate the step functions
  unique_points <- unique(unlist(lapply(df_split_by_hour, function(df) df$cum_sum_quantita)))
  unique_points <- sort(unique_points)
  
  # Step 3: Evaluate the step functions at these points
  values_desired_hour <- sapply(step_funs_desired_hour, function(step_fun) step_fun(unique_points))
  
  # Step 4: Calculate the mean of these values
  mean_values <- rowMeans(values_desired_hour)
  
  # Step 5: Save mean values in a dummy dataframe
  temp <- data.frame(unique_points, mean_values)
  temp$Ora <- desired_hour
  
  # Step 6: Append the dummy dataframe to the main dataframe
  data <- rbind(data, temp)
}

# Cleaning variables
rm(temp, unique_points, mean_values, i, desired_hour, values_desired_hour, step_funs_desired_hour, hour_index)

# Convert the 'Ora' column to a factor with the desired order
data$Ora <- factor(data$Ora, levels = desired_hours)

# Plot using ggplot2
ggplot(data, aes(x = unique_points, y = mean_values, color = Ora)) +
  geom_line() +
  labs(x = "Quantity",
       y = "Mean Prezzo",
       title = "Mean OFF curves") +
  theme_minimal() +
  xlim(75000,105000) +
  ylim(0,400) + 
  geom_abline(slope = 0 , intercept = mean_prezzo_zonale, lty=2) +
  geom_text(aes(x = 77000, y = mean_prezzo_zonale + 10, label = "mean PrezzoZonale"), 
            color = "black", size = 3)

######################### Smoothing and gcv #################
rm(list = ls())

######################### Library #############################

library(dplyr)
library(ggplot2)
library(tidyverse)
library(fda)
library(lubridate)

######################### Data Processing #############################

# Load the data from .csv file
df <- read.csv("csv/2023-01-01_to_2023-12-31.csv")

df$Ora <- factor(df$Ora, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))
df$Data <- as.Date(df$Data)
df$ZonaMercato <- factor(df$ZonaMercato)

day <- c("2023-01-01")
hours <- c("18")
zona_mercato <- c("CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;")

######################### Smoothing - Prezzo #############################
######################### Prezzo by Cumulative Quantity #############################

# subset of df for smoothing
df_subset <- df[df$Data %in% day & df$Ora %in% hours, ]

df_subset <- df_subset[order(df_subset$Prezzo), ]

df_subset$cum_sum_quantita <- cumsum(df_subset$Quantita)
df_subset$cum_sum_quantita <- as.numeric(df_subset$cum_sum_quantita)

# Set bounds
l_bound <- 20000
u_bound <- 35000

# Subset the data based on the bounds
restricted_data <- subset(df_subset, cum_sum_quantita >= l_bound & cum_sum_quantita <= u_bound)
restricted_prezzo <- restricted_data$Prezzo
restricted_abscissa <- restricted_data$cum_sum_quantita

#Create a step function
step_function <- stepfun(restricted_abscissa, c(restricted_prezzo[1], restricted_prezzo))

########################## Test 0 - isn't monotonic ##########################
# Set parameters
m <- 4          # spline order 
degree <- m-1   # spline degree 
nbasis <- 15    # number of basis functions

# Create the B-spline basis
basis <- create.bspline.basis(rangeval=c(l_bound, u_bound), nbasis=nbasis, norder=m)

# Fit the data via Least Squares
# Evaluate the basis on the restricted abscissa
basismat <- eval.basis(restricted_abscissa, basis)
est_coef <- lsfit(basismat, restricted_prezzo, intercept=FALSE)$coef
Xsp0 <- basismat %*% est_coef

# Plotting
plot(step_function, verticals=T, col = "red", main = "Prezzo by Cumulative Quantity")
abline(v=basis$params)
lines(restricted_abscissa, restricted_data$PrezzoZonale)
points(restricted_abscissa, Xsp0,type="l",col="blue",lwd=2)

########################## Test 1 - monotonic ##########################
# is default in smooth.spline and df are decided empirically 
# Fit the data via smooth.spline, cv=False implies the use of gcv
fit <- smooth.spline(restricted_data$cum_sum_quantita, restricted_data$Prezzo, cv = FALSE)

# Plotting
Xsp0 <- fitted(fit)
ggplot(restricted_data, aes(x = cum_sum_quantita, y = Xsp0)) +
  geom_line(aes(y = Xsp0), color = "red") +
  geom_line(aes(y = PrezzoZonale), color = "green", lwd = 1) +
  geom_line(aes(y = step_function(restricted_abscissa)), color = "blue", lwd = 1) +
  labs(title = "Original vs Smoothed, Monotonic Curve - Test 1", x = "Cumulative Sum Quantity", y = "Price") +
  lims(y = c(0, 400)) + # Set the y-axis limits to the range of the Prezzo values
  theme_minimal()

########################## Test 2 - monotonic choose of basis ##########################
# Set parameters
m <- 20        # spline order - max order possibile
nbasis <- 34   # number of basis functions

# Create the B-spline basis
basis <- create.bspline.basis(rangeval=c(l_bound, u_bound), nbasis=nbasis, norder=m)

# Fit data on the basis 
fit <- smooth.basis(restricted_abscissa, restricted_prezzo, basis)

# Plotting
Xsp0 <- fitted(fit)
ggplot(restricted_data, aes(x = cum_sum_quantita, y = Xsp0)) +
  geom_line(aes(y = Xsp0), color = "red") +
  geom_line(aes(y = PrezzoZonale), color = "green", lwd = 1) +
  geom_line(aes(y = step_function(restricted_abscissa)), color = "blue", lwd = 1) +
  labs(title = "Original vs Smoothed, Monotonic Curve - Test 2", x = "Cumulative Sum Quantity", y = "Price") +
  lims(y = c(0, 400)) + # Set the y-axis limits to the range of the Prezzo values
  theme_minimal()

######################### Prezzo by Cumulative Quantity Multiple curve #############################
# general settings
l_bound <- 10000
u_bound <- 35000

########################## Test 0 - isn't monotonic ##############################
# Define the spline order and degree
m <- 4          # spline order 
degree <- m-1    # spline degree 

# Define the number of basis functions and the lower and upper bounds for the data
nbasis <- 6

# Create a B-spline basis with the specified range, number of basis functions, and order
basis <- create.bspline.basis(rangeval=c(l_bound, u_bound), nbasis=nbasis, norder=m)

# Initialize an empty list to store the results
my_list <- list()

# Extract the first 30 unique dates from the 'Data' column of the data frame
v <- unique(df$Data)[1:30]

# Loop through each unique date and each hour of the day
for(d in v) {
  for(h in 1:24) {
    # Subset the data frame to include only rows where 'Data' matches the current date and 'Ora' matches the current hour
    df_subset <- df[df$Data == d & df$Ora == h]
    
    # Sort the subset by 'Prezzo' (price)
    df_subset <- df_subset[order(df_subset$Prezzo), ]
    
    # Calculate the cumulative sum of 'Quantita' (quantity) for each row in the subset
    df_subset$cum_sum_quantita <- cumsum(df_subset$Quantita)
    
    # Ensure the cumulative sum is numeric
    df_subset$cum_sum_quantita <- as.numeric(df_subset$cum_sum_quantita)

    # Set the bounds for the restricted domain
    restricted_data <- subset(df_subset, cum_sum_quantita >= l_bound & cum_sum_quantita <= u_bound, Prezzo > 0)
    restricted_prezzo <- restricted_data$Prezzo
    restricted_abscissa <- restricted_data$cum_sum_quantita
    
    # Evaluate the B-spline basis at the selected abscissa values
    basismat <- eval.basis(restricted_abscissa, basis)

    # Fit
    fit <- smooth.basis(restricted_abscissa, restricted_prezzo, basis)
    est_coef <- fit$fd$coef
    
    # Calculate the estimated 'Xsp0' values using the estimated coefficients and the B-spline basis
    Xsp0 <- basismat %*% est_coef
    
    # Create a name for the current subset based on the date and hour
    name <- paste(as.character(as.Date(d)), as.character(h))
    
    # Store the estimated 'Xsp0' values in the list under the current name
    my_list[[name]] <- Xsp0
  }
}

d <- "2023-01-01"

# Plot all the curve for a hour
plot(NA, xlim = c(l_bound, u_bound), ylim = c(0, max(df$PrezzoZonale)), xlab = "Cumulative Quantity", ylab = "Price", main = "Multiple Curves", type = "n")

for(h in 1:24) {
  df_subset <- df[df$Data == d & df$Ora == h, ]
  df_subset <- df_subset[order(df_subset$Prezzo), ]
  df_subset$cum_sum_quantita <- cumsum(df_subset$Quantita)
  df_subset$cum_sum_quantita <- as.numeric(df_subset$cum_sum_quantita)
  abscissa <- df_subset$cum_sum_quantita
  indeces <- abscissa >= l_bound & abscissa <= u_bound
  r_abscissa <- abscissa[indeces]
  r_prezzo <- df_subset$Prezzo[indeces]
  step_function <- stepfun(r_abscissa, c(r_prezzo[1], r_prezzo))
  lines(r_abscissa, step_function(r_abscissa), col = "red") # Add step function
  lines(r_abscissa, df_subset$PrezzoZonale[indeces], col = "green") # Add PrezzoZonale
  points(r_abscissa, my_list[[paste(d, as.character(h))]], type = "l", col = "blue", lwd = 2) # Add points
}

########################## Test 1 - monotonic ############################
# Initialize an empty list to store the results
my_list <- list()

# Extract the first 30 unique dates from the 'Data' column of the data frame
v <- unique(df$Data)[1:1]

# Loop through each unique date and each hour of the day
for(d in v){
  for(h in 1:24) {
    # Subset the data frame to include only rows where 'Data' matches the current date and 'Ora' matches the current hour
    df_subset <- df[df$Data == d & df$Ora == h, ]
    
    # Sort the subset by 'Prezzo' (price)
    df_subset <- df_subset[order(df_subset$Prezzo), ]
    
    # Calculate the cumulative sum of 'Quantita' (quantity) for each row in the subset
    df_subset$cum_sum_quantita <- cumsum(df_subset$Quantita)
    
    # Ensure the cumulative sum is numeric
    df_subset$cum_sum_quantita <- as.numeric(df_subset$cum_sum_quantita)

    # Set the bounds for the restricted domain
    restricted_data <- subset(df_subset, cum_sum_quantita >= l_bound & cum_sum_quantita <= u_bound, Prezzo > 0)
    restricted_prezzo <- restricted_data$Prezzo
    restricted_abscissa <- restricted_data$cum_sum_quantita

    if(length(restricted_data$Data) != 0) {
      # Fit our smooth spline
      fit <- smooth.spline(restricted_data$cum_sum_quantita, restricted_data$Prezzo)

      # Evaluate the basis on the restricted abscissa
      Xsp0 <- fitted(fit)

      # Create a name for the current subset based on the date and hour
      name <- paste(as.character(as.Date(d)), as.character(h))
      
      # Store the estimated 'Xsp0' values in the list under the current name
      my_list[[name]] <- Xsp0
    }
  }
}

########################## Test 2 - monotonic with chosen basis - Very bad ##########################
# Set parameters
m <- 15        # spline order - max order possibile
nbasis <- min(m, length(unique(df$Data)) * 2) # Adjust based on your data   # number of basis functions

# Initialize an empty list to store the results
my_list <- list()

# Extract the first 30 unique dates from the 'Data' column of the data frame
v <- unique(df$Data)[1:1]

# Loop through each unique date and each hour of the day
for(d in v){
  for(h in 1:24) {
    # Subset the data frame to include only rows where 'Data' matches the current date and 'Ora' matches the current hour
    df_subset <- df[df$Data == d & df$Ora == h, ]
    
    # Sort the subset by 'Prezzo' (price)
    df_subset <- df_subset[order(df_subset$Prezzo), ]
    
    # Calculate the cumulative sum of 'Quantita' (quantity) for each row in the subset
    df_subset$cum_sum_quantita <- cumsum(df_subset$Quantita)
    
    # Ensure the cumulative sum is numeric
    df_subset$cum_sum_quantita <- as.numeric(df_subset$cum_sum_quantita)

    # Set the bounds for the restricted domain
    restricted_data <- subset(df_subset, cum_sum_quantita >= l_bound & cum_sum_quantita <= u_bound, Prezzo > 0)
    restricted_prezzo <- restricted_data$Prezzo
    restricted_abscissa <- restricted_data$cum_sum_quantita

    if(length(restricted_data$Data) != 0) {
      # Create the B-spline basis
      basis <- create.bspline.basis(rangeval=c(l_bound, u_bound), nbasis=nbasis, norder=m)

      # Evaluate the basis on the restricted abscissa
      fit <- smooth.basis(restricted_abscissa, restricted_prezzo, basis)

      # Calculate the estimated 'Xsp0' values using the estimated coefficients and the B-spline basis
      Xsp0 <- fitted(fit)

      # Create a name for the current subset based on the date and hour
      name <- paste(as.character(as.Date(d)), as.character(h))
      
      # Store the estimated 'Xsp0' values in the list under the current name
      my_list[[name]] <- Xsp0
    }
  }
}

d <- "2023-01-01"

# plot all the curve for a hour
plot(NA, xlim = c(l_bound, u_bound), ylim = c(0, 400), xlab = "Cumulative Quantity", ylab = "Price", main = "Multiple Curves")
for(h in 1:2) {
  # Subset the data frame to include only rows where 'Data' matches the current date and 'Ora' matches the current hour
  df_subset <- df[df$Data == d & df$Ora == h, ]
  
  # Sort the subset by 'Prezzo' (price)
  df_subset <- df_subset[order(df_subset$Prezzo), ]
  
  # Calculate the cumulative sum of 'Quantita' (quantity) for each row in the subset
  df_subset$cum_sum_quantita <- cumsum(df_subset$Quantita)
  
  # Ensure the cumulative sum is numeric
  df_subset$cum_sum_quantita <- as.numeric(df_subset$cum_sum_quantita)

  # Set the bounds for the restricted domain
  restricted_data <- subset(df_subset, cum_sum_quantita >= l_bound & cum_sum_quantita <= u_bound, Prezzo > 0)
  restricted_prezzo <- restricted_data$Prezzo
  restricted_abscissa <- restricted_data$cum_sum_quantita

  # Step function
  step_function <- stepfun(restricted_abscissa, c(restricted_prezzo[1], restricted_prezzo))

  lines(r_abscissa, step_function(r_abscissa), col = "red") # Add step function
  lines(restricted_abscissa, restricted_data$PrezzoZonale, col = "green") # Add PrezzoZonale
  points(restricted_abscissa, my_list[[paste(d, as.character(h))]], type = "l", col = "blue", lwd = 2) # Add points
}

######################### Cross-Validation #############################
########################## Test 0 - isn't monotonic ##############################
# generalized cross-validation
d <- c("2023-01-02")
h <- c("2")
df_subset <- df[df$Data == d & df$Ora == h, ]
df_subset <- df_subset[order(df_subset$Prezzo), ]

df_subset$cum_sum_quantita <- cumsum(df_subset$Quantita)
df_subset$cum_sum_quantita <- as.numeric(df_subset$cum_sum_quantita)
abscissa <- df_subset$cum_sum_quantita
indeces <- abscissa >= l_bound & abscissa <= u_bound
r_abscissa <- abscissa[indeces]
r_prezzo <- df_subset$Prezzo[indeces]
m <- 4
nbasis <- 5:12
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(c(l_bound, u_bound), nbasis[i], m)
  gcv[i] <- smooth.basis(r_abscissa, r_prezzo, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)
#chol fact error if nbasis is too high

# Pippo -- If we use smooth.spline we don't need to do this

# ALE - - - - generalized cross-validation

# Idea (just my empirical idea, to be asked to GUILLAUME)
# To determine the number of basis, since we must have the same
# for every day (it is not clear to me if we have this restriction
# also for every hour, since theoretically what hour we would know
# to predict), do gcv on every date and look at the num_basis
# that would be chosen the higher number of times
# For computational reasons, I'll just extract randomly the dates form the range

# Q. Can we choose a different num of basis for each hour? (IMO YES)
# As much as I know, the bid takes place the day before for every hour of the day
# after. So we already know the hour we're looking to predict.

# Other criticality. It is not so clear to me how to select in a NON empirical way
# the restricted domain bounds

l_bound <- 1000
u_bound <- 40000

d <- c("2023-01-02")

start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-02-01")
total_days <- as.numeric(difftime(end_date, start_date, units = "days"))
dates_vector <- seq(start_date, end_date, by = "day")
num_dates_extract_to_gcv <- 5
random_indices <- sample(seq_len(total_days), num_dates_extract_to_gcv)
extracted_dates <- dates_vector[random_indices]
extracted_dates
df

min_num_basis_current_moment <- numeric(length(num_dates_extract_to_gcv))

basisOrder <- 4
basis_min <- 4
basis_max <- 12  # More than this we have matrix singularity
numbasis <- seq(basis_min, basis_max, 1)
numbasis

# All this should be done for every hour

max_frequency_basis_perhour <-numeric(24)
bestBasis_perhour <-numeric(24)
result_perhour <-numeric(24)

for (hour in 1:24) {
  
  for (j in 1:num_dates_extract_to_gcv) {
      
      extracted_na <- TRUE
      
      while (extracted_na) {
        df_subset <- df[df$Data == extracted_dates[i] & df$Ora == hour, ]
        df_subset <- df_subset[order(df_subset$Prezzo), ]
        
        df_subset$cum_sum_quantita <- cumsum(df_subset$Quantita)
        df_subset$cum_sum_quantita <- as.numeric(df_subset$cum_sum_quantita)
        abscissa <- df_subset$cum_sum_quantita
        
        
        indeces <- abscissa >= l_bound & abscissa <= u_bound
        r_abscissa <- abscissa[indeces]
        r_prezzo <- df_subset$Prezzo[indeces]
        
        
        if (sum(is.na(r_abscissa)==0)) {
            extracted_na <-FALSE }
        else {
          random_indices <- sample(seq_len(total_days), 1)
          extracted_dates[i] <- dates_vector[random_indices]
        }
      }
      
      GeneralizedCrossValidations <- numeric(length(numbasis))
      for (i in 1:length(numbasis)){
        basis <- create.bspline.basis(rangeval=c(l_bound, u_bound), nbasis=numbasis[i], norder=basisOrder)
        GeneralizedCrossValidations[i] <- smooth.basis(r_abscissa, r_prezzo, basis)$gcv
      } 
      min_num_basis_current_moment[j]=numbasis[[which.min(GeneralizedCrossValidations)]]
  
  }
   
 frequency_table <- table(min_num_basis_current_moment)
 print(frequency_table)
 
 # Best numbasis with the highest frequency
 max_frequency_basis_perhour[hour] <- as.numeric(names(frequency_table)[which.max(frequency_table)])
 bestBasis_perhour[hour] <- create.bspline.basis(rangeval=c(l_bound, u_bound), nbasis=max_frequency_element, norder=basisOrder)
 result_perhour[hour] <- smooth.basis(r_abscissa, r_prezzo, bestBasis)
 
}

# We can observe that the best number of basis is generally 11 for the first hours 
# of the day, while it is 12 for the latest hours of the day.

# --> Take always 11/12 basis or differentiate wrt to the hour?
# I have implemented both approaches. To be chosen what we decide.

max_frequency_basis_perhour
best_num_basis <- names(table(max_frequency_basis_perhour))[which.max(table(max_frequency_basis_perhour))]

bestBasis <- create.bspline.basis(rangeval=c(l_bound, u_bound), nbasis=best_num_basis, norder=basisOrder)
    

# Ci sono spesso problemi con sto chol :( 

x11()
plot.new()
for(d in v) {
  for(h in 1:24) {
    df_subset <- df[df$Data == d & df$Ora == h, ]
    df_subset <- df_subset[order(df_subset$Prezzo), ]
    df_subset$cum_sum_quantita <- cumsum(df_subset$Quantita)
    df_subset$cum_sum_quantita <- as.numeric(df_subset$cum_sum_quantita)
    abscissa <- df_subset$cum_sum_quantita
    indeces <- abscissa >= l_bound & abscissa <= u_bound
    r_abscissa <- abscissa[indeces]
    r_prezzo <- df_subset$Prezzo[indeces]
    step_function <- stepfun(r_abscissa, c(r_prezzo[1], r_prezzo))
    lines(r_abscissa, step_function(r_abscissa), col = "red") 
    lines(r_abscissa, df_subset$PrezzoZonale[indeces], col = "green") 
    result <- smooth.basis(r_abscissa, r_prezzo, bestBasis)
    Y_hat <- eval.fd(r_abscissa, result$fd, Lfd=0)
    points(r_abscissa, Y_hat, type = "l", col = "blue", lwd = 2)
  }
}


