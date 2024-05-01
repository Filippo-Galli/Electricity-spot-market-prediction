library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)

# Load the data from .csv file

df <- read.csv("Dutch TTF Natural Gas Futures Historical Data.csv", stringsAsFactors = FALSE)
df_2019 <- read.csv("csv/2019-01-01_to_2019-12-31.csv")
df_2020 <- read.csv("csv/2020-01-01_to_2020-12-31.csv")
df_2021 <- read.csv("csv/2021-01-01_to_2021-12-31.csv")
df_2022 <- read.csv("csv/2022-01-01_to_2022-12-31.csv")
df_2023 <- read.csv("csv/2023-01-01_to_2023-12-31.csv")

# Controllo della struttura dei dati
str(df)

# Converti la prima colonna (presunta data) in formato ISO 8601
df$Date <- as.Date(df$Date, format = "%m/%d/%Y") 

# Crea il grafico con le date sull'asse x e la seconda colonna sull'asse y
ggplot(df, aes(x = Date, y = Price)) +
  geom_line() +
  labs(x = "Date", y = "Price")


# Calcola la media del prezzo per ogni data
df_2019_media <- df_2019 %>%
  group_by(Data) %>%
  summarize(MediaPrezzo = mean(Prezzo))

# Converti il tipo di dato della colonna 'Date' in Date
df_2019_media$Data <- as.Date(df_2019_media$Data)

# Crea il grafico del prezzo in funzione delle date
ggplot(df_2019_media, aes(x = Data, y = MediaPrezzo)) +
  geom_line() +
  labs(x = "Data", y = "Prezzo medio", title = "Prezzo medio in funzione delle date")


# Calcola la media del prezzo per ogni data
df_2020_media <- df_2020 %>%
  group_by(Data) %>%
  summarize(MediaPrezzo = mean(Prezzo))

# Converti il tipo di dato della colonna 'Date' in Date
df_2020_media$Data <- as.Date(df_2020_media$Data)

# Crea il grafico del prezzo in funzione delle date
ggplot(df_2020_media, aes(x = Data, y = MediaPrezzo)) +
  geom_line() +
  labs(x = "Data", y = "Prezzo medio", title = "Prezzo medio in funzione delle date")


# Calcola la media del prezzo per ogni data
df_2021_media <- df_2021 %>%
  group_by(Data) %>%
  summarize(MediaPrezzo = mean(Prezzo))

# Converti il tipo di dato della colonna 'Date' in Date
df_2021_media$Data <- as.Date(df_2021_media$Data)

# Crea il grafico del prezzo in funzione delle date
ggplot(df_2021_media, aes(x = Data, y = MediaPrezzo)) +
  geom_line() +
  labs(x = "Data", y = "Prezzo medio", title = "Prezzo medio in funzione delle date")


# Calcola la media del prezzo per ogni data
df_2022_media <- df_2022 %>%
  group_by(Data) %>%
  summarize(MediaPrezzo = mean(Prezzo))

# Converti il tipo di dato della colonna 'Date' in Date
df_2022_media$Data <- as.Date(df_2022_media$Data)

# Crea il grafico del prezzo in funzione delle date
ggplot(df_2022_media, aes(x = Data, y = MediaPrezzo)) +
  geom_line() +
  labs(x = "Data", y = "Prezzo medio", title = "Prezzo medio in funzione delle date")


# Calcola la media del prezzo per ogni data
df_2023_media <- df_2023 %>%
  group_by(Data) %>%
  summarize(MediaPrezzo = mean(Prezzo))

# Converti il tipo di dato della colonna 'Date' in Date
df_2023_media$Data <- as.Date(df_2023_media$Data)

# Crea il grafico del prezzo in funzione delle date
ggplot(df_2023_media, aes(x = Data, y = MediaPrezzo)) +
  geom_line() +
  labs(x = "Data", y = "Prezzo medio", title = "Prezzo medio in funzione delle date")



# Crea il grafico con le date sull'asse x e la seconda colonna sull'asse y
ggplot() +
  geom_line(data = df, aes(x = Date, y = Price, color = "Gas Price")) +
  geom_line(data = df_2019_media, aes(x = Data, y = MediaPrezzo, color = "Electricity Price")) +
  geom_line(data = df_2020_media, aes(x = Data, y = MediaPrezzo, color = "Electricity Price")) +
  geom_line(data = df_2021_media, aes(x = Data, y = MediaPrezzo, color = "Electricity Price")) +
  geom_line(data = df_2022_media, aes(x = Data, y = MediaPrezzo, color = "Electricity Price")) +
  geom_line(data = df_2023_media, aes(x = Data, y = MediaPrezzo, color = "Electricity Price")) +
  labs(x = "Date", y = "Price", title = "Price Over Time") +
  scale_color_manual(values = c("Gas Price" = "blue", "Electricity Price" = "red"),
                     labels = c("Gas Price", "Electricity Price")) +
  theme(legend.position = "bottom")
  
  